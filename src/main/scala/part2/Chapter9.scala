package part2


import java.util.regex.Pattern
import scala.language.implicitConversions
import scala.util.matching.Regex

object Chapter9 {

  case class ParseError(stack: List[(Location, String)] = List()) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latest: Option[(Location, String)] =
      stack.lastOption

    def latestLoc: Option[Location] =
      latest map (_._1)

    /**
     * Display collapsed error stack - any adjacent stack elements with the
     * same location are combined on one line. For the bottommost error, we
     * display the full line, with a caret pointing to the column of the error.
     * Example:
     * 1.1 file 'companies.json'; array
     * 5.1 object
     * 5.2 key-value
     * 5.10 ':'
     * { "MSFT" ; 24,
     */
    override def toString: String =
      if (stack.isEmpty) "no error message"
      else {
        val collapsed = collapseStack(stack)
        val context =
          collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
            collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
        collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
          context
      }

    def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
      s.groupBy(_._1)
        .view
        .mapValues(_.map(_._2).mkString("; "))
        .toList
        .sortBy(_._1.offset)

    def formatLoc(l: Location): String = l.line + "." + l.col
  }

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int): String = loc.input.substring(loc.offset, loc.offset + n)
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int): Location = copy(offset = offset + n)

    def currentLine: String =
      if (input.length > 1) input.linesIterator.drop(line - 1).next
      else ""

    def columnCaret: String = (" " * (col - 1)) + "^"
  }

  trait Parsers[Parser[+_]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def string(s: String): Parser[String]

    def label[A](msg: String)(p: Parser[A]): Parser[A]

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

    def defaultSucceed[A](a: A): Parser[A] =
      string("") map (_ => a)

    def succeed[A](a: A): Parser[A]

    def slice[A](p: Parser[A]): Parser[String]

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      if (n <= 0) succeed(List.empty[A])
      else map2(p, listOfN(n - 1, p))(_ :: _)
    }

    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) | succeed(List.empty[A])

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def sep[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] =
      sep1(p, s) | succeed(List.empty[A])

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    //one or more
    def sep1[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] =
      map2(p, many(s *> p))(_ :: _)

    def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
      map2(slice(p), p2)((_, b) => b)


    def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
      map2(p, slice(p2))((a, _) => a)

    implicit def regex(r: Regex): Parser[String]

    def quoted: Parser[String] =
      string("\"") *> thru("\"") map (_.dropRight(1))

    def escapedQuoted: Parser[String] =
      token(quoted) label "escaped quoted"

    def thru(s: String): Parser[String] =
      (".*?" + Pattern.quote(s)).r

    def doubleString: Parser[String] =
      token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

    def double: Parser[Double] =
      doubleString map (_.toDouble) label "double literal"

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      flatMap(p1)(a => map(p2)(b => (a, b)))

    def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      flatMap(p1)(a => map(p2)(b => f(a, b)))

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
      flatMap(p)(a => succeed(f(a)))

    def surround[A](p1: Parser[Any], p2: Parser[Any])(p: Parser[A]): Parser[A] =
      p1 *> p <* p2

    def whitespace: Parser[String] =
      "\\s*".r

    def eof: Parser[String] =
      regex("\\z".r) label "unexpected trailing characters"

    def root[A](p: Parser[A]): Parser[A] =
      p <* eof

    def token[A](p: Parser[A]): Parser[A] =
      attempt(p) <* whitespace

    def opt[A](p: Parser[A]): Parser[Option[A]] =
      p.map(x => {
        Some(x)
      }) or succeed(None)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))

    implicit class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)

      def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def many: Parser[List[A]] = self.many(p)
      def many1: Parser[List[A]] = self.many1(p)

      def slice: Parser[String] = self.slice(p)

      def as[B](b: B): Parser[B] = map(_ => b)

      //skip left
      def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)
      //skip right
      def <*(p2: => Parser[Any]): Parser[A] = self.skipR(p, p2)

      def sep1[B](pb: Parser[B]): Parser[List[A]] = self.sep1(p, pb)
      def sep[B](pb: Parser[B]): Parser[List[A]] = self.sep(p, pb)

      def run(s: String): Either[ParseError, A] =
        self.run(p)(s)

      def label(msg: String): Parser[A] =
        self.label(msg)(p)

      def opt: Parser[Option[A]] = self.opt(p)

      def scope(msg: String): Parser[A] =
        self.scope(msg)(p)
    }


  }

  trait JSON

  object JSON {

    case object JNull extends JSON

    case class JNumber(d: Double) extends JSON

    case class JString(s: String) extends JSON

    case class JBool(s: Boolean) extends JSON

    case class JArray(a: IndexedSeq[JSON]) extends JSON

    case class JObject(o: Map[String, JSON]) extends JSON


    def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
      import P.{string => _, _}

      implicit def tok[A](s: String): Parser[String] =
        token(P.string(s))

      def array: Parser[JArray] = surround("[", "]") {
        (value sep (",") map (lst => JArray(lst.toIndexedSeq)))
      } scope "array"


      def obj: Parser[JObject] =
        surround("{", "}") {
          (keyValue sep ",") map (lst => JObject(lst.toMap))
        } scope "obj"


      def literal: Parser[JSON] = {
        "null".as(JNull) |
          double.map(JNumber) |
          escapedQuoted.map(JString) |
          "true".as(JBool(true)) |
          "false".as(JBool(false))
      } scope "literal"

      def value: Parser[JSON] = literal | array | obj

      def keyValue: Parser[(String, JSON)] = (escapedQuoted <* ":") ** (whitespace *> value)

      root(whitespace *> (obj | array))
    }


  }


  def main(args: Array[String]): Unit = {
    import JSON._
    import Parser2._

    val example =
      """{
        "Company name": "Microsoft corporation",
        "Active": true,
        "Price": 30,
        "Related Companies": ["HPQ", "IBM", "YHOO", "DELL", "GOOG"]
        }""".stripMargin


    jsonParser(Parser2).run(example).fold(
      pe => println(pe),
      json => println(json)
    )

  }

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, charsConsumed) => Success(a, n + charsConsumed)
      case _                         => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(g, c) => Failure(f(g), c)
      case _             => this
    }

    def uncommit: Result[A] = this match {
      case Failure(get, true) => Failure(get, isCommitted = false)
      case _                  => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  type Parser2[+A] = ParseState => Result[A]

  object Parser2 extends Parsers[Parser2] {

    override def run[A](p: Parser2[A])(input: String): Either[ParseError, A] =
      p(ParseState(Location(input))).extract

    override def succeed[A](a: A): Parser2[A] = _ => Success(a, 0)

    override def or[A](p1: Parser2[A], p2: => Parser2[A]): Parser2[A] = s =>
      p1(s) match {
        case Failure(_, false) => p2(s)
        case r                 => r
      }

    override def flatMap[A, B](p: Parser2[A])(g: A => Parser2[B]): Parser2[B] =
      s => p(s) match {
        case Success(a, n)   => g(a)(s.advanceBy(n))
          .addCommit(n != 0)
          .advanceSuccess(n)
        case f@Failure(_, _) => f
      }

    override implicit def string(w: String): Parser2[String] = {
      lazy val msg = s"'$w'"
      s => {
        val i = firstNonMatchingIndex(s.loc.input, w, s.loc.offset)

        if (i == -1) {
          Success(w, w.length)
        } else {
          Failure(s.loc.advanceBy(i).toError(s"Expected: $msg"), i != 0)
        }
      }
    }

    override implicit def regex(r: Regex): Parser2[String] = {
      lazy val msg = s"regex $r"
      s => {
        r.findPrefixOf(s.input) match {
          case None    =>
            Failure(s.loc.toError(s"$msg"), isCommitted = false)
          case Some(m) =>
            Success(m, m.length)
        }
      }
    }

    override def slice[A](p: Parser2[A]): Parser2[String] = s => {
      p(s) match {
        case Success(_, charsConsumed) => Success(s.slice(charsConsumed), charsConsumed)
        case f@Failure(_, _)           => f
      }
    }

    def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
      var i = 0
      while (i < s1.length && i < s2.length) {
        if (s1.charAt(offset + i) != s2.charAt(i)) return i
        else i += 1
      }

      if (s1.length - offset >= s2.length) -1
      else s1.length - offset
    }

    override def scope[A](msg: String)(p: Parser2[A]): Parser2[A] =
      s => p(s).mapError(_.push(s.loc, msg))

    override def label[A](msg: String)(p: Parser2[A]): Parser2[A] =
      s => p(s).mapError(_.label(msg))

    override def attempt[A](p: Parser2[A]): Parser2[A] = s =>
      p(s).uncommit
  }

}
