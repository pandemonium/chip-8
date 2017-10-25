package chip8
package `binary-parser`

trait Parsers { parsers =>
  import scala.language.implicitConversions
  import Function.const

  type Word      = Short
  type Sentence  = Vector[Word]
  type Parser[A] = ParseState => ParseResult[A]

  def run[A](p: Parser[A])(input: Sentence): ParseResult[A] =
    p(ParseState(input, 0))

  sealed trait ParseResult[+A]
  case class Success[A](result: A, remaining: ParseState)
    extends ParseResult[A]
  case class Failure[A](remaining: ParseState)
    extends ParseResult[A]

  case class ParseState(input: Sentence, nibbleIndex: Int) {
    private def word   = nibbleIndex / 4
    private def nibble = nibbleIndex % 4
    private def extract(bitmap: Word, length: Int): Word = {
      val maskImage = math.pow(2, 4.0 * length).toInt - 1
      val shift     = (3 - nibble - length + 1) * 4
      val mask      = maskImage << shift

      ((bitmap & mask) >> shift).asInstanceOf[Word]
    }

    def nibble(length: Int) = extract(input(word), length)

    def advance(delta: Int): ParseState =
      copy(nibbleIndex = nibbleIndex + delta)
    
    def isExhausted = word >= input.length
  }

  case class ~[A, B](a: A, b: B)

  def nibble(n: Word)            = accept(n.==, 1)
  def word(w: Word, length: Int) = accept(w.==, length)
  def byte(w: Word)              = accept(w.==, 2)
  def capture(length: Int)       = accept(const(true), length)

  def accept(p: Word => Boolean, length: Int): Parser[Word] = 
    state => state nibble length match {
      case bitmap if p(bitmap) =>
        succeed(bitmap, state advance length)
      case _ =>
        fail(state)
    }

  def succeed[A](result: A, state: ParseState): ParseResult[A] = 
    Success(result, state)

  def fail[A](ps: ParseState): ParseResult[A] = 
    Failure(ps)

  def product[A, B](p: Parser[A], q: Parser[B]): Parser[A ~ B] =
    for (a <- p; b <- q) 
      yield new ~ (a, b)

  def coproduct[A, B](p: Parser[A], q: => Parser[B]): Parser[A Either B] = 
    state => p(state) match {
      case l @ Failure(_)    => q(state) map Right.apply
      case r @ Success(_, _) => r map Left.apply
    }

  def or[A <: C, B <: C, C](p:    Parser[A], 
                            q: => Parser[B]): Parser[C] =
    state => p(state) match {
      case l @ Failure(_)    => q(state)
      case r @ Success(_, _) => r
    }

  def many[A](p: Parser[A]): Parser[Seq[A]] =
    state => p(state) match {
      case Failure(_) => 
        succeed(Seq.empty, state)
      case Success(a, state1) => 
        if (state1.isExhausted)
          succeed(Seq(a), state1)
        else          
          many(p)(state1) match {
            case Failure(_) =>
              succeed(Seq(a), state1)
            case Success(as, state2) =>
              succeed(a +: as, state2)
          }
    }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p(_) map f

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    state => map(p)(f)(state) match {
      case Success(next, state1) => next(state1)
      case _                     => fail(state)
    }

  implicit def intIsParserOps(x: Int): ParserOps[Word] = 
    new ParserOps(nibble(x.asInstanceOf[Word]))

  implicit def intIsParser(x: Int): Parser[Word] = 
    nibble(x.asInstanceOf[Word])

  implicit class IntParserOps(x: Int) {
    def unary_! : Parser[Word] = capture(x)
  }

  implicit class ParserOps[A](p: Parser[A]) {
    def ~ [B] (q: Parser[B]): Parser[A ~ B]             = product(p, q)
    def ^^ [B] (f: A => B): Parser[B]                   = p map f
    def | [B <: C, C >: A] (q: => Parser[B]): Parser[C] = parsers.or[A, B, C](p, q)
    def map[B](f: A => B): Parser[B]                    = parsers.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B]        = parsers.flatMap(p)(f)
  }

  implicit class ResultOps[A](r: ParseResult[A]) {
    def map[B](f: A => B): ParseResult[B] = r match {
      case Success(res, rem) => Success(f(res), rem)
      case Failure(x)        => Failure(x)
    }
  }
}