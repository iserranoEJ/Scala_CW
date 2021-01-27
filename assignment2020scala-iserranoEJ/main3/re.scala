// Core Part about Regular Expression Matching
//=============================================

object CW8c {

// Regular Expressions

  import scala.language.implicitConversions    
  import scala.language.reflectiveCalls 

  abstract class Rexp
  case object ZERO extends Rexp
  case object ONE extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
  case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
  case class STAR(r: Rexp) extends Rexp             // star


  // some convenience for typing regular expressions

  def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
  }

  implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

  implicit def RexpOps (r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~ (s: Rexp) = SEQ(r, s)
  }

  implicit def stringOps (s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def % = STAR(s)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
  }

  // (1) Complete the function nullable according to
  // the definition given in the coursework; this 
  // function checks whether a regular expression
  // can match the empty string and Returns a boolean
  // accordingly.

  def nullable (r: Rexp) : Boolean = r match{
    case ZERO => false
    case ONE => true
    case CHAR(c) => false
    case ALT(s, c) => nullable(s) || nullable(c)
    case SEQ(s,c) => nullable(s) && nullable(c)
    case STAR(r) => true 
  }


  // (2) Complete the function der according to
  // the definition given in the coursework; this
  // function calculates the derivative of a 
  // regular expression w.r.t. a character.

  def der (c: Char, r: Rexp) : Rexp = r match{
    case ZERO => ZERO
    case ONE => ZERO
    case CHAR(d) => if(c == d) ONE else ZERO
    case ALT(r1, r2) => der(c, r1) | der(c, r2)
    case SEQ(r1, r2) => if(nullable(r1)){
      (der(c, r1) ~ r2) | der(c, r2)
    }
      else SEQ(der(c, r1), r2)
    case STAR(rs) => der(c, rs) ~ STAR(rs)
  }


  // (3) Complete the simp function according to
  // the specification given in the coursework; this
  // function simplifies a regular expression from
  // the inside out, like you would simplify arithmetic 
  // expressions; however it does not simplify inside 
  // STAR-regular expressions.


  def simp(r: Rexp) : Rexp = r match {
    
    case ALT(x,y) => (simp(x), simp(y)) match{
      case (r0, ZERO) => r0
      case (ZERO, r0) => r0
      case (r0, r1) if(r0==r1) => r0
      case (r0, r1) => simp(r0) | simp(r1)
    }

    case SEQ(x, y) => (simp(x), simp(y)) match{
      case (r0, ZERO) => ZERO
      case (ZERO, r0) => ZERO
      case (r0, ONE) => simp(r0)
      case (ONE, r0) => simp(r0)
      case (r0, r1) => simp(r0) ~ simp(r1)
    } 
    case r0 => r0
  }

  // (4) Complete the two functions below; the first 
  // calculates the derivative w.r.t. a string; the second
  // is the regular expression matcher taking a regular
  // expression and a string and checks whether the
  // string matches the regular expression

  def ders (s: List[Char], r: Rexp) : Rexp = s match{
    case Nil => r
    case c::cs => ders(cs, simp(der(c, r)))
  }

  def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList, r))


  // (5) Complete the size function for regular
  // expressions according to the specification 
  // given in the coursework.

  def size(r: Rexp): Int = r match {
    case ZERO => 1
    case ONE => 1
    case CHAR(c) => 1
    case ALT(a, b) => 1 + size(a) + size(b) 
    case SEQ(a, b) => 1 + size(a) + size(b) 
    case STAR(a) => 1 + size(a)  
  }
}