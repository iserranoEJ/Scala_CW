// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================
object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}

def is_left_assoc(operator: Assoc) = {
  if(operator.toString == "LA") true else false
}

// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(op: String) : Boolean = ops.contains(op)
def prec_left(op1: String, op2: String) : Boolean =  precs.get(op1).get > precs.get(op2).get ||  precs.get(op1).get == precs.get(op2).get
def prec_right(op1: String, op2: String) : Boolean =  precs.get(op1).get > precs.get(op2).get 
def is_number(s: String) = s.forall(_.isDigit)
def is_left_par(s: String): Boolean = s == "("
def is_right_par(s: String): Boolean = s == ")"

def check_right_par(stack: Toks, output: Toks):(Toks,Toks)={
	if(!is_left_par(stack.last)){ 
		check_right_par(stack.dropRight(1), output:+stack.last)
	}
	else if(is_left_par(stack.last)) (stack.dropRight(1), output)
	else (Nil,Nil)
}

def check_op(s: String, stack: Toks, output: Toks):(Toks,Toks)= stack match{
  case x::xs => {
    val op = assoc(s) 
   
    if(is_left_assoc(op) && prec_left(stack.last, s)){
      println(stack)
      check_op(s,stack.dropRight(1), output:+stack.last)
    }
    else if(!is_left_assoc(op) && prec_right(stack.last, s)){
      println(stack)
      check_op(s,stack.dropRight(1), output:+stack.last)
    }
    else{
     
      (stack:+s, output)
    }
  }  
	case _ => (stack:+s, output)
}
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks ={

 toks match {
		
		case Nil => out :::st.reverse
		case "("::xs => syard(xs, st:+ "(", out)
		case ")"::xs =>{
			if(is_left_par(st.last)) syard(xs,  st.dropRight(1), out)
			else syard(toks, st.dropRight(1), out:+st.last)
		}
		case x::xs if (is_op(x))=> {
			
			if(!st.isEmpty){
				if(is_left_par(st.last)) syard(xs, st:+x, out)
				else if(is_left_assoc(assoc(x)) && prec_left(st.last, x)) syard(toks ,st.dropRight(1), out:+st.last)
				else if(!is_left_assoc(assoc(x)) && prec_right(st.last, x)) syard(toks, st.dropRight(1), out:+st.last)
				else syard(xs, st:+x, out)
			}
			else syard(xs, st:+x, out)
		}
		case x::xs if(is_number(x))=>{
			syard(xs, st, out:+x)
		}
	}
}

def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match{
	case Nil => st.last.toInt
	case x::xs if (is_number(x))=>  compute(xs, st:+x.toInt) 
	case top::bottom if(is_op(top))=>{
		if (top == "+") {
		
			compute(bottom, st.dropRight(2):+(st.last + st(st.length - 2)))
		}
		else if (top == "-") {
			compute(bottom, st.dropRight(2):+(st(st.length - 2) - st.last))
		}
		else if (top == "*")  {
			compute(bottom, st.dropRight(2):+(st.last * st(st.length - 2)))
		}
    else if (top == "^"){
			compute(bottom, st.dropRight(2):+(math.pow(st(st.length - 2).toDouble, st.last.toDouble)).toInt)
		}
		else{
			compute(bottom, st.dropRight(2):+(st(st.length - 2) / st.last))
		}
	}
}

// test cases
 syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.


// test cases
compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
compute(syard(split("10 + 12 * 33")))       // 406
compute(syard(split("( 5 + 7 ) * 2")))      // 24
compute(syard(split("5 + 7 / 2")))          // 8
compute(syard(split("5 * 7 / 2")))          // 17
compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
compute(syard(split("4 ^ 3 ^ 2")))      // 262144
compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
