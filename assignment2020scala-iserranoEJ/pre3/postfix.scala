// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = ops.contains(op)
def prec(op1: String, op2: String) : Boolean =  precs.get(op1).get > precs.get(op2).get ||  precs.get(op1).get == precs.get(op2).get
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
	
    if(!is_left_par(stack.last) && prec(stack.last, s)){
      println("nice")
      check_op(s, stack.dropRight(1), output:+stack.last)
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
				else if(prec(st.last, x)) syard(toks, st.dropRight(1), out:+st.last)
				else syard(xs, st:+x, out)
			}
			else syard(xs, st:+x, out)
		}
		case x::xs if(is_number(x))=>{
			syard(xs, st, out:+x)
		}
	}
}



// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

syard(split("3 + 4 + 5"))           // 3 4 + 5 +
syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +
syard(split("3 + 4 * ( 2 - 1 )")) == List("3", "4", "2", "1", "-", "*", "+")
syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) == List("3", "4", "5", "+", "+")
syard(split("5 + 7 / 2")) == List("5", "7", "2", "/", "+")
syard(split("5 * 7 / 2")) == List("5", "7", "*", "2", "/")
 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as arguments. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match{
	case Nil => st.last.toInt
	case x::xs if (is_number(x))=>  compute(xs, st:+x.toInt) 
	case top::bottom if(is_op(top))=>{
		if (top == "+") {
			println(st.last)
			println(st(st.length - 2)) 
			compute(bottom, st.dropRight(2):+(st.last + st(st.length - 2)))
		}
		else if (top == "-") {
			println(st.last)
			println(st(st.length - 2)) 
			compute(bottom, st.dropRight(2):+(st(st.length - 2) - st.last))
		}
		else if (top == "*")  {
				println(st.last)
			println(st(st.length - 2)) 
			compute(bottom, st.dropRight(2):+(st.last * st(st.length - 2)))
		}
		else{
				println(st.last)
			println(st(st.length - 2)) 
			compute(bottom, st.dropRight(2):+(st(st.length - 2) / st.last))
		}
	}
}



// test cases
compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
compute(syard(split("10 + 12 * 33")))       // 406
compute(syard(split("( 5 + 7 ) * 2")))      // 24
compute(syard(split("5 + 7 / 2")))          // 8
compute(syard(split("5 * 7 / 2")))          // 17
compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


