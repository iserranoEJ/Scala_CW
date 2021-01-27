// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = if(x._1 < dim && x._1 >= 0 && x._2 < dim && x._2 >=0 && !path.contains(x)) true else false 
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] ={
  val all_moves = List[Pos](new Pos(x._1 + 1, x._2 + 2), new Pos(x._1 + 2, x._2 + 1), new Pos(x._1 + 2, x._2 - 1), new Pos(x._1 + 1, x._2 - 2), 
  new Pos(x._1 - 1, x._2 - 2), new Pos(x._1 - 2, x._2 - 1), new Pos(x._1 - 2, x._2 + 1), new Pos(x._1 - 1, x._2 + 2) )
  all_moves.filter(a => is_legal(dim, path, a))
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match{
  case Nil => None
  case x::xs => {
    val value = f(x)
    if(value.isDefined) value else first(xs,f)
    }
}

def is_legal_cycle(dim: Int, path: Path, x: Pos) : Boolean = if(x._1 < dim && x._1 >= 0 && x._2 < dim && x._2 >=0 ) true else false 



def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val possible = for (n <- legal_moves(dim, path, x)) yield (n, legal_moves(dim, path, n).length)
    val ordered = possible.sortWith(_._2 < _._2)
    for(n <- ordered) yield n._1
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 

def cycle_moves(dim: Int, path: Path, x: Pos) : List[Pos] ={
  val all_moves = List[Pos](new Pos(x._1 + 1, x._2 + 2), new Pos(x._1 + 2, x._2 + 1), new Pos(x._1 + 2, x._2 - 1), new Pos(x._1 + 1, x._2 - 2), 
  new Pos(x._1 - 1, x._2 - 2), new Pos(x._1 - 2, x._2 - 1), new Pos(x._1 - 2, x._2 + 1), new Pos(x._1 - 1, x._2 + 2) )
  all_moves.filter(a => is_legal_cycle(dim, path, a))
}

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = path match {
  case x if(path.length == dim*dim) => if(cycle_moves(dim, path, path.head).contains(path.last)) Option[Path] (x) else None
  case y => {
      first(ordered_moves(dim, y, y(0)), x => first_closed_tour_heuristics(dim, x +: y))
  }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = path match {
  case x if(path.length == dim*dim) => Option[Path] (x)
  case y => first(ordered_moves(dim, y, y(0)), x => first_tour_heuristics(dim, x +: y))
}

}
