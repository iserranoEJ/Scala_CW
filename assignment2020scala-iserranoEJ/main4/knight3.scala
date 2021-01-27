// Finding a single tour on a "mega" board
//=========================================

object CW9c {

    // !!! Copy any function you need from file knight1.scala !!!
    // !!! or knight2.scala                                   !!! 
    //
    // If you need any auxiliary function, feel free to 
    // implement it, but do not make any changes to the
    // templates below.


    type Pos = (Int, Int)    // a position on a chessboard 
    type Path = List[Pos]    // a path...a list of positions

    //(9) Implement a function that searches for a 
    //    you have to be careful to write a tail-recursive version as this 
    //    function will be called with dimensions of up to 70 * 70
    //    and starting field (0, 0). It has to produce a solution within
    //    30 seconds.


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

    def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
        val possible = for (n <- legal_moves(dim, path, x)) yield (n, legal_moves(dim, path, n).length)
        val ordered = possible.sortBy(_._2)
        for(n <- ordered) yield n._1
    }
    import scala.annotation.tailrec


    def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
        tour_on_mega_board_tailrec(dim, List(path))
    }

    @tailrec
    def tour_on_mega_board_tailrec(dim: Int, acc: List[Path]) : Option[Path] = 
    acc match {
        case Nil => None
        case _ =>  
        acc.head match {
            case Nil => None
            case x if(x.size == dim*dim) => Option[Path] (x)
            case y => {
                val cs = ordered_moves(dim, y, y.head)
                tour_on_mega_board_tailrec(dim,cs.map(_+:y):::acc.tail)
            }
        
        }

    }

}
