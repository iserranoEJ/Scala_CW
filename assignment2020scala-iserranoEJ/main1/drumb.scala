// Core Part about a really dumb investment strategy
//===================================================

object CW6b {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 


// (1) The function below takes a stock symbol and a year as arguments.
//     It should read the corresponding CSV-file and then extract the January 
//     data from the given year. The data should be collected in a list of
//     strings (one entry for each line in the CSV-file).

import io.Source
import scala.util._

def get_january_data(symbol: String, year: Int) : List[String] ={
    for(n <- Source.fromFile("./"++symbol++".csv").getLines.toList if(n.startsWith(year.toString ++ "-01"))) yield n
}
get_january_data("GOOG", 1980) == List()
get_january_data("GOOG", 2010).head == "2010-01-04,312.204773"


// (2) From the output of the get_january_data function, the next function 
//     should extract the first line (if it exists) and the corresponding
//     first trading price in that year with type Option[Double]. If no line 
//     is generated by get_january_data then the result is None; and Some if 
//     there is a price.


def get_first_price(symbol: String, year: Int) : Option[Double] = {
    val tupleList = for(n <- get_january_data(symbol, year) if(Option[String](n).isDefined)) yield (n.split(",").head, Option[Double](n.split(",").last.toDouble))
    if(tupleList == Nil) None else tupleList.head._2
    //tupleList.head._2
}
get_first_price("GOOG", 2010) == Some(312.204773)
// (3) Complete the function below that obtains all first prices
//     for the stock symbols from a portfolio (list of strings) and 
//     for the given range of years. The inner lists are for the
//     stock symbols and the outer list for the years.


def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = {
    for (x <- years.toList) yield {
        for(n<- portfolio) yield get_first_price(n, x)
    }
}
    

// (4) The function below calculates the change factor (delta) between
//     a price in year n and a price in year n + 1. 

def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = (price_old, price_new) match{

    case (None, None) => None
    case (None, _) => None
    case (_, None) => None
    case(_,_) => Option[Double]((price_new.get - price_old.get)/ price_old.get)
}

get_delta(None, None) == None
get_delta(Some(50.0), None) == None
get_delta(None, Some(100.0)) == None
get_delta(Some(50.0), Some(100.0)) == Some(1.0)

// (5) The next function calculates all change factors for all prices (from a 
//     portfolio). The input to this function are the nested lists created by 
//     get_prices above.

// data ->

def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = {
    for(n <- (0 until data.length -1).toList) yield
        for(x <- (0 until data(n).length).toList) yield {
            get_delta(data(n)(x),data(n+1)(x) )
        }
}

get_deltas(get_prices(List("GOOG", "AAPL"), 2010 to 2012)) ==
(List(List(Some(-0.03573991804411003), Some(0.539974575389325)),
List(Some(0.10103414222249969), Some(0.24777764141006836))))
// List(
//     List(Some(a1), Some(b1))
//     List(Some(a2), Some(b2))
//     List(Some(a3), Some(b3))
// )
//             |
//             V
// List(
//     List(Some(a1-a2), Some(b1-b2))
//     List(Some(a2-a3), Some(b2-b3))
// )

// val lst2 = List(
//     List(Some(-0.111), Some(0.222), Some(0.111)),
//     List(Some(-0.333), Some(0.444), Some(0.333))
// )



// (6) Write a function that given change factors, a starting balance and an index,
//     calculates the yearly yield, i.e. new balance, according to our dumb investment 
//     strategy. Index points to a year in the data list.

def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    val lst = for(n <- data(index) if(n.isDefined)) yield n.get
    ((for(n <- lst) yield n.toDouble*(balance.toDouble/lst.length)).sum.toLong + balance)  
}

val ds = get_deltas(get_prices(List("GOOG", "AAPL"), 2010 to 2012))
yearly_yield(ds, 100, 0) == 125
yearly_yield(ds, 100, 1) == 117

// (7) Write a function compound_yield that calculates the overall balance for a 
//     range of years where in each year the yearly profit is compounded to the new 
//     balances and then re-invested into our portfolio. For this use the function and 
//     results generated under (6). The function investment calls compound_yield
//     with the appropriate deltas and the first index.

def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = {
    if(index == data.length) balance
    else compound_yield(data, yearly_yield(data, balance, index), index + 1)
}


def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = {
    compound_yield(get_deltas(get_prices(portfolio, years)), start_balance, 0)
}

investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2000, 100) == 100
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2001, 100) == 27
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2002, 100) == 42
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2003, 100) == 27
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2004, 100) == 38
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2005, 100) == 113
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2006, 100) == 254
investment(List("GOOG", "AAPL", "BIDU"), 2000 to 2007, 100) == 349
investment(List("GOOG", "AAPL", "BIDU"), 1990 to 2017, 100) == 11504


//Test cases for the two portfolios given above

//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))

}
