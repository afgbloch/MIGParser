package parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

object Parser extends StandardTokenParsers {
  lexical.reserved ++= List("module", "input", "output", "wire", "assign", "endmodule")
  lexical.delimiters ++= List(",", ";")
  import lexical.NumericLit

  val circuit:Set = ???
  
  def term: Parser[List[Node]] =
      "input" ~ rep(ident ~ ",") ~ ident ~ ";" ^^ {case key ~ inputs ~ input ~ end => Input(input) :: inputs.map{x => Input(x._1)}} |
      "output" ~ rep(ident ~ ",") ~ ident ~ ";" ^^ {case key ~ inputs ~ input ~ end => Gate(input,  Output) :: inputs.map{x => Gate(x._1, Output)}} |
      "wire" ~ rep(ident ~ ",") ~ ident ~ ";" ^^ {case key ~ inputs ~ input ~ end => Gate(input, Wire) :: inputs.map{x => Gate(x._1, Wire)}}
  
  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    
    phrase(term)(tokens) match {
      case Success(nodes, _) => nodes.foreach{
        case Gate(name, _) => println(name)
        case _@n => println(name)
      }
      case e =>
        //println(e)
    }
  }

  
  def main(args: Array[String]): Unit = {

    val file = io.Source.fromFile(args.head)
    for(line <- file.getLines()) {
      parse(line)
    }
    
  }
}
