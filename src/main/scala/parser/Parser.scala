package parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

object Parser extends StandardTokenParsers {
  lexical.reserved ++= List("input", "output", "wire", "assign", "one")
  lexical.delimiters ++= List(",", ";", "=", "~", "&", "|", "(", ")")
  import lexical.NumericLit

  var circuit:Set[Node] = Set()
  var inputSet:Set[parser.Input] = Set()
  var outputSet:Set[parser.Output] = Set()
  
  def term: Parser[List[Node]] =
      "input" ~> rep(ident <~ ",") ~ ident <~ ";" ^^ {case inputs ~ input => Input(input) :: inputs.map{x => Input(x)}} |
      "output" ~> rep(ident <~ ",") ~ ident <~ ";" ^^ {case  inputs ~ input => Output(input) :: inputs.map{x => Output(x)}} |
      "wire" ~ "one" ~ "," ~> rep(ident <~ ",") ~ ident <~ ";" ^^ {case  inputs ~ input => Gate(input) :: inputs.map{x => Gate(x)}} |
      assignParser
      
      
def assignParser: Parser[List[Node]] =
      ("assign" ~> ident <~ "=" ) ~ (inputGateParser <~ "&") ~ (inputGateParser <~ ";") ^^ 
          {case name ~ in1 ~ in2 => List(Assign(name, And, in1 ::: in2)) } |
      ("assign" ~> ident <~ "=" ) ~ (inputGateParser <~ "|") ~ (inputGateParser <~ ";") ^^ 
          {case name ~ in1 ~ in2 => List(Assign(name, Or, in1 ::: in2)) } |
      ("assign" ~> ident <~ "=") ~
          ("(" ~> inputGateParser <~ "&" ~ inputGateParser ~ ")" ~ "|") ~
          ("(" ~> inputGateParser ~ "&" ~> inputGateParser <~ ")" ~ "|") ~
          ("(" ~> inputGateParser <~ "&" ~ inputGateParser ~ ")" ~ ";") ^^ 
          {case  name ~ in1 ~ in2 ~ in3 => List(Assign(name, Majority, in1 ::: in2 ::: in3)) } |
     ("assign" ~> ident  <~ "=") ~ inputGateParser <~ ";" ^^ { case name ~ in1 => List(Assign(name, None, in1))} |
     "assign" ~ "one" ~ "=" ~ numericLit ~";" ^^^ List(One)


def inputGateParser: Parser[List[(Boolean, String)]] = 
      "~" ~> ident ^^ {case ident => List((true, ident))} | 
      ident ^^ {case ident => List((false, ident))}   |
      "~" ~ "one" ^^^ List((true, "one")) | 
      "one" ^^^ List((false, "one"))   

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    
    phrase(term)(tokens) match {
      case Success(nodes, _) => nodes.foreach{
        case o@Output(_) => circuit += o; outputSet += o
        case g@Gate(_) => circuit += g;
        case i@Input(_) => i.asap = 0; circuit += i; inputSet += i
        case e@One => circuit += e
        case Assign(name, tpe, inputs) => assign(name, tpe, inputs)
      }
      case e@_ => //println("Parsing error" + e)
    
  }
}
    
def assign(name:String, tpe:GateType, inputs:List[(Boolean, String)]) = {
  val gate = findGate(name)
  gate match {
    case g@Gate(name) => {
      g.tpe = tpe
      g.inputs = inputs.map{
        case (b, s) => {
          val pred = findGate(s)
          pred match {
            case g2@Gate(_) => g2.outputs ::= g
            case i@Input(_) => i.outputs ::= g
            case o@One => o.outputs ::= g
            case _ => //nothing
          }
          GateInput(b, pred)
        }
      }
    }
    case o@Output(name) => {
      val (b, s) = inputs.head
      val pred = findGate(s)
      pred match {
        case g@Gate(_) => g.outputs ::= o
        case i@Input(_) => i.outputs ::= o
        case o2@One => o2.outputs ::= o
        case _ => //nothing
      }
      o.inputs = List(GateInput(b, pred))
    }
    case _ =>
  
  }
}

def findGate(name:String) = circuit.find{x => x.name == name}.get
  
  def main(args: Array[String]): Unit = {

    val file = io.Source.fromFile(args.head)
    for(line <- file.getLines()) {
      parse(line)
    }
//    circuit.foreach { 
//      case Gate(n) => print(n+ ";") 
//      case Input(n) => print(n+ ";") 
//      case Output(n) => print(n+ ";") 
//      case One => print("one;") 
//     }
    var maj = 0
    var noInvertedMaj = 0;
    var oneInvertedMaj = 0;
    var allInvertedMaj = 0;
    var and = 0
    var or = 0
    var inverted = 0
    circuit.foreach {
      case g@Gate(_) => {
        if(g.tpe == Majority){
          maj+=1
          if(g.inputs.forall { x => !x.inverted }) {noInvertedMaj+=1}
          if(g.inputs.exists { x => x.inverted }) {oneInvertedMaj+=1}
          if (g.inputs.forall { x => x.inverted }) {allInvertedMaj+=1}
          
        }
        if(g.tpe == And){and+=1}
        if(g.tpe == Or){or+=1}
      }
      case _ =>
    }
    println("Majority =" + maj)
    println("Majority with at least one inverted input =" + oneInvertedMaj)
    println("Majority with no inverted input =" + noInvertedMaj)
    println("Majority with all inverted input =" + allInvertedMaj)
    println("And =" + and)
    println("Or =" + or)
    var inputCounter = 0
//    
//    inputSet foreach {
//      case Input(n) => print(n+ ";"); inputCounter+=1
//    }
//    println("Inputs = " + inputCounter)
    val time = asap()
    //alap(outputSet, time)
    circuit.foreach{
      x => println(x.name + " : " + x.asap/* + " : " + x.alap*/)
    }
   
  }

def asap() = {
  var v = (circuit -- inputSet) - One

  while(!v.isEmpty) {
    var tmp = v
    v.foreach {
      case i:Succ => {
        val l = i.inputs.map { x => x.node.asap }
        if(l.forall { x => x != -1 }) {
          i.asap = l.max + 1
          tmp = v - i
        }
      }
    }
    v = tmp
  }
}

def alap() = {
  var v = (circuit -- inputSet) - One

  while(!v.isEmpty) {
    var tmp = v
    v.foreach {
      case i:Succ => {
        val l = i.inputs.map { x => x.node.asap }
        if(l.forall { x => x != -1 }) {
          i.asap = l.max + 1
          tmp = v - i
        }
      }
    }
    v = tmp
  }
}
}
