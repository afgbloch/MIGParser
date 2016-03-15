package parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.util.Scanner

object Parser extends StandardTokenParsers {
  lexical.reserved ++= List("input", "output", "wire", "assign", "one")
  lexical.delimiters ++= List(",", ";", "=", "~", "&", "|", "(", ")")
  import lexical.NumericLit

  def term: Parser[List[Node]] =
    "input" ~> rep(ident <~ ",") ~ ident <~ ";" ^^ { case inputs ~ input => Input(input) :: inputs.map { x => Input(x) } } |
      "output" ~> rep(ident <~ ",") ~ ident <~ ";" ^^ { case inputs ~ input => Output(input) :: inputs.map { x => Output(x) } } |
      "wire" ~ "one" ~ "," ~> rep(ident <~ ",") ~ ident <~ ";" ^^ { case inputs ~ input => Gate(input) :: inputs.map { x => Gate(x) } } |
      assignParser

  def assignParser: Parser[List[Node]] =
    ("assign" ~> ident <~ "=") ~ (inputGateParser <~ "&") ~ (inputGateParser <~ ";") ^^
      { case name ~ in1 ~ in2 => List(Assign(name, And, in1 ::: in2)) } |
      ("assign" ~> ident <~ "=") ~ (inputGateParser <~ "|") ~ (inputGateParser <~ ";") ^^
      { case name ~ in1 ~ in2 => List(Assign(name, Or, in1 ::: in2)) } |
      ("assign" ~> ident <~ "=") ~
      ("(" ~> inputGateParser <~ "&" ~ inputGateParser ~ ")" ~ "|") ~
      ("(" ~> inputGateParser ~ "&" ~> inputGateParser <~ ")" ~ "|") ~
      ("(" ~> inputGateParser <~ "&" ~ inputGateParser ~ ")" ~ ";") ^^
      { case name ~ in1 ~ in2 ~ in3 => List(Assign(name, Majority, in1 ::: in2 ::: in3)) } |
      ("assign" ~> ident <~ "=") ~ inputGateParser <~ ";" ^^ { case name ~ in1 => List(Assign(name, None, in1)) } |
      "assign" ~ "one" ~ "=" ~ numericLit ~ ";" ^^^ List(One)

  def inputGateParser: Parser[List[(Boolean, String)]] =
    "~" ~> ident ^^ { case ident => List((true, ident)) } |
      ident ^^ { case ident => List((false, ident)) } |
      "~" ~ "one" ^^^ List((true, "one")) |
      "one" ^^^ List((false, "one"))

  def parse(file: String, res:Results) = {
    val p = io.Source.fromFile(file).getLines.toList.par.map { l =>
      val tokens = new lexical.Scanner(l)
      phrase(term)(tokens)
    }
    p.seq.foreach {
      case Success(nodes, _) => nodes.foreach {
        case o @ Output(_) =>
          res.nodeMap += o.name -> o; res.outputSet += o
        case g @ Gate(_) => res.nodeMap += g.name -> g; res.circuit += g;
        case i @ Input(_) => i.asap = 0; res.nodeMap += i.name -> i; res.inputSet += i
        case e @ One => res.nodeMap += e.name -> e
        case Assign(name, tpe, inputs) => assign(name, tpe, inputs, res)
      }
      case e @ _ => //println("Parsing error" + e)
    }
  }

  def assign(name: String, tpe: GateType, inputs: List[(Boolean, String)], res:Results) = {
    val gate = findGate(name, res)
    gate match {
      case g @ Gate(name) => {
        g.tpe = tpe
        g.inputs = inputs.map {
          case (b, s) => {
            val pred = findGate(s, res)
            pred match {
              case g2 @ Gate(_) => g2.outputs ::= g
              case i @ Input(_) => i.outputs ::= g
              case o @ One      => o.outputs ::= g
              case _            => //nothing
            }
            GateInput(b, pred)
          }
        }
      }
      case o @ Output(name) => {
        val (b, s) = inputs.head
        val pred = findGate(s, res)
        pred match {
          case g @ Gate(_)  => g.outputs ::= o
          case i @ Input(_) => i.outputs ::= o
          case o2 @ One     => o2.outputs ::= o
          case _            => //nothing
        }
        o.inputs = List(GateInput(b, pred))
      }
      case _ =>

    }
  }

  def findGate(name: String, res:Results): Node = res.nodeMap.get(name).get

  def main(args: Array[String]): Unit = {
    while(true){
      print("File name : ")
      run(scala.io.StdIn.readLine())
    }
  }

  def run(s: String) = {

    val res = new Results()

    parse(s, res)

    val timing = asap(res)
    println("Critical path = " + timing)
    alap(timing, res)

    res.circuit.foreach { gate =>
      var inverted = gate.inputs.count(_.inverted)

      gate.tpe match {
        case Majority => {
          res.maj += 1
          inverted match {
            case 0 => res.noInvertedMaj += 1
            case 1 => res.oneInvertedMaj += 1
            case 2 => res.twoInvertedMaj += 1
            case 3 => res.threeInvertedMaj += 1
            case _ => throw new MatchError("Invalide number of inputs")
          }
        }
        case And => {
          res.and += 1
          inverted match {
            case 0 => res.noInvertedAnd += 1
            case 1 => res.oneInvertedAnd += 1
            case 2 => res.twoInvertedAnd += 1
            case _ => throw new MatchError("Invalide number of inputs")
          }
        }
        case Or => {
          res.or += 1
          inverted match {
            case 0 => res.noInvertedOr += 1
            case 1 => res.oneInvertedOr += 1
            case 2 => res.twoInvertedOr += 1
            case _ => throw new MatchError("Invalide number of inputs")
          }
        }

        case _ => throw new MatchError("Undefind Gate Type")
      }
    }

    println("Majority = " + res.maj)
    println("Majority with no inverted input = " + res.noInvertedMaj)
    println("Majority with one inverted input = " + res.oneInvertedMaj)
    println("Majority with two inverted inputs = " + res.twoInvertedMaj)
    println("Majority with three inverted inputs = " + res.threeInvertedMaj)
    println("And = " + res.and)
    println("And with no inverted input = " + res.noInvertedAnd)
    println("And with one inverted input = " + res.oneInvertedAnd)
    println("And with two inverted inputs = " + res.twoInvertedAnd)
    println("Or = " + res.or)
    println("Or with no inverted input = " + res.noInvertedOr)
    println("Or with one inverted input = " + res.oneInvertedOr)
    println("Or with two inverted inputs = " + res.twoInvertedOr)

    val invertedOutput = res.outputSet.count(_.inputs.head.inverted)
    println("Output = " + res.outputSet.size)
    println("Output with inverted input = " + invertedOutput)

  }

  def asap(res:Results): Int = {
    var v = res.inputSet
      .flatMap(_.outputs)
      .filter {
        case Gate(_) => true
        case _       => false
      }
    var timing = -1
    while (!v.isEmpty) {
      var tmp = v
      v.foreach {
        case i @ Gate(_) => {
          val l = i.inputs.map(_.node.asap)
          if (l.forall(_ != -1)) {
            i.asap = l.max + 1
            if (i.asap > timing) { timing = i.asap }
            tmp = tmp - i
            i match {
              case i: Pred => {
                tmp ++= i.outputs
                  .filter(_.asap < i.asap + 1)
                  .filter {
                    case Gate(_) => true
                    case _       => false
                  }
              }
              case _ =>
            }
          }
        }
        case _ => println("error not a Succ")
      }
      v = tmp
    }
    timing
  }

  def alap(asap: Int, res:Results) = {
    res.outputSet.foreach(_.alap = asap + 1)
    var v = res.outputSet
      .flatMap(_.inputs)
      .map(_.node)
      .filter {
        case Gate(_) => true
        case _       => false
      }

    while (!v.isEmpty) {
      var tmp = v
      v.foreach {
        case i: Pred => {
          val l = i.outputs.map(_.alap)
          if (l.forall(_ != -1)) {
            i.alap = l.max - 1
            tmp = tmp - i
            i match {
              case i: Succ => {
                tmp ++= i.inputs
                  .filter(_.node.alap < i.alap + 1)
                  .map(_.node)
                  .filter {
                    case Gate(_) => true
                    case _       => false
                  }
              }
              case _ =>
            }
          }
        }
        case _ => println("error not a Succ")
      }
      v = tmp
    }
  }

}
