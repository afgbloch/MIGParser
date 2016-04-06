package parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.util.Scanner
import java.io._

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

  def parse(res:Results) = {
    val p = io.Source.fromFile("test/" + res.name).getLines.toList.par.map { l =>
      val tokens = new lexical.Scanner(l)
      phrase(term)(tokens)
    }
    p.seq.foreach {
      case Success(nodes, _) => nodes.foreach {
        case o @ Output(_) => res.nodeMap += o.name -> o; res.outputSet += o
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
    
    var bench:List[Results] = List()
    print("table/readable(1/0) : ")
    val table = scala.io.StdIn.readLine().toInt == 1
    
//    print("Numbers of bench = ")
//    val nb = scala.io.StdIn.readLine().toInt
//    
//    for(i <- 1 to nb ) {
//      print("File name : ")
//      val res = new Results
//      res.name = scala.io.StdIn.readLine()
//      bench ::= res
//    }
////    
    (new File("test")).listFiles.filter(x => x.isFile && (x.getName.takeRight(2) == ".v")).toList.foreach { file =>
      val res = new Results
      res.name = file.getName;println(file.getName)
      bench ::= res
    }
    
    bench.par.foreach{res => run(res); println("Done with " + res.name)}
      
    if(table) {
    	val writer = new PrintWriter(new File("results.csv"))
      val majFanoutNb = bench.map(_.majFanout.size).max
      val nodeFanoutNb = bench.map(_.totalFanout.size).max
    	var head = "benchmark|cp|# nodes in cp|% maj in cp|# inputs|# outputs|% inverted outputs|# nodes|% majority|" +
    			"% with zero inverted input|% with one inverted input|% with two inverted inputs|" +
    			"% with three inverted inputs|% with zero maj input|%  with one maj input|" + 
    			"% with two maj input|% with three maj input|% and|% with zero inverted input|" +
    			"% with one inverted input|% with two inverted inputs|% or|% with zero inverted input|" +
    			"% with one inverted input|% with two inverted inputs|"
    	for(i <- 1 to nodeFanoutNb) {
    	  head += "% nodes with " + i + " fan-out|"
    	}
      for(i <- 1 to majFanoutNb) {
    	  head += "% maj with " + i + " fan-out|"
    	}
    	writer.write(head + "\n")
   
      bench.foreach{ res =>
        var line = res.name.take(res.name.size-2) + "|" + res.cp + "|" + res.cpGateNb + "|" + res.cpMaj_% + "|" +
                res.inputSet.size + "|" + res.outputSet.size + "|" + res.invertedOutput_% + "|" + res.circuit.size + "|" +
                res.maj_% + "|" + res.noInvertedMaj_% + "|" + res.oneInvertedMaj_% + "|" + res.twoInvertedMaj_% + "|" +
                res.threeInvertedMaj_% + "|" + res.noMajInput_% + "|" + res.oneMajInput_% + "|" + res.twoMajInput_% + "|" + 
                res.threeMajInput_% + "|" + res.and_% + "|" + res.noInvertedAnd_% + "|" + res.oneInvertedAnd_% + "|" +
                res.twoInvertedAnd_% + "|" + res.or_% + "|" + res.noInvertedOr_% + "|" + res.oneInvertedOr_% + "|" + 
                res.twoInvertedOr_% + "|"
                
        for(i <- 1 to nodeFanoutNb) {
        	line += res.totalFanout_%(i)+ "|"
        }
        for(i <- 1 to majFanoutNb) {
        	line += res.majFanout_%(i) + "|"
        }
        writer.write(line + "\n")
     
      }
      writer.close()
    	println("===========================")
    } else {
      bench.foreach{ res =>
        println("Name = " + res.name)
        println("Critical path = " + res.cp)
        println("  Nodes with zero mobility = " + res.cpGateNb)
        println("  Maj with zero mobility = " + res.cpMajNb + " (" + res.cpMaj_% + " %)")
        println("Nodes = " + res.circuit.size)
        println("Inputs = " + res.inputSet.size)
        println("Outputs = " + res.outputSet.size)
        println("Output with inverted input = " + res.invertedOutput + " (" + res.invertedOutput_% + " %)")
        println("Majority = " + res.maj + " (" + res.maj_% + " %)")
        println("  Inverted inputs")
        println("    no inverted input = " + res.noInvertedMaj + " (" + res.noInvertedMaj_% + " %)")
        println("    one inverted input = " + res.oneInvertedMaj + " (" + res.oneInvertedMaj_% + " %)")
        println("    two inverted inputs = " + res.twoInvertedMaj + " (" + res.twoInvertedMaj_% + " %)")
        println("    three inverted inputs = " + res.threeInvertedMaj + " (" + res.threeInvertedMaj_% + " %)")
        println("  Majority Inputs")
        println("    no Majority input = " + res.noMajInput + " (" + res.noMajInput_% + " %)")
        println("    one Majority input = " + res.oneMajInput + " (" + res.oneMajInput_% + " %)")
        println("    two Majority inputs = " + res.twoMajInput + " (" + res.twoMajInput_% + " %)")
        println("    three Majority inputs = " + res.threeMajInput + " (" + res.threeMajInput_% + " %)")
        println("And = " + res.and + " (" + res.and_% + "%)")
        println("  And with no inverted input = " + res.noInvertedAnd + " (" + res.noInvertedAnd_% + " %)")
        println("  And with one inverted input = " + res.oneInvertedAnd + " (" + res.oneInvertedAnd_% + " %)")
        println("  And with two inverted inputs = " + res.twoInvertedAnd + " (" + res.twoInvertedAnd_% + " %)")
        println("Or = " + res.or + " (" + res.or_% + "%)")
        println("  Or with no inverted input = " + res.noInvertedOr + " (" + res.noInvertedOr_% + " %)")
        println("  Or with one inverted input = " + res.oneInvertedOr + " (" + res.oneInvertedOr_% + " %)")
        println("  Or with two inverted inputs = " + res.twoInvertedOr + " (" + res.twoInvertedOr_% + " %)")
        println("Fanout")
        println("  Nodes with : ")
        res.totalFanout.keys.toSeq.sorted.foreach{ i =>
          println("    " + i + " fan-out = " + res.totalFanout(i) +" (" + res.totalFanout_%(i) + " %)")
        }
        println("  Majority with : ")
        res.majFanout.keys.toSeq.sorted.foreach{ i =>
          println("    " + i + " fan-out = " + res.majFanout(i) +" (" + res.majFanout_%(i) + " %)")
        }
      }
      println("===========================")
    }
  }

  def run(res: Results) = {
    parse(res)
    asap(res)
    alap(res)

    res.circuit.foreach { gate =>
      
      val fanout = gate.outputs.size
      res.totalFanout.get(fanout) match {
        case Some(i) => res.totalFanout += fanout -> (i+1)
        case scala.None => res.totalFanout += fanout -> 1
      }
      
      var inverted = gate.inputs.count(_.inverted)
      if(gate.mobiity == 0) {res.cpGateNb+=1}
      
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
          
          if(gate.mobiity == 0) {res.cpMajNb+=1}
          
          val majInputsNb = gate.inputs.count { 
            case GateInput(_, i@Gate(_)) if i.tpe == Majority => true
            case _ => false
          } 
          
          majInputsNb match {
            case 0 => res.noMajInput += 1
            case 1 => res.oneMajInput += 1
            case 2 => res.twoMajInput += 1
            case 3 => res.threeMajInput += 1
            case _ => throw new MatchError("Invalide number of inputs")
          }
          
          res.majFanout.get(fanout) match {
            case Some(i) => res.majFanout += fanout -> (i+1)
            case scala.None => res.majFanout += fanout -> 1
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

        case _ => //throw new MatchError("Undefind Gate Type")
      }
    }
  }

  def asap(res:Results) = {
    var v = res.inputSet
      .flatMap(_.outputs)
      .filter {
        case Gate(_) => true
        case _       => false
      }
    while (!v.isEmpty) {
      var tmp = v
      v.foreach {
        case i @ Gate(_) => {
          val l = i.inputs.map(_.node.asap)
          if (l.forall(_ != -1)) {
            i.asap = l.max + 1
            if (i.asap > res.cp) { res.cp = i.asap }
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
  }

  def alap(res:Results) = {
    res.outputSet.foreach(_.alap = res.cp + 1)
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
            i.alap = l.min - 1
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
