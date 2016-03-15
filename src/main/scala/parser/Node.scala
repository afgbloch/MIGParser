package parser

import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap

sealed abstract class Node {
  val name:String
  var asap:Int = -1
  var alap:Int = -1
  def mobiity = alap - asap
 override def hashCode = name.hashCode()
}

trait Pred {
  var outputs:List[Node] = Nil
}

trait Succ {
  var inputs:List[GateInput] = Nil
}

final case class Gate(name:String) extends Node with Pred with Succ{
  var tpe:GateType = None 
}

final case class Input(name:String) extends Node with Pred

final case class Output(name:String) extends Node with Succ

final case object One extends Node with Pred{
  val name:String = "one";
  asap = 0
}

final case class Assign(name:String, tpe:GateType, inputs:List[(Boolean, String)]) extends Node


sealed abstract class GateType
final case object And extends GateType
final case object Or extends GateType
final case object Majority extends GateType
final case object None extends GateType

sealed case class GateInput(inverted:Boolean, node:Node)

class Results {
  var maj = 0
  var noInvertedMaj = 0
  var oneInvertedMaj = 0
  var twoInvertedMaj = 0
  var threeInvertedMaj = 0
  var and = 0
  var noInvertedAnd = 0
  var oneInvertedAnd = 0
  var twoInvertedAnd = 0
  var or = 0
  var noInvertedOr = 0
  var oneInvertedOr = 0
  var twoInvertedOr = 0
  
  var circuit: Set[parser.Gate] = HashSet()
  var inputSet: Set[parser.Input] = HashSet()
  var outputSet: Set[parser.Output] = HashSet()
  var nodeMap: Map[String, Node] = HashMap()
}
  
  