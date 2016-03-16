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
  def maj_% = maj.toFloat / circuit.size * 100
  
  var noInvertedMaj = 0
  def noInvertedMaj_% = noInvertedMaj.toFloat / maj * 100
  var oneInvertedMaj = 0
  def oneInvertedMaj_% = oneInvertedMaj.toFloat / maj * 100
  var twoInvertedMaj = 0
  def twoInvertedMaj_% = twoInvertedMaj.toFloat / maj * 100
  var threeInvertedMaj = 0
  def threeInvertedMaj_% = threeInvertedMaj.toFloat / maj * 100
  
  var noMajInput = 0
  def noMajInput_% = noMajInput.toFloat / maj * 100
  var oneMajInput = 0
  def oneMajInput_% = oneMajInput.toFloat / maj * 100
  var twoMajInput = 0
  def twoMajInput_% = twoMajInput.toFloat / maj * 100
  var threeMajInput = 0
  def threeMajInput_% = threeMajInput.toFloat / maj * 100
  
  var and = 0
  def and_% = and.toFloat / circuit.size * 100
  var noInvertedAnd = 0
  def noInvertedAnd_% = noInvertedAnd.toFloat / and * 100
  var oneInvertedAnd = 0
  def oneInvertedAnd_% = oneInvertedAnd.toFloat / and * 100
  var twoInvertedAnd = 0
  def twoInvertedAnd_% = twoInvertedAnd.toFloat / and * 100
  
  var or = 0
  def or_% = or.toFloat / circuit.size * 100
  var noInvertedOr = 0
  def noInvertedOr_% = noInvertedOr.toFloat / or * 100
  var oneInvertedOr = 0
  def oneInvertedOr_% = oneInvertedOr.toFloat / or * 100
  var twoInvertedOr = 0
  def twoInvertedOr_% = twoInvertedOr.toFloat / or * 100
  
  var cp = -1
  var cpGateNb = 0
  var cpMajNb = 0
  def cpMaj_% = cpMajNb.toFloat / cpGateNb * 100
  
  var totalFanout:Map[Int, Int] = HashMap() 
  def totalFanout_%(i:Int) = totalFanout(i).toFloat / circuit.size * 100  
  var majFanout:Map[Int, Int] = HashMap() 
  def majFanout_%(i:Int) = majFanout(i).toFloat / maj * 100
  
  var circuit: Set[parser.Gate] = HashSet()
  var inputSet: Set[parser.Input] = HashSet()
  var outputSet: Set[parser.Output] = HashSet()
  var nodeMap: Map[String, Node] = HashMap()
  
  def toCSV : String = ???

}
  
  