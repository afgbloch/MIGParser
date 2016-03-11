package parser
 
sealed abstract class Node {
  val name:String
  var asap:Int = -1
  var alap:Int = -1
  def mobiity = alap - asap
  def setAsap(v:Int) {
    this.synchronized {
      asap = v
    }
  }
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