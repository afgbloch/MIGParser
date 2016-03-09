package parser
 
sealed abstract class Node {
  val name:String
}
final case class Gate(name:String) extends Node {
  var tpe:GateType = None 
  var inputs:List[GateInput] = Nil
  var outputs:List[Node] = Nil
}

final case class Input(name:String) extends Node{
  var outputs:List[Node] = Nil
}

final case class Output(name:String) extends Node{
  var input:GateInput = null
}

final case object One extends Node{
  val name:String = "one";
  var outputs:List[Node] = Nil
}

final case class Assign(name:String, tpe:GateType, inputs:List[(Boolean, String)]) extends Node


sealed abstract class GateType
final case object And extends GateType
final case object Or extends GateType
final case object Majority extends GateType
final case object None extends GateType

sealed case class GateInput(inverted:Boolean, node:Node)