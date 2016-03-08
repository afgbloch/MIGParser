package parser

sealed abstract class Node(name:String, direction:Direction)
final case class Gate(name:String, direction:DirectionGate) extends Node(name, direction) {
  var tpe:GateType = Majority // FIXME
}
final case class Input(name:String) extends Node(name, InputDirection)

sealed abstract class GateType
final case object And extends GateType
final case object Majority extends GateType

sealed abstract class Direction
final case object InputDirection extends Direction

sealed abstract class DirectionGate extends Direction
final case object Output extends DirectionGate 
final case object Wire extends DirectionGate

