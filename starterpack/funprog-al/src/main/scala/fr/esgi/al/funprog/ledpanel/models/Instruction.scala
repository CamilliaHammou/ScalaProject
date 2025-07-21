package fr.esgi.al.funprog.ledpanel.models

//action sur l'intensité
enum Action {
  case Increment // +
  case Decrement // -
  case Switch // %
}

//zone d'application d'une instruction
sealed trait Zone
final case class SinglePosition(x: Int, y: Int) extends Zone
final case class RectangleZone(x1: Int, y1: Int, x2: Int, y2: Int)
    extends Zone {
  // on genere tout les pos dans la zone
  def positions: List[(Int, Int)] = {
    val minX = math.min(x1, x2)
    val maxX = math.max(x1, x2)
    val minY = math.min(y1, y2)
    val maxY = math.max(y1, y2)

    (for {
      x <- minX to maxX
      y <- minY to maxY
    } yield (x, y)).toList
  }
}

final case class Instruction private (
    time: Int,
    action: Action,
    color: Color,
    zone: Zone
) {
  def getPositions: List[(Int, Int)] = zone match {
    case SinglePosition(x, y) => List((x, y))
    case rect: RectangleZone  => rect.positions
  }
}

object Instruction {
  def create(
      time: Int,
      action: Action,
      color: Color,
      zone: Zone): Either[String, Instruction] = {
    if (time < 0) {
      Left[String, Instruction]("Time must be non-negative")
    } else {
      Right[String, Instruction](Instruction(time, action, color, zone))
    }
  }
}
