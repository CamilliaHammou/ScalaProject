package fr.esgi.al.funprog.ledpanel.errors

sealed trait SimulationError {
  def message: String
}

sealed trait ParseError extends SimulationError
final case class InvalidDimensions(input: String) extends ParseError {
  def message: String = s"Invalid panel dimensions: '$input'"
}
final case class InvalidInstruction(line: String, reason: String) extends ParseError {
  def message: String = s"Invalid instruction '$line': $reason"
}
final case class InvalidTime(timeStr: String) extends ParseError {
  def message: String = s"Invalid time value: '$timeStr'"
}
final case class InvalidAction(actionStr: String) extends ParseError {
  def message: String = s"Invalid action: '$actionStr'. Must be +, -, or %"
}
final case class InvalidColor(colorStr: String) extends ParseError {
  def message: String = s"Invalid color: '$colorStr'. Must be r,g,b with values 0 or 1"
}
final case class InvalidPosition(posStr: String) extends ParseError {
  def message: String = s"Invalid position: '$posStr'"
}

sealed trait ExecutionError extends SimulationError
final case class PositionOutOfBounds(x: Int, y: Int, width: Int, height: Int) extends ExecutionError {
  def message: String = s"Position ($x, $y) is out of bounds for panel ${width}x${height}"
}
final case class MultipleInstructionsAtSameTime(time: Int, x: Int, y: Int) extends ExecutionError {
  def message: String = s"Multiple instructions for LED ($x, $y) at time $time"
}
final case class ColorChangeOnActiveLED(x: Int, y: Int) extends ExecutionError {
  def message: String = s"Cannot change color of active LED at ($x, $y)"
}
final case class InvalidIntensityChange(x: Int, y: Int, currentIntensity: Double, action: String) extends ExecutionError {
  def message: String = s"Invalid intensity change for LED ($x, $y): current=$currentIntensity, action=$action"
}
final case class SimultaneousColorAndIntensityChange(x: Int, y: Int) extends ExecutionError {
  def message: String = s"Cannot change color and intensity simultaneously for LED ($x, $y)"
}

sealed trait FileError extends SimulationError
final case class FileNotFound(path: String) extends FileError {
  def message: String = s"File not found: $path"
}
final case class FileReadError(path: String, reason: String) extends FileError {
  def message: String = s"Error reading file '$path': $reason"
}
