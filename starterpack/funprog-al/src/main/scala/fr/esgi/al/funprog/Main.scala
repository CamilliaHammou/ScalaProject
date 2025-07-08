package fr.esgi.al.funprog

import better.files.*
import scala.util.{Try, Success, Failure}
import scala.language.unsafeNulls
import fr.esgi.al.funprog.ledpanel.models.*
import fr.esgi.al.funprog.ledpanel.parser.InstructionParser
import fr.esgi.al.funprog.ledpanel.simulator.{LEDSimulator, SimulationState}
import fr.esgi.al.funprog.ledpanel.errors.*

@main
def Main(): Unit = {
  val inputPath = "input/example.txt"
  //val inputPath = "input/test_simple.txt"
  //val inputPath = "input/test_colors.txt"
  //val inputPath = "input/test_rectangle.txt"
  //val inputPath = "input/test_switch.txt"
  
  val result = for {
    lines <- readFile(inputPath)
    parsedData <- parseFile(lines)
    (dimensions, instructions) = parsedData
    panel <- createPanel(dimensions._1, dimensions._2)
    finalState <- runSimulation(panel, instructions)
  } yield finalState
  
  result match {
    case Right(state) =>
      val summary = LEDSimulator.generateSummary(state)
      println(summary)
    case Left(error) =>
      println(s"Erreur: ${error.message}")
      System.exit(1)
  }
}

def readFile(path: String): Either[SimulationError, List[String]] = {
  val tryResult = Try {
    val file = File(path)
    if (!file.exists) {
      Left[SimulationError, List[String]](FileNotFound(path))
    } else {
      Right[SimulationError, List[String]](file.lines.toList)
    }
  }
  tryResult match {
    case Success(result) => result
    case Failure(_) => Left[SimulationError, List[String]](FileReadError(path, "Error reading file"))
  }
}
def parseFile(lines: List[String]): Either[SimulationError, ((Int, Int), List[Instruction])] = {
  lines match {
    case Nil => Left[SimulationError, ((Int, Int), List[Instruction])](InvalidDimensions("Empty file"))
    case dimensionLine :: instructionLines =>
      for {
        dimensions <- InstructionParser.parseDimensions(dimensionLine)
        instructions <- parseInstructions(instructionLines)
      } yield (dimensions, instructions)
  }
}

def parseInstructions(lines: List[String]): Either[SimulationError, List[Instruction]] = {
  val nonEmptyLines = lines.filter(line => line.trim.nonEmpty)
  
  def processLine(line: String, acc: Either[SimulationError, List[Instruction]]): Either[SimulationError, List[Instruction]] = {
    acc match {
      case Right(accList) =>
        InstructionParser.parseInstruction(line) match {
          case Right(instruction) => Right[SimulationError, List[Instruction]](instruction :: accList)
          case Left(error) => Left[SimulationError, List[Instruction]](error)
        }
      case left @ Left(_) => left
    }
  }
  val initialAcc = Right[SimulationError, List[Instruction]](List.empty[Instruction])
  nonEmptyLines.foldRight(initialAcc)(processLine)
}

def createPanel(width: Int, height: Int): Either[SimulationError, Panel] = {
  Panel.create(width, height) match {
    case Right(panel) => Right[SimulationError, Panel](panel)
    case Left(err) => Left[SimulationError, Panel](InvalidDimensions(s"$width x $height: $err"))
  }
}
def runSimulation(panel: Panel, instructions: List[Instruction]): Either[SimulationError, SimulationState] = {
  LEDSimulator.simulate(panel, instructions) match {
    case Right(state) => Right[SimulationError, SimulationState](state)
    case Left(execError) => Left[SimulationError, SimulationState](execError)
  }
}
