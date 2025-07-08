package fr.esgi.al.funprog.ledpanel.parser

import fr.esgi.al.funprog.ledpanel.models.*
import fr.esgi.al.funprog.ledpanel.errors.*

object InstructionParser {
  
  //des la premiere ligne on va parser les dimensions du panneau
  def parseDimensions(line: String): Either[ParseError, (Int, Int)] = {
    val trimmed = line.trim
    val parts = trimmed.split(" x ")
    
    if (parts.length != 2) {
      Left(InvalidDimensions(line))
    } else {
      val widthResult = parsePositiveInt(parts(0).trim)
      val heightResult = parsePositiveInt(parts(1).trim)
      
      (widthResult, heightResult) match {
        case (Some(width), Some(height)) => Right((width, height))
        case _ => Left(InvalidDimensions(line))
      }
    }
  }

  def parseInstruction(line: String): Either[ParseError, Instruction] = {
    val trimmed = line.trim
    if (trimmed.isEmpty) {
      Left(InvalidInstruction(line, "Empty line"))
    } else {
      val parts = trimmed.split(" \\| ")
      
      if (parts.length != 4) {
        Left(InvalidInstruction(line, "Must have 4 parts separated by |"))
      } else {
        for {
          time <- parseTime(parts(0).trim)
          action <- parseAction(parts(1).trim)
          color <- parseColor(parts(2).trim)
          zone <- parseZone(parts(3).trim)
          instruction <- Instruction.create(time, action, color, zone)
            .left.map(err => InvalidInstruction(line, err))
        } yield instruction
      }
    }
  }

  private def parseTime(timeStr: String): Either[ParseError, Int] = {
    parsePositiveInt(timeStr) match {
      case Some(time) => Right(time)
      case None => Left(InvalidTime(timeStr))
    }
  }

  private def parseAction(actionStr: String): Either[ParseError, Action] = {
    actionStr match {
      case "+" => Right(Action.Increment)
      case "-" => Right(Action.Decrement)
      case "%" => Right(Action.Switch)
      case _ => Left(InvalidAction(actionStr))
    }
  }

  private def parseColor(colorStr: String): Either[ParseError, Color] = {
    val parts = colorStr.split(",")
    
    if (parts.length != 3) {
      Left(InvalidColor(colorStr))
    } else {
      val rgbResults = parts.map(part => parseZeroOrOne(part.trim))
      
      if (rgbResults.forall(_.isDefined)) {
        val List(r, g, b) = rgbResults.map(_.get).toList
        Color.create(r, g, b).left.map(_ => InvalidColor(colorStr))
      } else {
        Left(InvalidColor(colorStr))
      }
    }
  }
  
  //posit simple ou rectangle
  private def parseZone(zoneStr: String): Either[ParseError, Zone] = {
    if (zoneStr.contains(" - ")) {
      parseRectangleZone(zoneStr)
    } else {
      parseSinglePosition(zoneStr)
    }
  }
  
  //position simple x et y
  private def parseSinglePosition(posStr: String): Either[ParseError, Zone] = {
    val parts = posStr.split(",")
    
    if (parts.length != 2) {
      Left(InvalidPosition(posStr))
    } else {
      val xResult = parseNonNegativeInt(parts(0).trim)
      val yResult = parseNonNegativeInt(parts(1).trim)
      
      (xResult, yResult) match {
        case (Some(x), Some(y)) => Right(SinglePosition(x, y))
        case _ => Left(InvalidPosition(posStr))
      }
    }
  }
  
  //zone rectangle x1 et y1 / x2,et y2
  private def parseRectangleZone(zoneStr: String): Either[ParseError, Zone] = {
    val parts = zoneStr.split(" - ")
    if (parts.length != 2) {
      Left(InvalidPosition(zoneStr))
    } else {
      for {
        pos1 <- parseSinglePosition(parts(0).trim).map {
          case SinglePosition(x, y) => (x, y)
          case _ => (0, 0)
        }
        pos2 <- parseSinglePosition(parts(1).trim).map {
          case SinglePosition(x, y) => (x, y)
          case _ => (0, 0) //impossible que sa arrive
        }
      } yield RectangleZone(pos1._1, pos1._2, pos2._1, pos2._2)
    }
  }
}
