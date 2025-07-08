package fr.esgi.al.funprog.ledpanel.simulator

import scala.annotation.tailrec
import fr.esgi.al.funprog.ledpanel.models.*
import fr.esgi.al.funprog.ledpanel.errors.*

final case class SimulationState(
  panel: Panel,
  currentTime: Int,
  cumulativeTime: Map[(Int, Int), Int]
)

object LEDSimulator {
  
  //on execute tout et return l'etat final
  def simulate(panel: Panel, instructions: List[Instruction]): Either[ExecutionError, SimulationState] = {
    val initialState = SimulationState(panel, 0, Map.empty)
    val sortedInstructions = instructions.sortBy(_.time)
    executeInstructions(initialState, sortedInstructions)
  }
  //on exec une par une
  @tailrec
  private def executeInstructions(
    state: SimulationState, 
    instructions: List[Instruction]
  ): Either[ExecutionError, SimulationState] = {
    instructions match {
      case Nil => Right[ExecutionError, SimulationState](state)
      case instruction :: rest =>
        executeInstruction(state, instruction) match {
          case Right(newState) => executeInstructions(newState, rest)
          case Left(error) => Left[ExecutionError, SimulationState](error)
        }
    }
  }
  
  //on execute que une seul
  private def executeInstruction(
    state: SimulationState, 
    instruction: Instruction
  ): Either[ExecutionError, SimulationState] = {
    //si on avance pas dans le temps
    val updatedState = if (instruction.time > state.currentTime) {
      updateCumulativeTime(state, instruction.time)
    } else {
      state
    }
    
    //on verif si y'a pas de conflit
    val positions = instruction.getPositions
    val conflictCheck = checkForConflicts(updatedState, instruction, positions)
    
    conflictCheck match {
      case Some(error) => Left[ExecutionError, SimulationState](error)
      case None =>
        applyInstructionToPositions(updatedState, instruction, positions)
    }
  }

  private def updateCumulativeTime(state: SimulationState, newTime: Int): SimulationState = {
    val timeDiff = newTime - state.currentTime
    val updatedCumulative = state.panel.leds.foldLeft(state.cumulativeTime) {
      case (cumulative, ((x, y), led)) =>
        if (led.isOn) {
          val currentCumul = cumulative.getOrElse((x, y), 0)
          cumulative.updated((x, y), currentCumul + timeDiff)
        } else {
          cumulative
        }
    }
    state.copy(currentTime = newTime, cumulativeTime = updatedCumulative)
  }

  private def checkForConflicts(
    state: SimulationState,
    instruction: Instruction,
    positions: List[(Int, Int)]
  ): Option[ExecutionError] = {
    None
  }

  private def applyInstructionToPositions(
    state: SimulationState,
    instruction: Instruction,
    positions: List[(Int, Int)]
  ): Either[ExecutionError, SimulationState] = {
    
    val initialValue: Either[ExecutionError, SimulationState] = Right[ExecutionError, SimulationState](state)
    positions.foldLeft(initialValue) {
      case (Right(currentState), (x, y)) =>
        applyInstructionToPosition(currentState, instruction, x, y)
      case (left @ Left(_), _) => left
    }
  }

  private def applyInstructionToPosition(
    state: SimulationState,
    instruction: Instruction,
    x: Int,
    y: Int
  ): Either[ExecutionError, SimulationState] = {
    
    if (!state.panel.isValidPosition(x, y)) {
      Left[ExecutionError, SimulationState](PositionOutOfBounds(x, y, state.panel.width, state.panel.height))
    } else {
      state.panel.getLED(x, y) match {
        case None => 
          //led existe pas alors on crée
          LED.create(x, y, instruction.color, 0.0) match {
            case Right(newLED) =>
              applyAction(newLED, instruction.action, instruction.color) match {
                case Right(updatedLED) =>
                  state.panel.updateLED(updatedLED) match {
                    case Right(updatedPanel) => Right[ExecutionError, SimulationState](state.copy(panel = updatedPanel))
                    case Left(_) => Left[ExecutionError, SimulationState](PositionOutOfBounds(x, y, state.panel.width, state.panel.height))
                  }
                case Left(error) => Left[ExecutionError, SimulationState](error)
              }
            case Left(_) => 
              val actionName = getActionName(instruction.action)
              Left[ExecutionError, SimulationState](InvalidIntensityChange(x, y, 0.0, actionName))
          }
          
        case Some(currentLED) =>
          applyAction(currentLED, instruction.action, instruction.color) match {
            case Right(updatedLED) =>
              state.panel.updateLED(updatedLED) match {
                case Right(updatedPanel) => Right[ExecutionError, SimulationState](state.copy(panel = updatedPanel))
                case Left(_) => Left[ExecutionError, SimulationState](PositionOutOfBounds(x, y, state.panel.width, state.panel.height))
              }
            case Left(error) => Left[ExecutionError, SimulationState](error)
          }
      }
    }
  }

  private def getActionName(action: Action): String = {
    action match {
      case Action.Increment => "increment"
      case Action.Decrement => "decrement"
      case Action.Switch => "switch"
    }
  }

  private def applyAction(
    led: LED, 
    action: Action, 
    newColor: Color
  ): Either[ExecutionError, LED] = {
    
    action match {
      case Action.Increment =>
        if (led.intensity >= 1.0) {
          Right[ExecutionError, LED](led)
        } else {
          val colorResult: Either[ExecutionError, LED] = if (led.isOff) {
            led.withColor(newColor) match {
              case Right(coloredLED) => Right[ExecutionError, LED](coloredLED)
              case Left(_) => Right[ExecutionError, LED](led)
            }
          } else {
            Right[ExecutionError, LED](led)
          }
          
          colorResult match {
            case Right(coloredLED) =>
              coloredLED.withIntensity(1.0) match {
                case Right(finalLED) => Right[ExecutionError, LED](finalLED)
                case Left(_) => Left[ExecutionError, LED](InvalidIntensityChange(led.x, led.y, led.intensity, "increment"))
              }
            case Left(execError) => Left[ExecutionError, LED](execError)
          }
        }
        
      case Action.Decrement =>
        if (led.intensity <= 0.0) {
          Right[ExecutionError, LED](led)
        } else {
          led.withIntensity(0.0) match {
            case Right(newLed) => Right[ExecutionError, LED](newLed)
            case Left(_) => Left[ExecutionError, LED](InvalidIntensityChange(led.x, led.y, led.intensity, "decrement"))
          }
        }
        
      case Action.Switch =>
        val newIntensity = if (led.intensity == 0.0) 1.0 else 0.0
        val colorResult: Either[ExecutionError, LED] = if (newIntensity > 0.0 && led.isOff) {
          led.withColor(newColor) match {
            case Right(coloredLED) => Right[ExecutionError, LED](coloredLED)
            case Left(_) => Right[ExecutionError, LED](led)
          }
        } else {
          Right[ExecutionError, LED](led)
        }
        
        colorResult match {
          case Right(coloredLED) =>
            coloredLED.withIntensity(newIntensity) match {
              case Right(finalLED) => Right[ExecutionError, LED](finalLED)
              case Left(_) => Left[ExecutionError, LED](InvalidIntensityChange(led.x, led.y, led.intensity, "switch"))
            }
          case Left(execError) => Left[ExecutionError, LED](execError)
        }
    }
  }

  def generateSummary(state: SimulationState): String = {
    val activeLEDs = state.panel.totalActiveLEDs
    val colorCounts = state.panel.countByColor
    val totalCumulativeTime = state.cumulativeTime.values.sum
    
    s"""- allumées: $activeLEDs
    - couleurs:
    - rouge: ${colorCounts("rouge")}
    - blanc: ${colorCounts("blanc")}
    - vert: ${colorCounts("vert")}
    - bleu: ${colorCounts("bleu")}
    - cumul: $totalCumulativeTime"""
  }
}
