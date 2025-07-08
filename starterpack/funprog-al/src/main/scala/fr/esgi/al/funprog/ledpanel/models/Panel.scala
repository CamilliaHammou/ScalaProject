package fr.esgi.al.funprog.ledpanel.models

//classe rectangle
case class Panel(width: Int, height: Int, leds: Map[(Int, Int), LED] = Map.empty) {
  require(width > 0, "Width must be positive")
  require(height > 0, "Height must be positive")
  
  //ici on verif si une position est valide
  def isValidPosition(x: Int, y: Int): Boolean = 
    x >= 0 && x < height && y >= 0 && y < width
  
  //on recup une led a une pos donnée
  def getLED(x: Int, y: Int): Option[LED] = 
    if (isValidPosition(x, y)) {
      leds.get((x, y))
    } else {
      None
    }
  
  //uptade la led
  def updateLED(led: LED): Either[String, Panel] = {
    if (isValidPosition(led.x, led.y)) {
      Right(copy(leds = leds.updated((led.x, led.y), led)))
    } else {
      Left(s"Position (${led.x}, ${led.y}) is out of bounds")
    }
  }
  
  //on recup tout les led aluumé
  def getActiveLEDs: List[LED] = 
    leds.values.filter(_.isOn).toList
  
  //on compte les leds par couleur
  def countByColor: Map[String, Int] = {
    val activeLEDs = getActiveLEDs
    Map(
      "rouge" -> activeLEDs.count(_.color == Color.RED),
      "vert" -> activeLEDs.count(_.color == Color.GREEN),
      "bleu" -> activeLEDs.count(_.color == Color.BLUE),
      "blanc" -> activeLEDs.count(_.color == Color.WHITE)
    )
  }
  
  //on compte le nombre tototal de leds allumé
  def totalActiveLEDs: Int = getActiveLEDs.length
}

object Panel {
  def apply(width: Int, height: Int): Panel = {
    val initialLEDs = (for {
      x <- 0 until height
      y <- 0 until width
    } yield (x, y) -> LED(x, y)).toMap
    
    Panel(width, height, initialLEDs)
  }
}
