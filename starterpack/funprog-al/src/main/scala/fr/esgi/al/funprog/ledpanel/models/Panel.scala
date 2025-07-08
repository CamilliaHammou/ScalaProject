package fr.esgi.al.funprog.ledpanel.models

//ici on repr un panneau led  rectangulaire
case class Panel private(width: Int, height: Int, leds: Map[(Int, Int), LED]) {
  
  //verif si une pos est valide
  def isValidPosition(x: Int, y: Int): Boolean = 
    x >= 0 && x < height && y >= 0 && y < width
  
  //recup une led a un pos deja donnée
  def getLED(x: Int, y: Int): Option[LED] = 
    if (isValidPosition(x, y)) {
      leds.get((x, y))
    } else {
      None
    }
  
  //update les led
  def updateLED(led: LED): Either[String, Panel] = {
    if (isValidPosition(led.x, led.y)) {
      Right(copy(leds = leds.updated((led.x, led.y), led)))
    } else {
      Left(s"Position (${led.x}, ${led.y}) is out of bounds")
    }
  }
  
  //recup toute les led alumé
  def getActiveLEDs: List[LED] = 
    leds.values.filter(_.isOn).toList
  
  //compte les led par couleur
  def countByColor: Map[String, Int] = {
    val activeLEDs = getActiveLEDs
    Map(
      "rouge" -> activeLEDs.count(_.color == Color.RED),
      "vert" -> activeLEDs.count(_.color == Color.GREEN),
      "bleu" -> activeLEDs.count(_.color == Color.BLUE),
      "blanc" -> activeLEDs.count(_.color == Color.WHITE)
    )
  }
  
  //nbr total de led allumé
  def totalActiveLEDs: Int = getActiveLEDs.length
}

object Panel {
  def create(width: Int, height: Int): Either[String, Panel] = {
    if (width <= 0) {
      Left("Width must be positive")
    } else if (height <= 0) {
      Left("Height must be positive")
    } else {
      val ledsResult = (for {
        x <- 0 until height
        y <- 0 until width
      } yield {
        LED.create(x, y).map(led => (x, y) -> led)
      }).toList
      
      //on verif si erreur lors de la creation
      val errors = ledsResult.collect { case Left(error) => error }
      if (errors.nonEmpty) {
        Left(s"Error creating LEDs: ${errors.head}")
      } else {
        val leds = ledsResult.collect { case Right(ledPair) => ledPair }.toMap
        Right(Panel(width, height, leds))
      }
    }
  }
}
