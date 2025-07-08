package fr.esgi.al.funprog.ledpanel.models

// Représente une couleur LED avec les composantes RGB (0 ou 1 chacune)
case class Color(red: Int, green: Int, blue: Int) {
  require(red == 0 || red == 1, "Red component must be 0 or 1")
  require(green == 0 || green == 1, "Green component must be 0 or 1")
  require(blue == 0 || blue == 1, "Blue component must be 0 or 1")
  
  def isOff: Boolean = red == 0 && green == 0 && blue == 0
  def isOn: Boolean = !isOff
  
  def colorName: String = this match {
    case Color(0, 0, 0) => "noir"
    case Color(1, 0, 0) => "rouge"
    case Color(0, 1, 0) => "vert"
    case Color(0, 0, 1) => "bleu"
    case Color(1, 1, 1) => "blanc"
  }
}

object Color {
  val BLACK = Color(0, 0, 0)
  val RED = Color(1, 0, 0)
  val GREEN = Color(0, 1, 0)
  val BLUE = Color(0, 0, 1)
  val WHITE = Color(1, 1, 1)
}

// Représente une LED avec sa position, couleur et intensité
case class LED(x: Int, y: Int, color: Color = Color.BLACK, intensity: Double = 0.0) {
  require(intensity >= 0.0 && intensity <= 1.0, "Intensity must be between 0 and 1")
  
  def isOn: Boolean = intensity > 0.0
  def isOff: Boolean = intensity == 0.0
  
  // Change l'intensité
  def withIntensity(newIntensity: Double): LED = {
    require(newIntensity >= 0.0 && newIntensity <= 1.0, "Intensity must be between 0 and 1")
    copy(intensity = newIntensity)
  }
  
  // Change la couleur (seulement si la LED est éteinte)
  def withColor(newColor: Color): Either[String, LED] = {
    if (isOn && color != newColor) {
      Left("Cannot change color of an LED that is on")
    } else {
      Right(copy(color = newColor))
    }
  }
}
