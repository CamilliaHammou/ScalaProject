package fr.esgi.al.funprog.ledpanel.models

//ici  on représente une couleur LED avec les composantes RGB (0 ou 1 chacune)
case class Color private(red: Int, green: Int, blue: Int) {
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
  
  def create(red: Int, green: Int, blue: Int): Either[String, Color] = {
    if (red != 0 && red != 1) {
      Left("Red component must be 0 or 1")
    } else if (green != 0 && green != 1) {
      Left("Green component must be 0 or 1")
    } else if (blue != 0 && blue != 1) {
      Left("Blue component must be 0 or 1")
    } else {
      Right(Color(red, green, blue))
    }
  }
}

// LED avec sa position, couleur et intensité
case class LED private(x: Int, y: Int, color: Color, intensity: Double) {
  def isOn: Boolean = intensity > 0.0
  def isOff: Boolean = intensity == 0.0
  
  //modif l'intensité
  def withIntensity(newIntensity: Double): Either[String, LED] = {
    if (newIntensity < 0.0 || newIntensity > 1.0) {
      Left("Intensity must be between 0 and 1")
    } else {
      Right(copy(intensity = newIntensity))
    }
  }
  
  //change la couleur seulement si la led est pas allumé
  def withColor(newColor: Color): Either[String, LED] = {
    if (isOn && color != newColor) {
      Left("Cannot change color of an LED that is on")
    } else {
      Right(copy(color = newColor))
    }
  }
}

object LED {
  def create(x: Int, y: Int, color: Color = Color.BLACK, intensity: Double = 0.0): Either[String, LED] = {
    if (intensity < 0.0 || intensity > 1.0) {
      Left("Intensity must be between 0 and 1")
    } else {
      Right(LED(x, y, color, intensity))
    }
  }
}
