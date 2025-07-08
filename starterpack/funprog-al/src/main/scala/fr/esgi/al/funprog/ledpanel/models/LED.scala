package fr.esgi.al.funprog.ledpanel.models

//ici  on représente une couleur LED avec les composantes RGB (0 ou 1 chacune)
final case class Color private(red: Int, green: Int, blue: Int) {
  def isOff: Boolean = red == 0 && green == 0 && blue == 0
  def isOn: Boolean = !isOff
  
  def colorName: String = this match {
    case Color(0, 0, 0) => "noir"
    case Color(1, 0, 0) => "rouge"
    case Color(0, 1, 0) => "vert"
    case Color(0, 0, 1) => "bleu"
    case Color(1, 1, 1) => "blanc"
    case _ => "inconnu"
  }
  
  def isSameAs(other: Color): Boolean = {
    red == other.red && green == other.green && blue == other.blue
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
      Left[String, Color]("Red component must be 0 or 1")
    } else if (green != 0 && green != 1) {
      Left[String, Color]("Green component must be 0 or 1")
    } else if (blue != 0 && blue != 1) {
      Left[String, Color]("Blue component must be 0 or 1")
    } else {
      Right[String, Color](Color(red, green, blue))
    }
  }
}

//lED avec sa position, couleur et intensité
final case class LED private(x: Int, y: Int, color: Color, intensity: Double) {
  def isOn: Boolean = intensity > 0.0
  def isOff: Boolean = intensity == 0.0
  
  //modif l'intensité
  def withIntensity(newIntensity: Double): Either[String, LED] = {
    if (newIntensity < 0.0 || newIntensity > 1.0) {
      Left[String, LED]("Intensity must be between 0 and 1")
    } else {
      Right[String, LED](copy(intensity = newIntensity))
    }
  }
  
  //change la couleur seulement si la led est pas allumé
  def withColor(newColor: Color): Either[String, LED] = {
    val colorsDifferent = !color.isSameAs(newColor)
    if (isOn && colorsDifferent) {
      Left[String, LED]("Cannot change color of an LED that is on")
    } else {
      Right[String, LED](copy(color = newColor))
    }
  }
}
object LED {
  def createBasic(x: Int, y: Int): Either[String, LED] = {
    Right[String, LED](LED(x, y, Color.BLACK, 0.0))
  }
  
  def createWithColor(x: Int, y: Int, color: Color): Either[String, LED] = {
    Right[String, LED](LED(x, y, color, 0.0))
  }
  
  def create(x: Int, y: Int, color: Color, intensity: Double): Either[String, LED] = {
    if (intensity < 0.0 || intensity > 1.0) {
      Left[String, LED]("Intensity must be between 0 and 1")
    } else {
      Right[String, LED](LED(x, y, color, intensity))
    }
  }
}
