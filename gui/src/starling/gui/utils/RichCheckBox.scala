package starling.gui.utils

import swing.CheckBox
import java.awt.Color


object RichCheckBox {
  implicit def checkBoxToRichCheckBox(checkBox : CheckBox) = new RichCheckBox(checkBox)

  class RichCheckBox(checkBox : CheckBox) {
    def ifSelected[T](value : => T) = if (checkBox.selected) Some(value) else None
  }
}

object RichColour {
  implicit def colourToRichColour(colour:Color) = new RichColour(colour)

  class RichColour(colour0:Color) {
    def blend(colour:Color, ratio:Float) = {
      val inverseRatio = 1.0f - ratio
      val rgb1 = colour0.getColorComponents(null)
      val rgb2 = colour.getColorComponents(null)
      new Color(
        rgb1(0) * ratio + rgb2(0) * inverseRatio,
        rgb1(1) * ratio + rgb2(1) * inverseRatio,
        rgb1(2) * ratio + rgb2(2) * inverseRatio
      )
    }
  }
}