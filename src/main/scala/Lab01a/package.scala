package Lab01a

import Lab01a.TrainerSolution.{Centimetre, Kilometre}

package object lab1a {
  def convertCentimetersToKilometre(cms: Centimetre) : Kilometre = {
    Kilometre(cms.value / cms.conversion)
  }
}