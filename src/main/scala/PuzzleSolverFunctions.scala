object PuzzleSolverFunctions {

  def placeLightBulb(x:Any):Unit={
    //Simply places the lightbulb
    //test
  }

  def greyBox(x: Int):Boolean={
    if (x == 0){return true}
    else return false
  }

  def landLocked(x: Int, y: String):Boolean={
    //Check if array-boxes around the chosen tile is only made up of either Ints (Boxes with numbers)
    //or Xs (Black boxes without numbers). If this is true, then return true. Else return false
    return null
  }

  def updateNumber(x: Any):Int={
    //Will run whenever a lightbulb is placed, lowering the value of numbers around it, pontetially
    //reducing them to 0 and thus generating more grey boxes
    return null
  }

  def implicitLandlocked(x: Any):Boolean={
    //Will check if a numbered block only has 1 combination of lightbulb placements, then place the lightbubs accordingly.
    return null
  }

  def shineLight(x: Boolean):String={
    //This will shine light from the lightbulbs that have been placed, the light overwrites grey boxes and empty tiles,
    //but stops at black boxes or numbered boxes.
    return null
  }

}
