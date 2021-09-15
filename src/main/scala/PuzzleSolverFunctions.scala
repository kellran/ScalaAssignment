import PuzzleReaderWriter.linesArrayOfArray

object PuzzleSolverFunctions {

  //Simply places the lightbulb
  def placeLightBulb(x:Int, y: Int):Boolean={
    if (linesArrayOfArray(x)(y) != '_'){
      return false
    }
    else {
      linesArrayOfArray(x)(y) = '*'
      return true
    }
  }

  //Simply places the grey boxes
  def placeGreyBox(x: Int, y:Int):Boolean={
    if (linesArrayOfArray(x)(y) != '_'){
      return false
    }
    else {
      linesArrayOfArray(x)(y) = 'G'
      return true
    }
  }

  //Check if array-boxes around the chosen tile is only made up of either Ints (Boxes with numbers)
  //or Xs (Black boxes without numbers). If this is true, then return true. Else return false
  def landLocked(x: Int, y: String):Boolean={
    return true
  }

  //Will run whenever a lightbulb is placed, lowering the value of numbers around it, pontetially
  //reducing them to 0 and thus generating more grey boxes
  def updateNumber(x: Any):Int={
    return 1
  }

  //Will check if a numbered block only has 1 combination of lightbulb placements, then place the lightbubs accordingly.
  def implicitLandlocked(x: Any):Boolean={
    return true
  }

  //This will shine light from the lightbulbs that have been placed, the light overwrites grey boxes and empty tiles,
  //but stops at black boxes or numbered boxes.
  def shineLight(x: Boolean):String={
    return "cbt"
  }

}
