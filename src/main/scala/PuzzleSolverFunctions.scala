import PuzzleReaderWriter.linesArrayOfArray

object PuzzleSolverFunctions {


  def char_if_valid(puzzle:Puzzle, row:Int, colum:Int, x:Int, y:Int, char:Char): Puzzle ={
    if(validpos(puzzle.puzzle,row,colum,x, y)){
      val newpuzzle = puzzle.setChar(row, colum, char)
      return newpuzzle
    }

    return puzzle
  }


  // returns true if the posistion (row,colum)
  // is a valid index
  def validpos(liste:List[List[Char]], row:Int,colum:Int,x:Int,y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(colum < 0 || colum > x - 1){
      return false
    }
    if(!isWhite(liste, row, colum)){
      return false
    }
    return true
  }


  // returns true if the pos
  // is a white tile
  def isWhite(puzzle_list:List[List[Char]], row:Int, colum:Int): Boolean ={
    if(puzzle_list(row)(colum) == '_'){
        return true
      }
    return false
  }



  // returns a new class, with greyboxes
  // around 0 numbered black tiles
  // found with the find_pos_zero function
  def greybox(puzzle:Puzzle, pos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val liste = puzzle.puzzle



    if(pos.nonEmpty){
      val row = pos.head._1
      val colom = pos.head._2

      val up = char_if_valid(puzzle,row - 1,colom,x,y,'G')
      val down = char_if_valid(up,row + 1,colom,x,y,'G')
      val left = char_if_valid(down ,row,colom - 1,x,y,'G')
      val right = char_if_valid(left,row,colom + 1,x,y,'G')
      return greybox(right,pos.drop(1))
    }
    return puzzle

  }

  // returns posistions of "0" numbered black tiles
  // in the form a list of tuples(row,colum)
  def find_pos_zero(puzzle:Puzzle, listofzeros:List[(Int,Int)], row:Int, colum:Int): List[(Int,Int)] ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val liste = puzzle.puzzle


    if (row > y - 1){
      return listofzeros
    }
    if(colum > x - 1){
      return find_pos_zero(puzzle,listofzeros,row +1, 0)
    }
    if(liste(row)(colum) == '0'){
      val newlist:List[(Int,Int)] = listofzeros :+ (row,colum)
      return find_pos_zero(puzzle,newlist,row,colum + 1)
    }
    else{
      return find_pos_zero(puzzle,listofzeros,row,colum + 1)
    }
  }


  // OLD FUNCTIONS BELOW:
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
