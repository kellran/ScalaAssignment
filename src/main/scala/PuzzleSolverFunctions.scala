import PuzzleReaderWriter.{lines, linesArrayOfArray}

object PuzzleSolverFunctions {

  // returns true if the posistion (row,column) of the list
  // is a lamp post, else false
  def check_if_lamppost_bool(puzzle_list:List[List[Char]], row:Int, column:Int): Boolean ={
    // Left
    if(puzzle_list(row)(column) == '*'){
      return true
    }
    else false
  }

  // returns 1 if the posistion (row,column) of the list
  // is a lamp post, else 0
  def check_if_lamppost_int(puzzle: Puzzle, row:Int, column:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzle_list = puzzle.puzzle
    if(validpos(row ,column ,x ,y)){
      if(puzzle_list(row)(column) == '*'){
        return 1
      }
      else 0
    }
    else 0
  }

  // returns the number of lamppost
  // adjecent to tile, given a posistion(row,column).
  def sum_of_lamps(puzzle: Puzzle, row:Int, column:Int): Int ={

    val up = check_if_lamppost_int(puzzle,row - 1,column)
    val down = check_if_lamppost_int(puzzle,row + 1,column)
    val left = check_if_lamppost_int(puzzle,row,column - 1)
    val right = check_if_lamppost_int(puzzle,row,column + 1)

    val sum = up + down + left + right
    return sum
  }

  def all_number_pos(puzzle: Puzzle): List[(Int,Int)] ={
    val zero: List[(Int,Int)] = find_pos_of_char(puzzle,List(),0,0,'0')
    val one: List[(Int,Int)] = zero ++ find_pos_of_char(puzzle,List(),0,0,'1')
    val two: List[(Int,Int)] = one ++ find_pos_of_char(puzzle,List(),0,0,'2')
    val three: List[(Int,Int)] = two ++ find_pos_of_char(puzzle,List(),0,0,'3')
    val four: List[(Int,Int)] = three ++ find_pos_of_char(puzzle,List(),0,0,'4')

    return four: List[(Int,Int)]
  }

  def update_number(puzzle: Puzzle ,savedstate:Puzzle ,numbers:List[(Int,Int)]): Puzzle ={
    val row = numbers.head._1
    val column = numbers.head._2

    val sum = sum_of_lamps(puzzle,row,column)
    val newpuzzle = puzzle.setInt(savedstate, row,column,sum)
    return newpuzzle
  }

  // takes in positions of numbers and updates them
  // with the update_number function.
  def update_numbers(puzzle: Puzzle,savedstate:Puzzle, numbers:List[(Int,Int)]): Puzzle ={
    if(numbers.nonEmpty){
      println(numbers)

      val update = update_number(puzzle,savedstate,numbers)
      return update_numbers(update,savedstate,numbers.drop(1))
    }
    return puzzle
  }

  // returns a new class, with greyboxes
  // around 0 numbered black tiles
  // found with the find_pos_zero function
  def place_landlocked(puzzle:Puzzle, pos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzlelist = puzzle.puzzle

    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val lamp = char_if_valid(puzzle,row,column,x,y,'*')
      return place_landlocked(lamp,pos.drop(1))
    }
    return puzzle
  }

  def char_if_valid(puzzle:Puzzle, row:Int, column:Int, x:Int, y:Int, char:Char): Puzzle ={
    if(validpos_white(puzzle.puzzle,row,column,x, y)){
      val newpuzzle = puzzle.setChar(row, column, char)
      return newpuzzle
    }

    return puzzle
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
      val column = pos.head._2

      val up = char_if_valid(puzzle,row - 1,column,x,y,'G')
      val down = char_if_valid(up,row + 1,column,x,y,'G')
      val left = char_if_valid(down ,row,column - 1,x,y,'G')
      val right = char_if_valid(left,row,column + 1,x,y,'G')
      return greybox(right,pos.drop(1))
    }
    return puzzle
  }

  // Checks each index in a 2d list
  // and checks if each adjecent tile is black.
  // returns a list of all 'landlocked tiles'
  def find_landlocked(puzzle:Puzzle, list_of_landlocked:List[(Int,Int)], row:Int, column:Int): List[(Int,Int)] ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val liste = puzzle.puzzle


    if (row > y - 1){
      return list_of_landlocked
    }
    if(column > x - 1){
      return find_landlocked(puzzle,list_of_landlocked,row +1, 0)
    }
    if(check_landlocked(liste,row - 1,column,x, y)) {
      if (check_landlocked(liste, row + 1, column, x, y)) {
        if (check_landlocked(liste, row, column - 1, x, y)) {
          if (check_landlocked(liste, row, column + 1, x, y)) {
            val newlist:List[(Int,Int)] = list_of_landlocked :+ (row,column)
            return find_landlocked(puzzle,newlist,row,column + 1)
          }
        }
      }
    }
    return find_landlocked(puzzle,list_of_landlocked,row,column + 1)
  }

  // returns posistions of given char
  // in the form a list of tuples(row,column)
  def find_pos_of_char(puzzle:Puzzle, listofzeros:List[(Int,Int)], row:Int, column:Int, char: Char): List[(Int,Int)] ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val liste = puzzle.puzzle

    if (row > y - 1){
      return listofzeros
    }
    if(column > x - 1){
      return find_pos_of_char(puzzle,listofzeros,row +1, 0,char)
    }
    if(liste(row)(column) == char){
      val newlist:List[(Int,Int)] = listofzeros :+ (row,column)
      return find_pos_of_char(puzzle,newlist,row,column + 1,char)
    }
    else{
      return find_pos_of_char(puzzle,listofzeros,row,column + 1,char)
    }
  }

  // returns true if the posistion (row,column) of the list
  // is either a non valid index or a black tile
  def check_landlocked(liste:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return true
    }
    if(column < 0 || column > x - 1){
      return true
    }
    if(isBlack(liste, row, column)){
      return true
    }
    return false
  }

  // returns true if the posistion (row,column)
  // is a valid index & not a white tile
  def validpos_white(liste:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    if(!isWhite(liste, row, column)){
      return false
    }
    return true
  }

  // returns true if the posistion (row,column)
  // is a valid index
  def validpos(row:Int,column:Int,x:Int,y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    return true
  }

  // returns true if the pos
  // is a white tile
  def isWhite(puzzle_list:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzle_list(row)(column) == '_'){
      return true
    }
    return false
  }

  // function takes in list, and posistion(row,column)
  // and returns true if it's black
  // false if not
  def isBlack(puzzle_list:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzle_list(row)(column) == '0'){
      return true
    }
    if(puzzle_list(row)(column) == '1'){
      return true
    }
    if(puzzle_list(row)(column) == '2'){
      return true
    }
    if(puzzle_list(row)(column) == '3'){
      return true
    }
    if(puzzle_list(row)(column) == '4'){
      return true
    }
    if(puzzle_list(row)(column) == 'X'){
      return true
    }
    return false
  }

  // OLD FUNCTIONS BELOW:
  /*
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
   */

}
