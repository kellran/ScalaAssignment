object PuzzleSolverFunctions {

  def find_implicit_grey(puzzle:Puzzle,greys:List[(Int,Int)], implicit_grey:List[(Int,Int)]): List[(Int,Int)] ={

    if(greys.nonEmpty){
      val row = greys.head._1
      val column = greys.head._2

      val up = count_char_until_black(puzzle,row,column,'U','_',0)
      val down = count_char_until_black(puzzle,row,column,'D','_',0)
      val left = count_char_until_black(puzzle,row,column,'L','_',0)
      val right = count_char_until_black(puzzle,row,column,'R','_',0)

      val sum = up + down + left + right

      if(sum == 1){
        val newlist:List[(Int,Int)] = implicit_grey :+ (row,column)
        return find_implicit_grey(puzzle ,greys.drop(1) ,newlist)
      }
      else find_implicit_grey(puzzle ,greys.drop(1) ,implicit_grey)
    }
    else return implicit_grey
  }


  def count_char_until_black(puzzle: Puzzle, row:Int, column:Int, direction:Char, char: Char, white_count:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeX
    val puzzlelist = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validpos(row - 1, column, x , y) && (!isBlack(puzzlelist,row - 1,column))){
          if(puzzlelist(row - 1)(column) == char){
            val white_count_update = white_count + 1
            return count_char_until_black(puzzle,row - 1, column, 'U', char, white_count_update)
          }
          return count_char_until_black(puzzle,row - 1, column,'U', char, white_count)
        }
      }
      case 'D' => {
        if(validpos(row + 1, column, x , y) && (!isBlack(puzzlelist,row + 1,column))){
          if(puzzlelist(row + 1)(column) == char){
            val white_count_update = white_count + 1
            return count_char_until_black(puzzle,row + 1, column, 'D', char, white_count_update)
          }
          return count_char_until_black(puzzle,row + 1, column, 'D',char, white_count)
        }
      }
      case 'L' => {
        if(validpos(row, column - 1, x , y) && (!isBlack(puzzlelist,row,column - 1))){
          if(puzzlelist(row)(column - 1) == char){
            val white_count_update = white_count + 1
            return count_char_until_black(puzzle,row, column - 1, 'L',char, white_count_update)
          }
          return count_char_until_black(puzzle,row, column - 1, 'L', char, white_count)
        }
      }
      case 'R' => {
        if(validpos(row, column + 1, x , y) && (!isBlack(puzzlelist,row,column + 1))){
          if(puzzlelist(row)(column + 1) == char){
            val white_count_update = white_count + 1
            return count_char_until_black(puzzle,row, column + 1, 'R', char, white_count_update)
          }
          return count_char_until_black(puzzle,row, column + 1, 'R', char, white_count)
        }
      }
    }
    return white_count

  }

  def lights(puzzle: Puzzle, pos:List[(Int,Int)]): Puzzle ={
    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val up = light(puzzle,row,column, 'X', 'U')
      val down = light(up,row,column, 'X', 'D')
      val left = light(down,row,column, 'X', 'L')
      val right = light(left,row,column, 'X', 'R')

      return lights(right,pos.drop(1))
    }
    return puzzle
  }


  def light(puzzle: Puzzle, row:Int, column:Int, char: Char, direction:Char): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzlelist = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row - 1, column, char, direction)
        }
        else return puzzle
      }
      case 'D' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row + 1, column, char, direction)
        }
        else return puzzle
      }
      case 'L' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row, column - 1, char, direction)
        }
        else return puzzle
      }
      case 'R' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row, column + 1, char, direction)
        }
        else return puzzle
      }
      case _ => {
        return puzzle
      }
    }
  }

  def count_until_char(puzzle: Puzzle, row:Int, column:Int, char: Char, direction:Char, char_count:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzlelist = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validpos(row - 1, column, x , y) && puzzlelist(row - 1)(column) != char){
          val count_update = char_count + 1
          count_until_char(puzzle, row - 1, column, char, direction, count_update)
        }
        else return char_count
      }
      case 'D' => {
        if(validpos(row + 1, column, x , y) && puzzlelist(row + 1)(column) != char){
          val count_update = char_count + 1
          count_until_char(puzzle, row + 1, column, char, direction, count_update)
        }
        else return char_count
      }
      case 'L' => {
        if(validpos(row, column - 1, x , y) && puzzlelist(row)(column - 1) != char){
          val count_update = char_count + 1
          count_until_char(puzzle, row, column - 1, char, direction, count_update)
        }
        else return char_count
      }
      case 'R' => {
        if(validpos(row, column + 1, x , y) && puzzlelist(row)(column + 1) != char){
          val char_count_update = char_count + 1
          count_until_char(puzzle, row, column + 1, char, direction, char_count_update)
        }
        else return char_count
      }
      case _ => {
        return char_count
      }
    }
  }

  def place_implicit(puzzle:Puzzle, pos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val up = char_if_valid(puzzle,row - 1,column,x,y,'*')
      val down = char_if_valid(up,row + 1,column,x,y,'*')
      val left = char_if_valid(down,row,column - 1,x,y,'*')
      val right = char_if_valid(left,row,column + 1,x,y,'*')
      return place_implicit(right,pos.drop(1))
    }
    return puzzle
  }

  // takes the list of all numbers of the puzzle and checks each posistion.
  // If the number of '_' equals it's number, add the posistion to the return list.
  // returns a list of all implicit landlocked tiles.
  def find_implicit_landlocked(puzzle:Puzzle,numbers:List[(Int,Int)], implicit_landlocked:List[(Int,Int)]): List[(Int,Int)] ={
    val liste = puzzle.puzzle

    if(numbers.nonEmpty){
      val row = numbers.head._1
      val column = numbers.head._2

      if(liste(row)(column).asDigit == sum_of_char(puzzle,row,column,'_')){
        val newlist:List[(Int,Int)] = implicit_landlocked :+ (row,column)
        return find_implicit_landlocked(puzzle ,numbers.drop(1) ,newlist)
      }
      else find_implicit_landlocked(puzzle ,numbers.drop(1) ,implicit_landlocked)
    }
    else return implicit_landlocked
  }


  // returns true if the posistion (row,column) of the list
  // is a "char", else false
  def check_if_char_bool(puzzlelist:List[List[Char]], row:Int, column:Int, char: Char): Boolean ={
    if(puzzlelist(row)(column) == char){
      return true
    }
    false
  }

  // returns 1 if the posistion (row,column) of the list
  // is a "char", else 0
  def check_if_char_int(puzzle: Puzzle, row:Int, column:Int, char: Char): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzle_list = puzzle.puzzle

    if(validpos(row ,column ,x ,y)){
      if(puzzle_list(row)(column) == char){
        return 1
      }
      else 0
    }
    else 0
  }

  // returns the number of char
  // adjecent to tile, given a posistion(row,column).
  def sum_of_char(puzzle: Puzzle, row:Int, column:Int,char: Char): Int ={

    val up = check_if_char_int(puzzle,row - 1,column, char)
    val down = check_if_char_int(puzzle,row + 1,column, char)
    val left = check_if_char_int(puzzle,row,column - 1, char)
    val right = check_if_char_int(puzzle,row,column + 1, char)

    val sum = up + down + left + right
    return sum
  }

  def all_number_pos(puzzle: Puzzle): List[(Int,Int)] ={
    val one: List[(Int,Int)] = find_pos_of_char(puzzle,List(),0,0,'1')
    val two: List[(Int,Int)] = one ++ find_pos_of_char(puzzle,List(),0,0,'2')
    val three: List[(Int,Int)] = two ++ find_pos_of_char(puzzle,List(),0,0,'3')
    val four: List[(Int,Int)] = three ++ find_pos_of_char(puzzle,List(),0,0,'4')

    return four: List[(Int,Int)]
  }

  def update_number(puzzle: Puzzle ,savedstate:Puzzle ,numbers:List[(Int,Int)]): Puzzle ={
    val row = numbers.head._1
    val column = numbers.head._2

    val sum_numbers = sum_of_char(puzzle,row,column, '*')
    val newpuzzle = puzzle.setInt(savedstate, row,column,sum_numbers)
    return newpuzzle
  }

  // takes in positions of numbers and updates them
  // with the update_number function.
  def update_numbers(puzzle: Puzzle,savedstate:Puzzle, numbers:List[(Int,Int)]): Puzzle ={
    if(numbers.nonEmpty){

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

    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val lamp = char_if_valid(puzzle,row,column,x,y,'*')
      return place_landlocked(lamp,pos.drop(1))
    }
    return puzzle
  }

  def char_if_valid_Light(puzzle:Puzzle, row:Int, column:Int, x:Int, y:Int, char:Char): Puzzle ={
    if(validpos_light(puzzle.puzzle,row,column,x, y)){
      val newpuzzle = puzzle.setChar(row, column, char)
      return newpuzzle
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
  // is a valid index & a white tile or grey tile
  def validpos_light(liste:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    if(isBlack(liste, row, column)) {
        return false
    }
    if(check_if_char_bool(liste,row,column,'*')){
      return false
    }
    if(isWhite(liste, row, column)){
      return true
    }
    if(isGray(liste, row, column)){
      return true
    }
    return true
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

  def isGray(puzzle_list:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzle_list(row)(column) == 'G'){
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
}
