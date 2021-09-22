object PuzzleSolverFunctions {


  def main_algorithm(puzzle: Puzzle): Puzzle ={
    val startpuzzle = puzzle
    println("Start:")
    puzzle.puzzle.foreach(x => println(x))

    //Update numbers automatically runs the greybox algorithm
    //Any function that places lightbulbs other than landlocked, automatically runs the update numbers algorithm
    //and then the shine light algorithm
    val greybox = greybox_algorithm(startpuzzle)
    val landlocked = landlocked_algorithm(greybox, startpuzzle)

    val finalpuzzle = algorithm_recursion(landlocked,startpuzzle)
    return finalpuzzle
  }

  // Find all zeros, and add greyboxes.
  def greybox_algorithm(puzzle: Puzzle): Puzzle ={

    val zeros = find_pos_of_char(puzzle,List(),0,0,'0')
    val greybox_puzzle = greybox(puzzle,zeros)
    println("Greybox:")
    greybox_puzzle.puzzle.foreach(x => println(x))

    return greybox_puzzle
  }

  // Find all numbers and update numbers
  def update_numbers_algorithm(puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    val numbers = all_number_pos(puzzle)
    val numbers_puzzle = update_numbers(puzzle, savedstate, numbers)
    println("Numbers update:")
    numbers_puzzle.puzzle.foreach(x => println(x))

    val greybox = greybox_algorithm(numbers_puzzle)

    return greybox
  }

  // Find all landlocked tiles and place lamps
  def landlocked_algorithm(puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    val landlocked_tiles = find_landlocked(puzzle,List(),0,0)
    val landlocked_puzzle = place_landlocked(puzzle,landlocked_tiles)
    println("Landlocked:")
    landlocked_puzzle.puzzle.foreach(x => println(x))

    val update_numbers = update_numbers_algorithm(landlocked_puzzle,savedstate)

    return update_numbers
  }

  // Find all implicit landlocked tiles and place lamps adjecent
  def implicit_landlocked_algorithm(puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    val numbers = all_number_pos(puzzle)
    val impl_land = find_implicit_landlocked(puzzle,numbers,List())
    val impl_land_puzzle = place_implicit(puzzle,impl_land)
    println("Implicit landlocked:")
    impl_land_puzzle.puzzle.foreach(x => println(x))

    val update_numbers = update_numbers_algorithm(impl_land_puzzle, savedstate)
    val shine_light = shine_light_algorithm(update_numbers)

    return shine_light
  }

  // Cast light from lamps
  def shine_light_algorithm(puzzle: Puzzle): Puzzle ={

    val lamps = find_pos_of_char(puzzle,List(),0,0,'*')
    val light_puzzle = lights(puzzle, lamps)
    println("Light:")
    light_puzzle.puzzle.foreach(x => println(x))

    return light_puzzle
  }

  // find grey implicits and place lamps in the white tile
  def implicit_grey_algorithm(puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    val greyboxes = find_pos_of_char(puzzle,List(),0,0,'G')
    val grey_implicits = find_implicit_white_grey(puzzle,greyboxes,List(), 1)
    val grey_implicit_puzzle = place_implicits(puzzle,grey_implicits)
    println("grey implicits:")
    grey_implicit_puzzle.puzzle.foreach(x => println(x))

    val update_numbers = update_numbers_algorithm(grey_implicit_puzzle, savedstate)
    val shine_light = shine_light_algorithm(update_numbers)

    return shine_light
  }

  // find white implicits and place lamps in the white tile
  def implicit_white_algorithm(puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    val whiteboxes = find_pos_of_char(puzzle,List(),0,0,'_')
    val white_implicits = find_implicit_white_grey(puzzle,whiteboxes,List(), 0)
    val white_implicit_puzzle = place_implicits(puzzle,white_implicits)
    println("white implicits:")
    white_implicit_puzzle.puzzle.foreach(x => println(x))

    val update_numbers = update_numbers_algorithm(white_implicit_puzzle, savedstate)
    val shine_light = shine_light_algorithm(update_numbers)

    return shine_light
  }

  def algorithm_recursion(landlocked_puzzle: Puzzle, savedstate:Puzzle): Puzzle ={

    if(!isidentical(landlocked_puzzle, implicit_landlocked_algorithm(landlocked_puzzle, savedstate))){
      return algorithm_recursion(implicit_landlocked_algorithm(landlocked_puzzle,savedstate),savedstate)
    }
    if(!isidentical(landlocked_puzzle, implicit_grey_algorithm(landlocked_puzzle,savedstate))){
      return algorithm_recursion(implicit_grey_algorithm(landlocked_puzzle,savedstate),savedstate)
    }
    if(!isidentical(landlocked_puzzle, implicit_white_algorithm(landlocked_puzzle,savedstate))){
      return algorithm_recursion(implicit_white_algorithm(landlocked_puzzle,savedstate),savedstate)
    }
    else return landlocked_puzzle
  }

  def isidentical(old_puzzle: Puzzle, new_puzzle: Puzzle): Boolean ={
    val list_old = old_puzzle.puzzle
    val list_new = new_puzzle.puzzle

    if (list_old == list_new){
      return true
    }
    return false
  }



  def place_illegal(puzzle: Puzzle, pos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val updated = char_if_valid(puzzle, row, column, x, y, 'G')
      return place_illegal(updated,pos.drop(1))
    }
    return puzzle

  }


  def place_implicits(puzzle: Puzzle, pos:List[(Int,Int)]): Puzzle ={

    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val up = place_implicit(puzzle,row,column, 'U')
      val down = place_implicit(up,row,column, 'D')
      val left = place_implicit(down,row,column, 'L')
      val right = place_implicit(left,row,column, 'R')

      return place_implicits(right,pos.drop(1))
    }
    return puzzle
  }

  def place_implicit(puzzle: Puzzle, row:Int, column:Int, direction:Char): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzlelist = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid(puzzle, row, column, x, y ,'*')
          place_implicit(newpuzzle, row - 1, column, direction)
        }
        else return puzzle
      }
      case 'D' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid(puzzle, row, column, x, y ,'*')
          place_implicit(newpuzzle, row + 1, column, direction)
        }
        else return puzzle
      }
      case 'L' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid(puzzle, row, column, x, y ,'*')
          place_implicit(newpuzzle, row, column - 1, direction)
        }
        else return puzzle
      }
      case 'R' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid(puzzle, row, column, x, y ,'*')
          place_implicit(newpuzzle, row, column + 1, direction)
        }
        else return puzzle
      }
      case _ => {
        return puzzle
      }
    }
  }

  def find_implicit_white_grey(puzzle:Puzzle, white_grey:List[(Int,Int)], implicit_white_grey:List[(Int,Int)], count:Int): List[(Int,Int)] ={

    if(white_grey.nonEmpty){
      val row = white_grey.head._1
      val column = white_grey.head._2

      val up = count_char_until_black(puzzle,row,column,'U','_',0)
      val down = count_char_until_black(puzzle,row,column,'D','_',0)
      val left = count_char_until_black(puzzle,row,column,'L','_',0)
      val right = count_char_until_black(puzzle,row,column,'R','_',0)

      val sum = up + down + left + right

      if(sum == count){
        val newlist:List[(Int,Int)] = implicit_white_grey :+ (row,column)
        return find_implicit_white_grey(puzzle ,white_grey.drop(1) ,newlist, count)
      }
      else find_implicit_white_grey(puzzle ,white_grey.drop(1) ,implicit_white_grey, count)
    }
    else return implicit_white_grey
  }

  def count_char_until_black(puzzle: Puzzle, row:Int, column:Int, direction:Char, char: Char, white_count:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
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
    return (white_count)

  }

  def lights(puzzle: Puzzle, pos:List[(Int,Int)]): Puzzle ={
    if(pos.nonEmpty){
      val row = pos.head._1
      val column = pos.head._2

      val up = light(puzzle,row,column, 'U')
      val down = light(up,row,column, 'D')
      val left = light(down,row,column, 'L')
      val right = light(left,row,column, 'R')

      return lights(right,pos.drop(1))
    }
    return puzzle
  }


  def light(puzzle: Puzzle, row:Int, column:Int, direction:Char): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzlelist = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row - 1, column, direction)
        }
        else return puzzle
      }
      case 'D' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row + 1, column, direction)
        }
        else return puzzle
      }
      case 'L' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row, column - 1, direction)
        }
        else return puzzle
      }
      case 'R' => {
        if(validpos(row, column, x , y) && (!isBlack(puzzlelist,row,column))){
          val newpuzzle = char_if_valid_Light(puzzle, row, column, x, y ,'L')
          light(newpuzzle, row, column + 1, direction)
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
