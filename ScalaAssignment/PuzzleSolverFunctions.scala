object PuzzleSolverFunctions {
  /**
   * Update numbers automatically runs the greybox algorithm
   * Any function that places lamps, automatically runs the update numbers algorithm
   * Any function that places lamps other than landlocked, automatically runs the shine light algorithm
   * @param startPuzzle Start puzzle which we will use our algorithm to solve
   * @return finalPuzzle - puzzle updated by main algorithm
   */
  def mainAlgorithm(startPuzzle: Puzzle): Puzzle ={
    val greyBox = greyBoxAlgorithm(startPuzzle)
    val landlocked = landlockedAlgorithm(greyBox, startPuzzle)

    val finalPuzzle = algorithmRecursion(landlocked,startPuzzle)
    return finalPuzzle
  }

  /**
   * Find all zeros, and add grey boxes adjacent.
   * @param puzzle puzzle class to add grey boxes to
   * @return puzzle class updated with grey boxes
   */
  def greyBoxAlgorithm(puzzle: Puzzle): Puzzle ={
    val zeros = findPosOfChar(puzzle,List(),0,0,'0')
    val greyBoxPuzzle = greyBox(puzzle,zeros)
    return greyBoxPuzzle
  }

  /**
   * Find all numbers and update numbers according to amount of lamps adjacent
   * @param puzzle puzzle class to update numbers to
   * @param savedState original puzzle class unaltered
   * @return puzzle with numberes updated and added
   */
  def updateNumbersAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val numbers = allNumberPos(puzzle)
    val numbersPuzzle = updateNumbers(puzzle, savedState, numbers)

    val greyBox = greyBoxAlgorithm(numbersPuzzle)
    return greyBox
  }

  /**
   * Find all landlocked tiles and place lamps in them
   * @param puzzle puzzle class to add lamps to landlocked tiles
   * @param savedState original puzzle class unaltered
   * @return puzzle with lamps added into landlocked tiles
   */
  def landlockedAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val landlockedTiles = findLandlocked(puzzle,List(),0,0)
    val landlockedPuzzle = placeLandlocked(puzzle, landlockedTiles)

    val updateNumbers = updateNumbersAlgorithm(landlockedPuzzle,savedState)
    return updateNumbers
  }

  /**
   * Find all implicit landlocked tiles and place lamps adjacent
   * @param puzzle puzzle class to add lamps to implicit landlocked tiles
   * @param savedState original puzzle class unaltered
   * @return puzzle with lamps added into implicit landlocked tiles
   */
  def implicitLandlockedAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val numbers = allNumberPos(puzzle)
    val implicitLand = findImplicitLandlocked(puzzle,numbers,List())
    val implicitLandPuzzle = placeImplicitNumber(puzzle,implicitLand)

    val updateNumbers = updateNumbersAlgorithm(implicitLandPuzzle, savedState)
    val shineLight = shineLightAlgorithm(updateNumbers)
    return shineLight
  }

  /**
   * Cast light from lamps
   * @param puzzle puzzle class to shine light from lamps
   * @return puzzle class with lamps shining light
   */
  def shineLightAlgorithm(puzzle: Puzzle): Puzzle ={
    val lamps = findPosOfChar(puzzle,List(),0,0,'*')
    val lightPuzzle = lights(puzzle, lamps)
    return lightPuzzle
  }

  /**
   * Find grey implicits and place lamps in the white tile
   * @param puzzle puzzle class to add lamps to implicit grey landlocked tiles
   * @param savedState original puzzle class unaltered
   * @return puzzle with lamps added into implicit grey landlocked tiles
   */
  def implicitGreyAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val greyBoxes = findPosOfChar(puzzle,List(),0,0,'G')
    val greyImplicits = findImplicitWhiteGrey(puzzle,greyBoxes,List(), 1)
    val greyImplicitsPuzzle = placeImplicitsWhiteGrey(puzzle,greyImplicits)

    val updateNumbers = updateNumbersAlgorithm(greyImplicitsPuzzle, savedState)
    val shineLight = shineLightAlgorithm(updateNumbers)
    return shineLight
  }

  /**
   * Find white implicits and place lamps in the white tile
   * @param puzzle puzzle class to add lamps to implicit white landlocked tiles
   * @param savedState original puzzle class unaltered
   * @return puzzle with lamps added into implicit white landlocked tiles
   */
  def implicitWhiteAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val whiteBoxes = findPosOfChar(puzzle,List(),0,0,'_')
    val whiteImplicits = findImplicitWhiteGrey(puzzle,whiteBoxes,List(), 0)
    val white_implicit_puzzle = placeImplicitsWhiteGrey(puzzle,whiteImplicits)

    val updateNumbers = updateNumbersAlgorithm(white_implicit_puzzle, savedState)
    val shineLight = shineLightAlgorithm(updateNumbers)
    return shineLight
  }

  /**
   * Find illegal positions and add grey boxes in them
   * @param puzzle puzzle class to add grey boxes to illegal positions
   * @param savedState original puzzle class unaltered
   * @return puzzle class with grey boxes in illegal positions
   */
  def illegalAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val whiteBoxes = findPosOfChar(puzzle,List(),0,0,'_')
    val illegalPos = findIllegals(puzzle, whiteBoxes, List(), savedState)
    val illegalPuzzle = placeIllegal(puzzle, illegalPos)

    return illegalPuzzle
  }

  /**
   * Continuously runs the solving algorithm until algorithm is changes nothing.
   * @param landlockedPuzzle puzzle class to run the main algorithm on
   * @param savedState original puzzle class unaltered
   * @return puzzle class updated with the main algorithm
   */
  def algorithmRecursion(landlockedPuzzle: Puzzle, savedState:Puzzle): Puzzle ={
    if(!isIdentical(landlockedPuzzle, implicitLandlockedAlgorithm(landlockedPuzzle, savedState))){
      return algorithmRecursion(implicitLandlockedAlgorithm(landlockedPuzzle,savedState),savedState)
    }
    if(!isIdentical(landlockedPuzzle, implicitGreyAlgorithm(landlockedPuzzle,savedState))){
      return algorithmRecursion(implicitGreyAlgorithm(landlockedPuzzle,savedState),savedState)
    }
    if(!isIdentical(landlockedPuzzle, implicitWhiteAlgorithm(landlockedPuzzle,savedState))){
      return algorithmRecursion(implicitWhiteAlgorithm(landlockedPuzzle,savedState),savedState)
    }
    if(!isIdentical(landlockedPuzzle, illegalAlgorithm(landlockedPuzzle, savedState))){
      return algorithmRecursion(illegalAlgorithm(landlockedPuzzle, savedState),savedState)
    }
    else return landlockedPuzzle
  }

  /**
   * Check if two puzzles are identical
   * @param oldPuzzle old puzzle to compare to newPuzzle
   * @param newPuzzle new puzzle to compare to oldPuzzle
   * @return true/false based on if they're identical
   */
  def isIdentical(oldPuzzle: Puzzle, newPuzzle: Puzzle): Boolean ={
    val listOld = oldPuzzle.puzzle
    val listNew = newPuzzle.puzzle

    if (listOld == listNew){
      return true
    }
    return false
  }

  /**
   * Bruteforce algorithm if main algorithm isn't enough
   * Checks if the puzzle sent from bruteforceRecursionRandom
   * is correct by checking the if there are any remaining
   * numbers or grey boxes, repeats itself if so.
   * @param puzzle puzzle altered by the algorithm
   * @param savedState is the original puzzle, used to update numbers
   * @return bruteforcePuzzle returns the finished puzzle after bruteforce is done
   */
  def bruteforceAlgorithm(puzzle: Puzzle, savedState:Puzzle): Puzzle ={
    val whitePos = findPosOfChar(puzzle,List(),0,0,'_')
    val bruteforceAttempt = bruteforceRecursionRandom(puzzle, whitePos, savedState)

    if(allNumberPos(bruteforceAttempt).nonEmpty){
      return bruteforceAlgorithm(puzzle,savedState)
    }
    if(findPosOfChar(bruteforceAttempt,List(),0,0,'G').nonEmpty){
      return bruteforceAlgorithm(puzzle,savedState)
    }
    return bruteforceAttempt
  }

  /**
   * Picks a random index of the list of white tiles
   * and places lamp in index. continues until there
   * are no remaining white tiles.
   * @param puzzle puzzle altered by the algorithm
   * @param whitePos list of white tiles
   * @param savedState is the original puzzle, used to update numbers
   * @return puzzle with all white tiles randomly filled in.
   */
  def bruteforceRecursionRandom(puzzle: Puzzle, whitePos:List[(Int,Int)], savedState:Puzzle): Puzzle ={
    val r = scala.util.Random

    if(whitePos.nonEmpty){
      val randomNumber = r.nextInt(whitePos.length)
      val randomIndex = whitePos(randomNumber)

      val row = randomIndex._1
      val column = randomIndex._2

      val bruteforce_puzzle = puzzle.setChar(row,column, '*')
      val update_numbers = updateNumbersAlgorithm(bruteforce_puzzle, savedState)
      val shine_light = shineLightAlgorithm(update_numbers)

      val whites_updated = findPosOfChar(shine_light,List(),0,0,'_')
      return bruteforceRecursionRandom(shine_light, whites_updated, savedState)
    }
    return puzzle
  }

  /**
   * Places grey box in position deemed illegal
   * @param puzzle puzzle class to fill in illegal position in
   * @param listOfIllegals position of all illegal positions in puzzle class
   * @return puzzle with all illegal positions filled with grey boxes
   */
  def placeIllegal(puzzle: Puzzle, listOfIllegals:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(listOfIllegals.nonEmpty){
      val row = listOfIllegals.head._1
      val column = listOfIllegals.head._2

      val updated = charIfValid(puzzle, row, column, x, y, 'G')
      return placeIllegal(updated,listOfIllegals.drop(1))
    }
    return puzzle

  }

  /**
   * finds lamp positions that would break the game.
   * @param puzzle puzzle class to check for illegal tiles in
   * @param remainingWhites position of all white tiles in puzzle class
   * @param listOfIllegals list of all illegal position in the puzzle class
   * @return listOfIllegals
   */
  def findIllegals(puzzle: Puzzle, remainingWhites:List[(Int,Int)], listOfIllegals:List[(Int,Int)], savedState:Puzzle): List[(Int,Int)] ={
    val numbers = allNumberPos(puzzle)

    if(remainingWhites.nonEmpty){
      val row = remainingWhites.head._1
      val column = remainingWhites.head._2

      val illegalCheck = puzzle.setChar(row,column, '*')
      val illegalCheckNumbersUpdate = updateNumbersAlgorithm(illegalCheck, savedState)
      val illegalCheckShine = shineLightAlgorithm(illegalCheckNumbersUpdate)
      val illegalCheckImplicit = implicitLandlockedAlgorithm(illegalCheckShine,savedState)

      if(!validateAllNumbers(illegalCheckImplicit,numbers)){
        val illegals_updated = listOfIllegals :+ (row,column)
        return findIllegals(puzzle, remainingWhites.drop(1), illegals_updated, savedState)
      }
      return findIllegals(puzzle, remainingWhites.drop(1), listOfIllegals, savedState)
    }
    return listOfIllegals
  }

  /**
   * Checks if any numbers in the puzzle are unsolvable.
   * @param puzzle puzzle class to check validity in
   * @param numbersPos position of all remaining numbers
   * @return true/false based on if the validity of all numbers remaining
   */
  def validateAllNumbers(puzzle: Puzzle, numbersPos: List[(Int, Int)]): Boolean ={
    if(numbersPos.nonEmpty){
      val row = numbersPos.head._1
      val column = numbersPos.head._2

      if(!validate_number(puzzle, row, column)){
        return false
      }
      return validateAllNumbers(puzzle,numbersPos.drop(1))
    }
    return true
  }


  /**
   * validates number, by checking if the number of the tile < adjacent white tiles
   * @param puzzle puzzle class to check validity in
   * @param row which row to check
   * @param column which column to check
   * @return true/false based on if the validity of the number
   */
  def validate_number(puzzle: Puzzle ,row:Int ,column:Int): Boolean ={
    val puzzleList = puzzle.puzzle

    if(puzzleList(row)(column).asDigit <= sumOfChar(puzzle,row,column,'_')){
      return true
    }
    return false
  }

  /**
   * Attempts to place lamps in white tiles towards all directions.
   * used to place lamps in white/grey implicit tiles
   * @param puzzle puzzle to place implicit in
   * @param implicitPos position of all implicits
   * @return puzzle with all implicit positions filled with lamps
   */
  def placeImplicitsWhiteGrey(puzzle: Puzzle, implicitPos:List[(Int,Int)]): Puzzle ={

    if(implicitPos.nonEmpty){
      val row = implicitPos.head._1
      val column = implicitPos.head._2

      val up = placeImplicitWhiteGrey(puzzle,row,column, 'U')
      val down = placeImplicitWhiteGrey(up,row,column, 'D')
      val left = placeImplicitWhiteGrey(down,row,column, 'L')
      val right = placeImplicitWhiteGrey(left,row,column, 'R')

      return placeImplicitsWhiteGrey(right,implicitPos.drop(1))
    }
    return puzzle
  }

  /**
   * Attempts to place a lamp in a white tile towards
   * a certain direction until it hits the end of the puzzle
   * or a black tile
   * @param puzzle puzzle to place implicit in
   * @param row start row to place from
   * @param column start column to place from
   * @param direction which direction to place towards
   * @return puzzle with lamps added in one direction
   */
  def placeImplicitWhiteGrey(puzzle: Puzzle, row:Int, column:Int, direction:Char): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValid(puzzle, row, column, x, y ,'*')
          placeImplicitWhiteGrey(puzzleUpdated, row - 1, column, direction)
        }
        else return puzzle
      }
      case 'D' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValid(puzzle, row, column, x, y ,'*')
          placeImplicitWhiteGrey(puzzleUpdated, row + 1, column, direction)
        }
        else return puzzle
      }
      case 'L' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValid(puzzle, row, column, x, y ,'*')
          placeImplicitWhiteGrey(puzzleUpdated, row, column - 1, direction)
        }
        else return puzzle
      }
      case 'R' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValid(puzzle, row, column, x, y ,'*')
          placeImplicitWhiteGrey(puzzleUpdated, row, column + 1, direction)
        }
        else return puzzle
      }
      case _ => {
        return puzzle
      }
    }
  }

  /**
   * takes in positions of all white/grey tiles remaining
   * and determines if they are implicit white/grey landlocked tiles.
   * returns list of all implicits white/grey landlocked tiles.
   * @param puzzle Puzzle class to check for grey/white implicit landlocked tiles in
   * @param whiteGreyPos position of greys/whites tiles in puzzle
   * @param listOfImplicitWhiteGrey List of implicit greys/whites positions to later return
   * @return listOfImplicitWhiteGrey Contains positions of all implicit landlocked greys/whites positions in form of List of tuple2, List[(Int, Int)]
   */
  def findImplicitWhiteGrey(puzzle:Puzzle, whiteGreyPos:List[(Int,Int)], listOfImplicitWhiteGrey:List[(Int,Int)], count:Int): List[(Int,Int)] ={

    if(whiteGreyPos.nonEmpty){
      val row = whiteGreyPos.head._1
      val column = whiteGreyPos.head._2

      val up = countCharUntilBlack(puzzle,row,column,'U','_',0)
      val down = countCharUntilBlack(puzzle,row,column,'D','_',0)
      val left = countCharUntilBlack(puzzle,row,column,'L','_',0)
      val right = countCharUntilBlack(puzzle,row,column,'R','_',0)

      val sum = up + down + left + right

      if(sum == count){
        val listOfImplicitWhiteGreyUpdated:List[(Int,Int)] = listOfImplicitWhiteGrey :+ (row,column)
        return findImplicitWhiteGrey(puzzle ,whiteGreyPos.drop(1) ,listOfImplicitWhiteGreyUpdated, count)
      }
      else findImplicitWhiteGrey(puzzle ,whiteGreyPos.drop(1) ,listOfImplicitWhiteGrey, count)
    }
    else return listOfImplicitWhiteGrey
  }

  /**
   * Counts the amount of a specific character
   * while moving towards a specified direction
   * until it hits a black wall or the end of the puzzle.
   * @param puzzle puzzle class to check in
   * @param row Start row of search
   * @param column Start column of search
   * @param char which character to look after
   * @param direction which direction to look
   * @param countChar the sum of character found until hit black tile/ end of puzzle.
   * @return charTileSum
   */
  def countCharUntilBlack(puzzle: Puzzle, row:Int, column:Int, direction:Char, char: Char, countChar:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validPos(row - 1, column, x , y) && (!isBlack(puzzleList,row - 1,column))){
          if(puzzleList(row - 1)(column) == char){
            val countCharUpdated = countChar + 1
            return countCharUntilBlack(puzzle,row - 1, column, 'U', char, countCharUpdated)
          }
          return countCharUntilBlack(puzzle,row - 1, column,'U', char, countChar)
        }
      }
      case 'D' => {
        if(validPos(row + 1, column, x , y) && (!isBlack(puzzleList,row + 1,column))){
          if(puzzleList(row + 1)(column) == char){
            val countCharUpdated = countChar + 1
            return countCharUntilBlack(puzzle,row + 1, column, 'D', char, countCharUpdated)
          }
          return countCharUntilBlack(puzzle,row + 1, column, 'D',char, countChar)
        }
      }
      case 'L' => {
        if(validPos(row, column - 1, x , y) && (!isBlack(puzzleList,row,column - 1))){
          if(puzzleList(row)(column - 1) == char){
            val countCharUpdated = countChar + 1
            return countCharUntilBlack(puzzle,row, column - 1, 'L',char, countCharUpdated)
          }
          return countCharUntilBlack(puzzle,row, column - 1, 'L', char, countChar)
        }
      }
      case 'R' => {
        if(validPos(row, column + 1, x , y) && (!isBlack(puzzleList,row,column + 1))){
          if(puzzleList(row)(column + 1) == char){
            val countCharUpdated = countChar + 1
            return countCharUntilBlack(puzzle,row, column + 1, 'R', char, countCharUpdated)
          }
          return countCharUntilBlack(puzzle,row, column + 1, 'R', char, countChar)
        }
      }
    }
    return (countChar)
  }

  /**
   * casts light in all directions from lamps
   * @param puzzle puzzle class to add light into
   * @param lampsPos position of all lamps in puzzle
   * @return puzzle class with lights added in all directions from all lamp positions
   */
  def lights(puzzle: Puzzle, lampsPos:List[(Int,Int)]): Puzzle ={
    if(lampsPos.nonEmpty){
      val row = lampsPos.head._1
      val column = lampsPos.head._2

      val up = light(puzzle,row,column, 'U')
      val down = light(up,row,column, 'D')
      val left = light(down,row,column, 'L')
      val right = light(left,row,column, 'R')

      return lights(right,lampsPos.drop(1))
    }
    return puzzle
  }

  /**
   * casts light in one direction from one lamp
   * @param puzzle Puzzle class to update with light
   * @param row start row for light
   * @param column start column for light
   * @param direction which direction to place light
   * @return puzzle class with light added in one direction from a certain position
   */
  def light(puzzle: Puzzle, row:Int, column:Int, direction:Char): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValidLight(puzzle, row, column, x, y ,'L')
          light(puzzleUpdated, row - 1, column, direction)
        }
        else return puzzle
      }
      case 'D' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValidLight(puzzle, row, column, x, y ,'L')
          light(puzzleUpdated, row + 1, column, direction)
        }
        else return puzzle
      }
      case 'L' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValidLight(puzzle, row, column, x, y ,'L')
          light(puzzleUpdated, row, column - 1, direction)
        }
        else return puzzle
      }
      case 'R' => {
        if(validPos(row, column, x , y) && (!isBlack(puzzleList,row,column))){
          val puzzleUpdated = charIfValidLight(puzzle, row, column, x, y ,'L')
          light(puzzleUpdated, row, column + 1, direction)
        }
        else return puzzle
      }
      case _ => {
        return puzzle
      }
    }
  }

  /**
   * counts the distance until it hits a specific character
   * @param puzzle puzzle class to check in
   * @param row Start row of search
   * @param column Start column of search
   * @param char which character to look after
   * @param direction which direction to look
   * @param charCount distance to character from start point
   * @return CharCount
   */
  def countUntilChar(puzzle: Puzzle, row:Int, column:Int, char: Char, direction:Char, charCount:Int): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    direction.toUpper match {
      case 'U' => {
        if(validPos(row - 1, column, x , y) && puzzleList(row - 1)(column) != char){
          val charCountUpdated = charCount + 1
          countUntilChar(puzzle, row - 1, column, char, direction, charCountUpdated)
        }
        else return charCount
      }
      case 'D' => {
        if(validPos(row + 1, column, x , y) && puzzleList(row + 1)(column) != char){
          val charCountUpdated = charCount + 1
          countUntilChar(puzzle, row + 1, column, char, direction, charCountUpdated)
        }
        else return charCount
      }
      case 'L' => {
        if(validPos(row, column - 1, x , y) && puzzleList(row)(column - 1) != char){
          val charCountUpdated = charCount + 1
          countUntilChar(puzzle, row, column - 1, char, direction, charCountUpdated)
        }
        else return charCount
      }
      case 'R' => {
        if(validPos(row, column + 1, x , y) && puzzleList(row)(column + 1) != char){
          val charCountUpdated = charCount + 1
          countUntilChar(puzzle, row, column + 1, char, direction, charCountUpdated)
        }
        else return charCount
      }
      case _ => {
        return charCount
      }
    }
  }

  /**
   * places lamps next to a numbered tile, if it's a valid pos
   * @param puzzle Puzzle class to update
   * @param listOfImplicitLandlocked List of implicit landlocked tiles to place lamps adjacent to
   * @return puzzle class with implicit landlocked tiles updated with lamps
   */
  def placeImplicitNumber(puzzle:Puzzle, listOfImplicitLandlocked:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(listOfImplicitLandlocked.nonEmpty){
      val row = listOfImplicitLandlocked.head._1
      val column = listOfImplicitLandlocked.head._2

      val up = charIfValid(puzzle,row - 1,column,x,y,'*')
      val down = charIfValid(up,row + 1,column,x,y,'*')
      val left = charIfValid(down,row,column - 1,x,y,'*')
      val right = charIfValid(left,row,column + 1,x,y,'*')
      return placeImplicitNumber(right,listOfImplicitLandlocked.drop(1))
    }
    return puzzle
  }

  /**
   * takes the list of all numbers of the puzzle and checks each number.
   * If the number of '_' equals it's number, add the position to the return list.
   * returns a list of all implicit landlocked tiles.
   * @param puzzle Puzzle class to check for landlocked tiles in
   * @param numbersPos position of numbers in puzzle class
   * @param listOfImplicitLandlocked List of char positions to later return
   * @return listOfImplicitLandlocked Contains positions of all implicit landlocked tiles in form of List of tuple2, List[(Int, Int)]
   */
  def findImplicitLandlocked(puzzle:Puzzle, numbersPos:List[(Int,Int)], listOfImplicitLandlocked:List[(Int,Int)]): List[(Int,Int)] ={
    val puzzleList = puzzle.puzzle

    if(numbersPos.nonEmpty){
      val row = numbersPos.head._1
      val column = numbersPos.head._2

      if(puzzleList(row)(column).asDigit == sumOfChar(puzzle,row,column,'_')){
        val listOfImplicitLandlockedUpdated:List[(Int,Int)] = listOfImplicitLandlocked :+ (row,column)
        return findImplicitLandlocked(puzzle ,numbersPos.drop(1) ,listOfImplicitLandlockedUpdated)
      }
      else findImplicitLandlocked(puzzle ,numbersPos.drop(1) ,listOfImplicitLandlocked)
    }
    else return listOfImplicitLandlocked
  }

  /**
   * returns true if the posistion (row,column) of the list
   * is a "char", else false
   * @param puzzleList puzzle list to check char in
   * @param row which row to check in
   * @param column which column to check in
   * @param char which character to look after
   * @return false/true based on if the character was found or not
   */
  def check_if_char_bool(puzzleList:List[List[Char]], row:Int, column:Int, char: Char): Boolean ={
    if(puzzleList(row)(column) == char){
      return true
    }
    false
  }

  /**
   * returns 1 if the posistion (row,column) of the list
   * is a "char", else 0
   * @param puzzle puzzle class to check char in
   * @param row which row to check in
   * @param column which column to check in
   * @param char which character to look after
   * @return 0/1 based on if the character was found or not
   */
  def checkIfCharInt(puzzle: Puzzle, row:Int, column:Int, char: Char): Int ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    if(validPos(row ,column ,x ,y)){
      if(puzzleList(row)(column) == char){
        return 1
      }
      else 0
    }
    else 0
  }

  /**
   * returns the number of char
   * adjecent to tile, given a posistion(row,column).
   * @param puzzle puzzle class to check sum of char in
   * @param row which row to check in
   * @param column which column to check in
   * @param char which character to look after
   * @return sumOfChar - int corresponding to the amount of character found adjacent
   */
  def sumOfChar(puzzle: Puzzle, row:Int, column:Int, char: Char): Int ={

    val up = checkIfCharInt(puzzle,row - 1,column, char)
    val down = checkIfCharInt(puzzle,row + 1,column, char)
    val left = checkIfCharInt(puzzle,row,column - 1, char)
    val right = checkIfCharInt(puzzle,row,column + 1, char)

    val sum = up + down + left + right
    return sum
  }

  /**
   * returns a list of all remaining numbers positions
   * @param puzzle puzzle class to find all the numbers in
   * @return listOfAllNumbers contains positions of all numbered tiles(1-4) in form of List of tuple2, List[(Int, Int)]
   */
  def allNumberPos(puzzle: Puzzle): List[(Int,Int)] ={
    val one: List[(Int,Int)] = findPosOfChar(puzzle,List(),0,0,'1')
    val two: List[(Int,Int)] = one ++ findPosOfChar(puzzle,List(),0,0,'2')
    val three: List[(Int,Int)] = two ++ findPosOfChar(puzzle,List(),0,0,'3')
    val four: List[(Int,Int)] = three ++ findPosOfChar(puzzle,List(),0,0,'4')

    return four: List[(Int,Int)]
  }

  /**
   * @param puzzle puzzle class to update number in
   * @param savedState the original puzzle unchanged - used to calculate current number in numbered tile
   * @param numberPos position of number to update
   * @return puzzle class with one number position updated according to amount of lamps adjacent
   */
  def updateNumber(puzzle: Puzzle, savedState:Puzzle, numberPos:List[(Int,Int)]): Puzzle ={
    val row = numberPos.head._1
    val column = numberPos.head._2

    val sumNumbers = sumOfChar(puzzle,row,column, '*')
    val puzzleUpdated = puzzle.setInt(savedState, row,column,sumNumbers)
    return puzzleUpdated
  }

  /**
   * takes in positions of numbers and updates them
   * with the update_number function.
   * with the update_number function.
   * @param puzzle puzzle class to update numbers in
   * @param savedState the original puzzle unchanged - used to calculate current number in numbered tile
   * @param numbersPos position of all numbers in the puzzle
   * @return puzzle class with numbers updated according to amount of lamps adjacent
   */
  def updateNumbers(puzzle: Puzzle, savedState:Puzzle, numbersPos:List[(Int,Int)]): Puzzle ={
    if(numbersPos.nonEmpty){

      val update = updateNumber(puzzle,savedState,numbersPos)
      return updateNumbers(update,savedState,numbersPos.drop(1))
    }
    return puzzle
  }

  /**
   * returns a new class, with greyboxes
   * around 0 numbered black tiles
   * found with the findPosOfChar function
   * @param puzzle puzzle class to place characters into
   * @param landlockedPos list of all landlocked tiles, List[(Int, Int)]
   * @return puzzle class with updated char in landlocked positions
   */
  def placeLandlocked(puzzle:Puzzle, landlockedPos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(landlockedPos.nonEmpty){
      val row = landlockedPos.head._1
      val column = landlockedPos.head._2

      val lamp = charIfValid(puzzle,row,column,x,y,'*')
      return placeLandlocked(lamp,landlockedPos.drop(1))
    }
    return puzzle
  }

  /**
   * @param puzzle puzzle class to check validity in
   * @param row wanted row placement of character
   * @param column wanted column placement of character
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @param char wanted character to place in position
   * @return puzzle class with updated char in wanted position (row,column)
   */
  def charIfValidLight(puzzle:Puzzle, row:Int, column:Int, x:Int, y:Int, char:Char): Puzzle ={
    if(validPosLight(puzzle.puzzle,row,column,x, y)){
      val puzzleUpdated = puzzle.setChar(row, column, char)
      return puzzleUpdated
    }

    return puzzle
  }

  /**
   * @param puzzle puzzle class to check validity in
   * @param row wanted row placement of character
   * @param column wanted column placement of character
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @param char wanted character to place in position
   * @return puzzle class with updated char in wanted position (row,column)
   */
  def charIfValid(puzzle:Puzzle, row:Int, column:Int, x:Int, y:Int, char:Char): Puzzle ={
    if(validPosWhite(puzzle.puzzle,row,column,x, y)){
      val updatedPuzzle = puzzle.setChar(row, column, char)
      return updatedPuzzle
    }
    return puzzle
  }

  /**
   * returns a new class, with greyboxes
   * around 0 numbered black tiles
   * found with the find_pos_zero function
   * @param puzzle Puzzle class to check for landlocked tiles in
   * @param zeroPos List of zero numbered black tiles, in the form of: List[(Int,Int)]
   * @return puzzle class with updated grey boxes around zero
   */
  def greyBox(puzzle:Puzzle, zeroPos:List[(Int,Int)]): Puzzle ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY

    if(zeroPos.nonEmpty){
      val row = zeroPos.head._1
      val column = zeroPos.head._2

      val up = charIfValid(puzzle,row - 1,column,x,y,'G')
      val down = charIfValid(up,row + 1,column,x,y,'G')
      val left = charIfValid(down ,row,column - 1,x,y,'G')
      val right = charIfValid(left,row,column + 1,x,y,'G')

      return greyBox(right,zeroPos.drop(1))
    }
    return puzzle
  }

  /**
   * Checks each index in a 2d list
   * and checks if each adjecent tile is black.
   * returns a list of all 'landlocked tiles
   * @param puzzle Puzzle class to check for landlocked tiles in
   * @param listOfLandlocked List of char positions to later return
   * @param row Start row of search
   * @param column Start column of search
   * @return listOfLandlocked Contains positions of all landlocked tiles in form of List of tuple2, List[(Int, Int)]
   */
  def findLandlocked(puzzle:Puzzle, listOfLandlocked:List[(Int,Int)], row:Int, column:Int): List[(Int,Int)] ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    if (row > y - 1){
      return listOfLandlocked
    }
    if(column > x - 1){
      return findLandlocked(puzzle,listOfLandlocked,row +1, 0)
    }
    if(checkLandlocked(puzzleList,row - 1,column,x, y)) {
      if (checkLandlocked(puzzleList, row + 1, column, x, y)) {
        if (checkLandlocked(puzzleList, row, column - 1, x, y)) {
          if (checkLandlocked(puzzleList, row, column + 1, x, y)) {
            val listOfLandlockedUpdated:List[(Int,Int)] = listOfLandlocked :+ (row,column)
            return findLandlocked(puzzle,listOfLandlockedUpdated,row,column + 1)
          }
        }
      }
    }
    return findLandlocked(puzzle,listOfLandlocked,row,column + 1)
  }

  /**
   * returns posistions of given char
   * in the form a list of tuples(row,column)
   * @param puzzle Puzzle class to check for char in
   * @param listOfChar List of char positions to later return
   * @param row Start row of search
   * @param column Start column of search
   * @param char Which char to look after in puzzle
   * @return listOfChar Contains positions of wanted char in form of List of tuple2, List[(Int, Int)]
   */
  def findPosOfChar(puzzle:Puzzle, listOfChar:List[(Int,Int)], row:Int, column:Int, char: Char): List[(Int,Int)] ={
    val x = puzzle.sizeX
    val y = puzzle.sizeY
    val puzzleList = puzzle.puzzle

    if (row > y - 1){
      return listOfChar
    }
    if(column > x - 1){
      return findPosOfChar(puzzle,listOfChar,row +1, 0,char)
    }
    if(puzzleList(row)(column) == char){
      val listOfCharUpdated:List[(Int,Int)] = listOfChar :+ (row,column)
      return findPosOfChar(puzzle,listOfCharUpdated,row,column + 1,char)
    }
    else{
      return findPosOfChar(puzzle,listOfChar,row,column + 1,char)
    }
  }

  /**
   * returns true if the posistion (row,column) of the list
   * is either a non valid index or a black tile
   * @param puzzleList puzzle state
   * @param row which row to check
   * @param column Which column to check
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @return Boolean
   */
  def checkLandlocked(puzzleList:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return true
    }
    if(column < 0 || column > x - 1){
      return true
    }
    if(isBlack(puzzleList, row, column)){
      return true
    }
    return false
  }

  /**
   * returns true if the posistion (row,column)
   * is a valid index & a white tile or grey tile
   * @param puzzleList puzzle state
   * @param row which row to check
   * @param column Which column to check
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @return Boolean
   */
  def validPosLight(puzzleList:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    if(isBlack(puzzleList, row, column)) {
        return false
    }
    if(check_if_char_bool(puzzleList,row,column,'*')){
      return false
    }
    if(isWhite(puzzleList, row, column)){
      return true
    }
    if(isGray(puzzleList, row, column)){
      return true
    }
    return true
  }

  /**
   * returns true if the posistion (row,column)
   * is a valid index & not a white tile
   * @param puzzleList puzzle state
   * @param row which row to check
   * @param column Which column to check
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @return Boolean
   */
  def validPosWhite(puzzleList:List[List[Char]], row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    if(!isWhite(puzzleList, row, column)){
      return false
    }
    return true
  }

  /**
   * returns true if the posistion (row,column)
   * is a valid index
   * @param row which row to check
   * @param column Which column to check
   * @param x Size of puzzle in x direction
   * @param y Size of puzzle in y direction
   * @return Boolean
   */
  def validPos(row:Int, column:Int, x:Int, y:Int):Boolean = {
    if(row < 0 || row > y - 1){
      return false
    }
    if(column < 0 || column > x - 1){
      return false
    }
    return true
  }

  /**
   * returns true if the pos
   * is a white tile
   * @param puzzleList puzzle list
   * @param row which row to check
   * @param column Which column to check
   * @return Boolean
   */
  def isWhite(puzzleList:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzleList(row)(column) == '_'){
      return true
    }
    return false
  }

  /**
   * returns true if the pos
   * is a grey tile
   * @param puzzleList puzzle state
   * @param row which row to check
   * @param column Which column to check
   * @return Boolean
   */
  def isGray(puzzleList:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzleList(row)(column) == 'G'){
      return true
    }
    return false
  }

  /**
   * function takes in list, and position(row,column)
   * and returns true if it's black
   * false if not
   * @param puzzleList puzzle state
   * @param row which row to check
   * @param column Which column to check
   * @return Boolean
   */
  def isBlack(puzzleList:List[List[Char]], row:Int, column:Int): Boolean ={
    if(puzzleList(row)(column) == '0'){
      return true
    }
    if(puzzleList(row)(column) == '1'){
      return true
    }
    if(puzzleList(row)(column) == '2'){
      return true
    }
    if(puzzleList(row)(column) == '3'){
      return true
    }
    if(puzzleList(row)(column) == '4'){
      return true
    }
    if(puzzleList(row)(column) == 'X'){
      return true
    }
    return false
  }
}