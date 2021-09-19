import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution, puzzlelist, unsolvedToList}
import PuzzleSolverFunctions.{all_number_pos, find_landlocked, find_pos_of_char, greybox, place_landlocked, sum_of_lamps, update_numbers}

object PuzzleSolver extends App{

  def solveCustom(puzzle:Puzzle):Puzzle = {
    // Puzzle String to List() and start class:
    unsolvedToList()
    val start_puzzle = new Puzzle(puzzle.sizeX, puzzle.sizeY, puzzlelist)

    println("Start:")
    start_puzzle.puzzle.foreach(x => println(x))

    // Find all zeros, and add greyboxes.
    val zeros = find_pos_of_char(start_puzzle,List(),0,0,'0')
    val greybox_puzzle = greybox(start_puzzle,zeros)
    println("Greybox:")
    greybox_puzzle.puzzle.foreach(x => println(x))

    // Find all landlocked tiles and place lamps
    val landlocked_tiles = find_landlocked(greybox_puzzle,List(),0,0)
    val landlocked_puzzle = place_landlocked(greybox_puzzle,landlocked_tiles)
    println("Landlocked:")
    landlocked_puzzle.puzzle.foreach(x => println(x))

    // Find all numbers and update numbers
    val numbers = all_number_pos(landlocked_puzzle)
    val numbers_puzzle = update_numbers(landlocked_puzzle,start_puzzle,numbers)
    println("Numbers update:")
    numbers_puzzle.puzzle.foreach(x => println(x))

    //Re-run greybox
    val zeros2 = find_pos_of_char(numbers_puzzle,List(),0,0,'0')
    val greybox_puzzle2 = greybox(numbers_puzzle,zeros2)
    println("Greybox:")
    greybox_puzzle2.puzzle.foreach(x => println(x))

    greybox_puzzle2
  }

  initRW("puzzleFiles/puzzle_unsolved.txt","puzzleFiles/puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solveCustom(getPuzzle(count)))
  }

  closing()
}