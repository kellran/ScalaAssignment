import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzleCustom, initRW, linjerList, putSolutionCustom, unsolvedToList}
import PuzzleSolverFunctions.{find_landlocked, find_pos_zero, greybox, place_landlocked}

object PuzzleSolver extends App{

  def solveCustom(puzzle:Puzzle):Puzzle = {
    unsolvedToList()
    val start_class = new Puzzle(puzzle.sizeX, puzzle.sizeY, linjerList)

    val zeros = find_pos_zero(start_class,List(),0,0)
    println("Start:")
    start_class.puzzle.foreach(x => println(x))

    val greybox_class = greybox(start_class,zeros)
    println("After greybox:")
    greybox_class.puzzle.foreach(x => println(x))

    val landlocked_tiles = find_landlocked(greybox_class,List(),0,0)
    println(landlocked_tiles)
    println("After landlocked:")
    val landlocked_class = place_landlocked(greybox_class,landlocked_tiles)
    landlocked_class.puzzle.foreach(x => println(x))
    greybox_class
  }

  initRW("puzzleFiles/puzzle_unsolved.txt","puzzleFiles/puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolutionCustom(solveCustom(getPuzzleCustom(count)))
  }

  closing()
}