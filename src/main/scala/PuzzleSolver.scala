import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzleCustom, initRW, linjerList, putSolutionCustom, unsolvedToList}
import PuzzleSolverFunctions.{find_pos_zero, greybox}

object PuzzleSolver extends App{

  def solveCustom(puzzle:Puzzle):Puzzle = {
    unsolvedToList()
    val test = new Puzzle(puzzle.sizeX, puzzle.sizeY, linjerList)

    val updated = find_pos_zero(test,List(),0,0)
    println("Before:")
    println(updated)
    test.puzzle.foreach(x => println(x))

    val greybox_updated = greybox(test,updated)
    println("After:")
    greybox_updated.puzzle.foreach(x => println(x))

    greybox_updated
  }

  initRW("puzzleFiles/puzzle_unsolved.txt","puzzleFiles/puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolutionCustom(solveCustom(getPuzzleCustom(count)))
  }

  closing()
}