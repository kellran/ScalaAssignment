import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, getPuzzleCustom, initRW, linesArrayOfArray, linjerList, putSolution, putSolutionCustom, solvedFile, unsolvedToList}
import PuzzleSolverFunctions.{find_pos_zero, greybox, placeGreyBox, placeLightBulb}

object PuzzleSolver extends App{ // This is the main file

  def solveCustom(puzzle:LightUpClass):LightUpClass = {
    unsolvedToList()
    val test = new LightUpClass(puzzle.sizeX, puzzle.sizeY, linjerList)

    val updated = find_pos_zero(test,List(),0,0)
    println("Before:")
    println(updated)
    test.puzzle.foreach(x => println(x))

    val greyboxclass = greybox(test,updated)
    println("After:")
    greyboxclass.puzzle.foreach(x => println(x))

    test
  }

/*
  def solve(puzzle:Puzzle):Puzzle = {
    // we predefine just two solutions
    val solution7x7 =
        "* _ _ _ _ _ _\n"+
        "2 _ _ * _ _ _\n"+
        "* 1 _ 2 0 _ *\n"+
        "X _ _ * _ _ _\n"+
        "X _ * _ _ _ _\n"+
        "* _ _ _ _ 0 _\n"+
        "2 * X _ * _ _"
    val solution10x5 =
        "_ _ _ _ * _ _ _ 1 *\n"+
        "_ * 1 0 _ * X _ 0 _\n"+
        "_ _ _ _ _ _ _ * _ 1\n"+
        "_ _ * 2 _ 1 * _ X *\n"+
        "_ _ _ * _ _ _ 1 * _"
    val size=puzzle.sizeX*100+puzzle.sizeY
    val solution = size match {
      case 707 => solution7x7
      case 1005 => solution10x5
      case _   => "cannot solve this puzzle"
    }
    // gjør string til array
    unsolvedToArray()
    new testClass(puzzle.sizeX, puzzle.sizeY,unsolvedToList(puzzle))
    // fjerner to første linjene
    linesArrayOfArray = linesArrayOfArray.drop(2)
    //println(linesArrayOfArray(0)(0))
    //println(getNumPuzzles())
    //println(getPuzzle(1))

    return new Puzzle(puzzle.sizeX, puzzle.sizeY, solution)
  }
 */
  initRW("puzzleFiles/puzzle_unsolved.txt","puzzleFiles/puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolutionCustom(solveCustom(getPuzzleCustom(count)))
  }

  closing()
}