import PuzzleReaderWriter._
import PuzzleSolverFunctions._

object PuzzleSolver extends App{

  def solveCustom(puzzle:Puzzle):Puzzle = {
    // Puzzle String to List() and start class:
    unsolvedToList()
    val start_puzzle = new Puzzle(puzzle.sizeX, puzzle.sizeY, puzzlelist)
    val finalpuzzle = main_algorithm(start_puzzle)
    val solutionlamps = find_pos_of_char(finalpuzzle,List(),0,0,'*')
    val solution = solutionFinisher(start_puzzle,solutionlamps)
    solution
  }

  initRW("puzzle_unsolved.txt","puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solveCustom(getPuzzle(count)))
  }

  closing()
}