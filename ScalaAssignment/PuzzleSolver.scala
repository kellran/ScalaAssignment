import PuzzleReaderWriter._
import PuzzleSolverFunctions._

object PuzzleSolver extends App{
  def Solve(puzzle:Puzzle):Puzzle = {

    // Puzzle String to List() and setup start class:
    val puzzleList = unsolvedToList()
    val startPuzzle = new Puzzle(puzzle.sizeX, puzzle.sizeY, puzzleList)
    println("Start:")
    startPuzzle.puzzle.foreach(x => println(x))


    // Run main algorithm
    val finalPuzzle = mainAlgorithm(startPuzzle)
    println("After main algorithm: ")
    finalPuzzle.puzzle.foreach(x => println(x))


    // Check if the algorithm left the puzzle unfinished
    // if so, run bruteforce
    val remainingWhites = findPosOfChar(finalPuzzle,List(),0,0,'_')
    if(remainingWhites.nonEmpty){
      println("Algorithm not enough, forced to bruteforce! \n")
      val bruteforce = bruteforceAlgorithm(finalPuzzle, startPuzzle)
      println("After bruteforce: ")
      bruteforce.puzzle.foreach(x => println(x))

      // extract the lamp position from the end puzzle, and put lamps
      // in the corresponding positions of the start puzzle.
      val solutionLamps = findPosOfChar(finalPuzzle,List(),0,0,'*')
      val solution = solutionFinisher(bruteforce,solutionLamps)
      solution
    }
    else{
      // extract the lamp position from the end puzzle, and put lamps
      // in the corresponding positions of the start puzzle.
      val solutionLamps = findPosOfChar(finalPuzzle,List(),0,0,'*')
      val solution = solutionFinisher(startPuzzle,solutionLamps)
      solution
    }
  }

  initRW("puzzle_unsolved.txt", "puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()
  println("Solving puzzle #1")
  putSolution(Solve(getPuzzle(0)))

  closing()
}