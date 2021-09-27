import PuzzleReaderWriter._
import PuzzleSolverFunctions._

object PuzzleSolver extends App{
  def Solve(puzzle:Puzzle):Puzzle = {

    // Puzzle String to List() and setup start class:
    unsolvedToList()
    val start_puzzle = new Puzzle(puzzle.sizeX, puzzle.sizeY, puzzlelist)
    println("Start:")
    start_puzzle.puzzle.foreach(x => println(x))

    // Run main algorithm
    val finalpuzzle = mainAlgorithm(start_puzzle)
    println("After main algorithm: ")
    finalpuzzle.puzzle.foreach(x => println(x))


    // Check if the algorithm left the puzzle unfinished
    // if so, run bruteforce
    val remainingWhites = findPosOfChar(finalpuzzle,List(),0,0,'_')
    if(remainingWhites.nonEmpty){
      println("Algorithm not enough, forced to bruteforce! \n")
      val bruteforce = bruteforceAlgorithm(finalpuzzle, start_puzzle, remainingWhites)
      println("After bruteforce: ")
      bruteforce.puzzle.foreach(x => println(x))

      // extract the lamp posistion from the end puzzle, and put lamps
      // in the corresponding posistions of the start puzzle.
      val solutionlamps = findPosOfChar(finalpuzzle,List(),0,0,'*')
      val solution = solutionFinisher(bruteforce,solutionlamps)
      solution
    }
    else{
      // extract the lamp posistion from the end puzzle, and put lamps
      // in the corresponding posistions of the start puzzle.
      val solutionlamps = findPosOfChar(finalpuzzle,List(),0,0,'*')
      val solution = solutionFinisher(start_puzzle,solutionlamps)
      solution
    }
  }

  initRW("puzzle_unsolved.txt","puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()

  println("Solving puzzle #1")
  putSolution(Solve(getPuzzle(0)))

  closing()
}