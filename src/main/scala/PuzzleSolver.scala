import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution, puzzlelist, unsolvedToList}
import PuzzleSolverFunctions.{all_number_pos, count_until_char, find_implicit_landlocked, find_landlocked, find_pos_of_char, greybox, light, lights, place_implicit, place_landlocked, sum_of_char, update_numbers}

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


    // Find all implicit landlocked tiles
    // and place lamps adjecent
    val numbers2 = all_number_pos(greybox_puzzle2)
    val impl_land = find_implicit_landlocked(greybox_puzzle2,numbers2,List())
    val impl_land_puzzle = place_implicit(greybox_puzzle2,impl_land)
    println(numbers2 + " " + impl_land)
    println("Implicit landlocked:")
    impl_land_puzzle.puzzle.foreach(x => println(x))

    // Find all numbers and update numbers
    val numbers3 = all_number_pos(impl_land_puzzle)
    val numbers_puzzle2 = update_numbers(impl_land_puzzle,start_puzzle,numbers3)
    println("Numbers update:")
    numbers_puzzle2.puzzle.foreach(x => println(x))

    // Re-run greybox
    val zeros3 = find_pos_of_char(numbers_puzzle2,List(),0,0,'0')
    val greybox_puzzle3 = greybox(numbers_puzzle2,zeros3)
    println("Greybox:")
    greybox_puzzle3.puzzle.foreach(x => println(x))


    // Cast light from lamps
    val lamps = find_pos_of_char(greybox_puzzle3,List(),0,0,'*')
    val light_puzzle = lights(greybox_puzzle3, lamps)
    println("Light:")
    light_puzzle.puzzle.foreach(x => println(x))

    light_puzzle
  }

  initRW("puzzleFiles/puzzle_unsolved.txt","puzzleFiles/puzzle_solved.txt")
  val numPuzzles=getNumPuzzles()


  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solveCustom(getPuzzle(count)))
  }

  closing()
}