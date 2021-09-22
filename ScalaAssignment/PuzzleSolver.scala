import PuzzleReaderWriter._
import PuzzleSolverFunctions._

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

    // find grey implicits and place lamps in the white tile
    val greyboxes = find_pos_of_char(light_puzzle,List(),0,0,'G')
    val grey_implicits = find_implicit_white_grey(light_puzzle,greyboxes,List(), 1)
    println(grey_implicits)
    val grey_implicit_puzzle = place_implicits(light_puzzle,grey_implicits)
    println("grey implicits:")
    grey_implicit_puzzle.puzzle.foreach(x => println(x))

    // find white implicits and place lamps in the white tile
    val whiteboxes = find_pos_of_char(grey_implicit_puzzle,List(),0,0,'_')
    val white_implicits = find_implicit_white_grey(grey_implicit_puzzle,whiteboxes,List(), 0)
    println(white_implicits)
    val white_implicit_puzzle = place_implicits(grey_implicit_puzzle,white_implicits)
    println("white implicits:")
    white_implicit_puzzle.puzzle.foreach(x => println(x))

    // Re-cast light from lamps
    val lamps2 = find_pos_of_char(white_implicit_puzzle,List(),0,0,'*')
    val light_puzzle2 = lights(white_implicit_puzzle, lamps2)
    println("Light:")
    light_puzzle2.puzzle.foreach(x => println(x))

    // find grey implicits and place lamps in the white tile
    val greyboxes2 = find_pos_of_char(light_puzzle2,List(),0,0,'G')
    val grey_implicits2 = find_implicit_white_grey(light_puzzle2,greyboxes2,List(), 1)
    println(grey_implicits2)
    val grey_implicit_puzzle2 = place_implicits(light_puzzle2,grey_implicits2)
    println("grey implicits:")
    grey_implicit_puzzle2.puzzle.foreach(x => println(x))


    // Re-cast light from lamps
    val lamps3 = find_pos_of_char(grey_implicit_puzzle2,List(),0,0,'*')
    val light_puzzle3 = lights(grey_implicit_puzzle2, lamps3)
    println("Light:")
    light_puzzle3.puzzle.foreach(x => println(x))

    val solutionlamps = find_pos_of_char(light_puzzle3,List(),0,0,'*')
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