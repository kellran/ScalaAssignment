class Puzzle(x:Int, y:Int, puzzleList: List[List[Char]]) {

  val sizeX=x;
  val sizeY=y;
  val puzzle = puzzleList

  def setChar(row:Int, column:Int, char:Char): Puzzle ={
    val ny_liste = puzzle.updated(row, puzzle(row).updated(column, char))
    return new Puzzle(sizeX,sizeY,ny_liste)
  }

  def setInt(savestate:Puzzle,row:Int, column:Int, sum:Int): Puzzle ={
    val savestatelist = savestate.puzzle
    val ny_liste = puzzle.updated(row, puzzle(row).updated(column, (savestatelist(row)(column).toInt - sum).toChar))
    return new Puzzle(sizeX,sizeY,ny_liste)
  }
}
