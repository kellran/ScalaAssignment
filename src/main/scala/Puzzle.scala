class Puzzle(x:Int, y:Int, puzzleList: List[List[Char]]) {

  val sizeX=x;
  val sizeY=y;
  val puzzle = puzzleList

  def setChar(row:Int, colum:Int, char:Char): Puzzle ={
    val ny_liste = puzzle.updated(row, puzzle(row).updated(colum, char))
    return new Puzzle(sizeX,sizeY,ny_liste)
  }
}