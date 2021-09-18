class LightUpClass(x:Int, y:Int, puzzleList: List[List[Char]]) {

  val sizeX=x;
  val sizeY=y;
  val puzzle = puzzleList

  def setChar(row:Int,colum:Int, bokstav:Char): LightUpClass ={
    val ny_liste = puzzle.updated(row, puzzle(row).updated(colum, bokstav))
    return new LightUpClass(sizeX,sizeY,ny_liste)
  }
}