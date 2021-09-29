import PuzzleSolverFunctions.charIfValid
import java.io.FileWriter
import scala.io._

object PuzzleReaderWriter{
  var unsolvedFile:String="";
  var solvedFile:String="";
  var lines:List[String]=Nil;
  var fw:FileWriter=null;

  def initRW(infile:String, outfile:String)={
    unsolvedFile=infile
    solvedFile=outfile
    lines = Source.fromFile(unsolvedFile).getLines().toList
    fw = new FileWriter(solvedFile, false)
  }

  def getPuzzle(index:Int):Puzzle={
    val sizeNumbers=lines.filter(_ startsWith("size"))(index).split(" ").last.split("x")
    return new Puzzle(sizeNumbers(0).toInt,sizeNumbers.last.toInt,List(List()))
  }

  // returns a lists of chars based on the unsolved.txt file
  def unsolvedToList(): List[List[Char]] ={
    val puzzleList:List[List[Char]] = lines
      .map(x => x.toList)
      .drop(2)
    return puzzleList
  }

  // inserts lamps into the original unaltered puzzle.
  def solutionFinisher(savedState:Puzzle, lampPos:List[(Int,Int)]): Puzzle ={
    val x = savedState.sizeX
    val y = savedState.sizeY
    if (lampPos.nonEmpty) {
      val row = lampPos.head._1
      val column = lampPos.head._2

      val tempPuzzle = charIfValid(savedState, row, column, x, y ,'*')
      return solutionFinisher(tempPuzzle,lampPos.drop(1))
    }
    return savedState
  }

  // Writes solution to txt file
  def putSolution(puzzle:Puzzle) ={
    fw.write("size "+puzzle.sizeX+"x"+puzzle.sizeY+"\n")
    val tempList = puzzle.puzzle
    tempList.foreach(templist => fw.write(templist.toString()
      .replace("List","")
      .replace("(","")
      .replace(")","")
      .replace(" ","")
      .replace(",","") + "\n"))
  }

  def closing()={
    fw.close()
  }
}