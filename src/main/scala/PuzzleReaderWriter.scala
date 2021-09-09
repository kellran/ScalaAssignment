
import java.io.FileWriter
import scala.io._

object PuzzleReaderWriter{
  var unsolvedFile:String="";
  var solvedFile:String="";
  var lines:List[String]=Nil;
  var linesArray:Array[String] = Array("")
  var linesArrayOfArray:Array[Array[Char]]= Array(Array());
  var fw:FileWriter=null;


  def initRW(infile:String, outfile:String)={
    unsolvedFile=infile
    solvedFile=outfile
    lines = Source.fromFile(unsolvedFile).getLines().toList
    fw = new FileWriter(solvedFile, false)
  }

  def getNumPuzzles():Int={
    val countPuzzles=lines(0).split(" ").last.toInt
    // writing number of puzzles into solution
    fw.write("puzzles "+countPuzzles.toString+"\n")
    return countPuzzles
  }

  def unsolvedToArray(): Unit ={
    linesArray = lines.toArray;
    linesArrayOfArray = linesArray.map(x => x .toArray)
    linesArrayOfArray
  }

  def getPuzzle(index:Int):Puzzle={
    val sizeNumbers=lines.filter(_ startsWith("size"))(index).split(" ").last.split("x")
    return new Puzzle(sizeNumbers(0).toInt,sizeNumbers.last.toInt,"")
  }

  def putSolution(puzzle:Puzzle)={
    fw.write("size "+puzzle.sizeX+"x"+puzzle.sizeY+"\n")
    fw.write(puzzle.solution+"\n")
  }

  def closing()={
    fw.close()
  }
}