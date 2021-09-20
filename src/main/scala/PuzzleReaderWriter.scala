
import java.io.FileWriter
import scala.io._

object PuzzleReaderWriter{
  var unsolvedFile:String="";
  var solvedFile:String="";
  var lines:List[String]=Nil;
  var linesArray:Array[String] = Array("")
  var puzzlelist:List[List[Char]] = List(List())

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

  def getPuzzle(index:Int):Puzzle={
    val sizeNumbers=lines.filter(_ startsWith("size"))(index).split(" ").last.split("x")
    return new Puzzle(sizeNumbers(0).toInt,sizeNumbers.last.toInt,List(List()))
  }

  def unsolvedToList(): Unit ={
    puzzlelist = lines
      .map(x => x.toList)
      .drop(2)
  }

  /*
  def solutionFormat(puzzle:Puzzle): Unit ={
    val templist = puzzle
    val finallist = {
      templist.puzzle.foreach(templist => templist.foreach(templist => println(templist)))
    }
    return finallist
  }
  */

  def putSolution(puzzle:Puzzle) ={
    fw.write("size "+puzzle.sizeX+"x"+puzzle.sizeY+"\n")
    val templist = puzzle.puzzle
    templist.foreach(templist => fw.write(templist.toString()
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