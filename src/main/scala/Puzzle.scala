// Puzzle class represents the problem and also the solution
class Puzzle(x:Int, y:Int, sol:String) { // just trivial data here
  val sizeX=x;
  val sizeY=y;
  val solution=sol;

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}
