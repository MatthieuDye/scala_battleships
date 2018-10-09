package battleships

import BattleshipsUtils._

case class Cell(xc: Int, yc: Int) {
  val x: Int = xc
  val y: Int = yc

  override def toString: String = "(" + intToChar(x) + ", " + (y+1) + ")"

  override def equals(o: Any): Boolean = {
    //TODO: check if Char.equals(Char) works
    o != null && o.isInstanceOf[Cell] && o.asInstanceOf[Cell].x == this.x && o.asInstanceOf[Cell].y.equals(this.y)
  }

  def adjacents() : List[Cell] = {
    val list = Cell(x-1, y) :: Cell(x, y-1) :: Cell(x+1, y) :: Cell(x,y+1) :: Nil
    list.filter(cell => cell.x <10 && cell.x >=0 && cell.y <10 && cell.y >= 0)
  }

}