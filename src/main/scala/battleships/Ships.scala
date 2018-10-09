package battleships


abstract class Ship(dir: Direction.Value, len: Int) {
  val _direction = dir
  val _length = len

  def direction() : Direction.Value = _direction

  def length() : Int = _length
}

case class Carrier(dir: Direction.Value) extends Ship(dir, 5)
case class Battleship(dir: Direction.Value) extends Ship(dir, 4)
case class Submarine(dir: Direction.Value) extends Ship(dir, 3)
case class Cruiser(dir: Direction.Value) extends Ship(dir, 3)
case class Destroyer(dir: Direction.Value) extends Ship(dir, 2)