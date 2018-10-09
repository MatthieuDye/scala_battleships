package battleships

import scala.language.implicitConversions

class UserBoard(map: Map[List[Cell], Ship] = Map[List[Cell], Ship]() ) {

  private val _ships = map

  def ships(): Map[List[Cell], Ship] = _ships

  def update(newBoard: UserBoard) = { new UserBoard(newBoard.ships())}

  def placeShip(ship: Ship, startingCell: Cell): UserBoard = {

    // generate ship coordinates vector:
    val newShipGenerator = ship.direction() match {
      case Direction.HORIZONTAL => for (i <- 0 until ship.length) yield Cell(startingCell.x + i, startingCell.y)
      case Direction.VERTICAL => for (i <- 0 until ship.length) yield Cell(startingCell.x, startingCell.y + i)
    }

    val newShipCells = newShipGenerator.toList
    //println(newShipCells)
    val setOfCells = ships().keySet.flatten.toList
    //println(setOfCells)

    if (newShipCells.exists(newCell => newCell.x < 0 || newCell.y < 0 || newCell.x >= Battleships.getBoardSize || newCell.y >= Battleships.getBoardSize)) {
      this
    } else if (newShipCells.exists(cellWith => containsAtLeastACell(newShipCells, setOfCells))) {
      this
    } else {
      val newMap = Map(newShipCells -> ship)
      new UserBoard( ships() ++ newMap )
    }
  }

  def isHit(targetedCell: Cell): Boolean = {
    ships().keySet.flatten.contains(targetedCell)
  }

  def containsAtLeastACell(listCells1: List[Cell], listCells2: List[Cell]): Boolean = {
    if (listCells1.isEmpty || listCells2.isEmpty) {
      false
    } else if (listCells2.exists(cell => listCells1.contains(cell)) || listCells1.exists(cell => listCells2.contains(cell))) {
      true
    } else {
      false
    }
  }
}

class AttacksBoard(shots: List[Cell] = List[Cell](), water: List[Cell] = List[Cell](),  toShot: List[Cell] = List[Cell]())  {

  private val _hitCells = shots

  private val _missedCells = water

  private val _toShot = toShot

  def hitCells(): List[Cell] = _hitCells

  def cellsToHit() : List[Cell] = _toShot

  def cellsMissed() : List[Cell] = _missedCells

  def alreadyHit(cell: Cell) : Boolean = {
    hitCells().contains(cell)
  }

  def update(hitCell: Cell) : AttacksBoard = { new AttacksBoard(hitCells() :+ hitCell)}

  def updateHitToHit(hitCell: Cell) : AttacksBoard = {
    if (!cellsToHit().contains(hitCell)) { // it was a randomly generated cell. I add this cell to hitCells and I want to look after the adjacents
      println("It was a randomly generated cell")
     new AttacksBoard(hitCells() :+ hitCell, cellsMissed() ,hitCell.adjacents() ++: cellsToHit())
    } // If the cell wasn't randomly generated, it's because she belongs to cellsToHit. In that case, I should take cellToHit tails and add adjacents I have not already attacked
    else {
      println("It was a part of cells to hit")
      var tempList = List[Cell]()
      hitCell.adjacents().foreach(cell => if (!hitCells().contains(cell)) tempList = tempList :+ cell)
      new AttacksBoard(hitCells() :+ hitCell, cellsMissed(), tempList ++: cellsToHit().tail)
    }
  }

  def updateMiss(missCell: Cell) : AttacksBoard = {
    if (cellsToHit().contains(missCell)) new AttacksBoard(hitCells(), cellsMissed() :+ missCell ,cellsToHit().tail)
    else new AttacksBoard(hitCells(), cellsMissed() :+ missCell, cellsToHit())
  }
}



