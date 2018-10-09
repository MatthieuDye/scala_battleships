package battleships

import BattleshipsUtils._

import scala.util.Random

case class Player(pseudo: String = "Anonymous", u: UserBoard = Battleships.createRandomBoard(), a: AttacksBoard = Battleships.createEmptyAttacksBoard()) {
  private val _userBoard = u
  private val _name = pseudo
  private val _attacksBoard = a

  override def toString: String = name()

  def name(): String = _name

  def userBoard(): UserBoard = _userBoard

  def attacksBoard(): AttacksBoard = _attacksBoard

  def numberShipsDestroyedBy(attackingPlayer: Player): Int = {
    var cpt = 0

    for (eachSetOfKey <- userBoard().ships().keySet) if (eachSetOfKey.forall(cell => {
      attackingPlayer.attacksBoard().alreadyHit(cell)
    })) {
      cpt = cpt + 1
    }
    cpt
  }

  def askForShot(): Cell = {
    Cell(charToInt(readerCoordChar()), readerCoordInt() - 1)
  }

  def updateHit(hitCell: Cell): Player = {
    if (!attacksBoard().alreadyHit(hitCell)) {
      Player(name(), userBoard(), attacksBoard().update(hitCell))
    } else {
      this
    }
  }

  def updateMiss (missedCell: Cell) : Player = {
    new Player(name(), userBoard(), attacksBoard().updateMiss(missedCell))
  }

}

class Computer(lvl: Int, userBoard: UserBoard = Battleships.createRandomBoard(), attacksBoard: AttacksBoard = Battleships.createEmptyAttacksBoard()) extends Player(pseudo = "Default IA", userBoard, attacksBoard) {
  private val _level = lvl

  def level(): Int = _level

  override def toString: String = name + " level " + level()

  override def askForShot(): Cell = level() match {
    case 1 => generateRandomCell(new Random())
    case 2 => val tryCell = generateRandomCell(new Random())
      if (this.attacksBoard().hitCells().contains(tryCell)) askForShot() else tryCell
    case 3 =>
      if ( attacksBoard().cellsToHit().isEmpty) generateRandomCell(new Random())
      else  attacksBoard().cellsToHit().head
      //TODO: can be updated in the case the head of cellsToHis has already benn hit and is in hitCells
  }

  override def updateHit(hitCell: Cell): Computer = { // because of events, i'm sure the hitCell has hit a ship. This function has to create a new Computer which knows one of the hit cells is hitCell
    level match {
      case 1 | 2 =>
        println("Basis AI : no needs to change")
        new Computer(level(), userBoard(), attacksBoard().update(hitCell))
      case 3 =>
        if (!attacksBoard().alreadyHit(hitCell)) {
        println("Hit to hit maj")
        new Computer(level(), userBoard(), attacksBoard().updateHitToHit(hitCell))
      } else {
        println("It is a miss, so no changes")
        this
      }

    }
  }

  override def updateMiss(missedCell: Cell): Player = {
    level match {
      case 1 => this
      case 2 | 3 => new Computer(level(), userBoard(), attacksBoard().updateMiss(missedCell) )
    }
  }
}