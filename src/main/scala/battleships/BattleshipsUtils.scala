package battleships

import scala.util.Random
import scala.io.StdIn.readLine
import scala.sys.process._

object BattleshipsUtils {

  def clear(): Int = "clear".!

  def showGameMode() : Unit = {
    print("Choose the game mode !\n")
    print("P v P(0), P v Ai level 1(1), P v AI level 2(2), P v AI level 3(3)\n")

  }

  def getGameMode() : Int = {
    val intValue = readLine().trim()
    intValue match {
      case "0" | "1" | "2" | "3" | "9" => intValue.toInt
      case _ =>
        print("The game is expecting you to choose a number between parenthesis. Try again.\n")
        getGameMode()
    }
  }

  // use it minus 1
  def readerCoordInt() : Int = {
    print("Choose a digit between 1 and 10.\n")
    val intValue = readLine().trim()
    intValue match {
      case  "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "10" => intValue.toInt
      case _ =>
        print("The game is expecting you to choose a number between 1 and 10. Try again.\n")
        readerCoordInt()
    }
  }

  def readerCoordChar() : Char = {
    print("Choose a letter between A and J.\n")
    val charValue = readLine().trim().toUpperCase()
    charValue match {
      case "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" => charValue.charAt(0)
      case _ =>
        print("The game is expecting you to choose only one letter between A and J. Try again.\n")
        readerCoordChar()
    }
  }

  def readerUserString() : String = {
    val stringValue = readLine().trim()
    stringValue
  }

  def charToInt(c: Char) : Int = c match {
    case 'A' => 0
    case 'B' => 1
    case 'C' => 2
    case 'D' => 3
    case 'E' => 4
    case 'F' => 5
    case 'G' => 6
    case 'H' => 7
    case 'I' => 8
    case 'J' => 9


  }


  def intToChar(i: Int) : Char = i match {
    case 0 => 'A'
    case 1 => 'B'
    case 2 => 'C'
    case 3 => 'D'
    case 4 => 'E'
    case 5 => 'F'
    case 6 => 'G'
    case 7 => 'H'
    case 8 => 'I'
    case 9 => 'J'
    case _ => 'u'


  }

  def generateRandomCell(rand: Random): Cell = {
    val x = rand.nextInt(Battleships.getBoardSize)
    val y = rand.nextInt(Battleships.getBoardSize)
    Cell(x, y)
  }

  def generateRandomOrientation(rand: Random): Direction.Value = {
    if (rand.nextBoolean()) {
      Direction.VERTICAL
    } else {
      Direction.HORIZONTAL
    }
  }


}
