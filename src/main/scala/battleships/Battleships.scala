package battleships

import battleships.BattleshipsUtils._
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random


case class GameState(attackingPlayer: Player, defendingPlayer: Player)

object Battleships extends App {

  val BOARD_SIZE = 10

  clear()
  println("Raise the sails !")

  var board = createRandomBoard()

  showGameMode()
  val gameModeChoice = getGameMode()
  print(s"You chose $gameModeChoice. \n")

  gameModeChoice match {
    case 0 =>
      print("Ask for name of player 1. \n")
      val player1 = Player(readerUserString(), board)
      print(s"Vous êtes le joueur ${player1.name()}. \n")

      print("Ask for name of player 2. \n")
      val player2 = Player(readerUserString(), board)
      print(s"Vous êtes le joueur ${player2.name()}. \n")


      val gamePvP = GameState(player1, player2)
      mainLoop(gamePvP)
    case 1 | 2 | 3 =>
      print("Ask for name of humain player. \n")
      val player = Player(readerUserString(), board)
      print(s"You are the cap'tain ${player.name()}. \n")

      val gamePvAI = GameState(player, new Computer(gameModeChoice))
      mainLoop(gamePvAI)

    case 9 =>
      val ai1 = new Computer(1)
      val ai2 = new Computer(2)
      val ai3 = new Computer(3)

      testsAI(1, ai1, ai2)
  }


  @tailrec
  def mainLoop(state: GameState): Unit = {

    //println(s"Turn of player ${state.attackingPlayer}")



    var userInput: String = ""

    if (!state.attackingPlayer.isInstanceOf[Computer]) {
      println("Do you want to continue ? (y/n)")
      userInput = readerUserString()
    } else {
      userInput = "y"
    }


    userInput match {
      case "y" | "Y" =>
        println(state.defendingPlayer.userBoard().ships())

        val targetCell = state.attackingPlayer.askForShot()
        //println(targetCell)

        if (isHit(state.defendingPlayer, targetCell)) {
          //println("You hit it!")

          val updatedAttackingPlayer = state.attackingPlayer.updateHit(targetCell)

          //println(updatedAttackingPlayer.attacksBoard().hitCells())
          println(updatedAttackingPlayer.attacksBoard().cellsToHit())

          val nextGameState = GameState(state.defendingPlayer, updatedAttackingPlayer)

          // compute state

          if (updatedAttackingPlayer.attacksBoard().hitCells().size == 5+4+3+3+2) println(s"End of game : ${state.attackingPlayer} won against ${state.defendingPlayer}")
          else { // if updatedAttackingPlayer.attacksBoard().hitCells().size = 5+4+3+3+2
            mainLoop(nextGameState)
          }
        } else {
          //println("It's a miss...")

          val updatedAttackingPlayer = state.attackingPlayer.updateMiss(targetCell)

          val nextGameState = GameState(state.defendingPlayer, updatedAttackingPlayer)
          mainLoop(nextGameState)
        }
      case _ => println("Game Stopped")
    }
  }


  /** API - random board */
  def createRandomBoard(): UserBoard = {
    val now = java.util.Calendar.getInstance().getTime.getTime
    val rand = new Random(now)
    var board = createEmptyUserBoard()

    val fleet = List(Carrier(generateRandomOrientation(rand)), Battleship(generateRandomOrientation(rand)), Cruiser(generateRandomOrientation(rand)), Submarine(generateRandomOrientation(rand)), Patrol(generateRandomOrientation(rand)))

    //TODO: create new userboard for each ship
    fleet.foreach(ship => {
      board = randomPlaceShip(rand, board, ship)
    })
    board
  }

  /** API - empty board */
  def createEmptyUserBoard(): UserBoard = new UserBoard()

  def createEmptyAttacksBoard(): AttacksBoard = new AttacksBoard()


  private def randomPlaceShip(rand: Random, board: UserBoard, ship: Ship): UserBoard = {
    val startingCell = generateRandomCell(rand)
    val newBoard = board.placeShip(ship, startingCell)
    if (newBoard.ships().equals(board.ships())) {
      randomPlaceShip(rand, board, ship)
    } else newBoard
  }

  def getBoardSize: Int = BOARD_SIZE

  def isHit(defender: Player, targetedCell: Cell): Boolean = {
    defender.userBoard().isHit(targetedCell)

  }

  //TODO: API - game state */
  /** API - places ship */
  def placeShip(board: UserBoard, ship: Ship, pos: Cell): UserBoard = board.placeShip(ship, pos)

  @tailrec
  def testsAI(recursion: Int, ai1: Computer, ai2: Computer) : Unit = {

    println(s"Need to complete Game ${recursion}")
    mainLoop(new GameState(ai1, ai2))
    clear()
    testsAI(recursion-1, ai2, ai1)


  }

}