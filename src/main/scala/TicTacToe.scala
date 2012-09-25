
package net.brianvaughan.scala.ai.games.tictactoe

import net.brianvaughan.scala.ai.games._

import com.weiglewilczek.slf4s._

sealed abstract class Player(val char:String) {
  override def toString = char
}

case object X extends Player("X")
case object O extends Player("O")


class TicTacToeGameState(board:Seq[Option[Player]],
                         player:Player,
                         override val movesIntoGame:Int=0)
  extends ComputerPlayableGameState
{

  override def isWinner = isPlayerWinner(player)
  override def isLoser = isPlayerWinner(opponent)
  override def isTie = movesIntoGame >= 9

  def isPlayerWinner(p:Player) = {
    board match {
      case Seq(Some(`p`),Some(`p`),Some(`p`),_,_,_,_,_,_) => true
      case Seq(_,_,_,Some(`p`),Some(`p`),Some(`p`),_,_,_) => true
      case Seq(_,_,_,_,_,_,Some(`p`),Some(`p`),Some(`p`)) => true
      case Seq(Some(`p`),_,_,Some(`p`),_,_,Some(`p`),_,_) => true
      case Seq(_,Some(`p`),_,_,Some(`p`),_,_,Some(`p`),_) => true
      case Seq(_,_,Some(`p`),_,_,Some(`p`),_,_,Some(`p`)) => true
      case Seq(Some(`p`),_,_,_,Some(`p`),_,_,_,Some(`p`)) => true
      case Seq(_,_,Some(`p`),_,Some(`p`),_,Some(`p`),_,_) => true
      case _ => false
    }
  }


  override def preFetchDeep = Unit
  override def preFetchShallow = Unit

  override def gameStateDescription = {
    if( isWinner ) {
      player + " has won!"
    } else if ( isLoser ) {
      opponent + " has won!"
    } else if( isTie ) {
      "no more moves!"
    } else {
      "still playing"
    }
  }

  override def allPossibleResultingGameStates = {
    allLegalMoves map(move)
  }

  private def allLegalMoves = {
    (0 to 8).filter{ x => isLegalMove(x) }
  }

  private def isLegalMove(index:Int) = {
     (! gameOver) && (pieceAt(index) match {
       case Some(x) => false
       case None => true
     })
  }

  def move(index:Int) = {
    if( ! isLegalMove(index) ) {
      throw new InvalidMoveException("index is full!")
    }
    val newBoard = board.toArray.clone
    newBoard.update(index,Some(player))
    new TicTacToeGameState(newBoard,opponent,movesIntoGame+1)
  }

  //"board" is 3x3
  private def index(x:Int,y:Int) = x + y * 3
  private def getX(index:Int) = index % 3
  private def getY(index:Int) = index / 3
  private def pieceAt(index:Int) = board(index):Option[Player]

  private lazy val opponent = player match {
    case X => O
    case O => X
  }


  override def toString = {
    var output = "move: " + movesIntoGame
    output += "\nPlayer: " + player +
    "\nScore: " + evaluate

    for( j <- 2 to 0 by -1;
         i <- 0 to 2) {
      if( i == 0 ) {
        output += "\n"
      }
      output += (board(index(i,j)) match {
        case Some(x) => x.toString + "|"
        case None => " |"
      })
    }
    output
  }
  override def evaluate = { 
    if( isWinner ) { 9999999 } 
    else if( isLoser ) { -9999999 }
    else 0
  }
}

object TicTacToeGameState {

  private lazy val squares = List.fill(3*3)(None).toArray

  val startingBoard = new TicTacToeGameState(squares,O)

  def watchAnIterativeDeepeningGame(millis:Long) = {
    val player = new MiniMaxGamePlayer
    var board:ComputerPlayableGameState = startingBoard
    while( ! board.gameOver ) {
      board = player.bestMoveIterativeDeepening(board,millis,9)
      println(board)
    }
    println("Game Over")
    println(board.gameStateDescription)
    board
  }
}





