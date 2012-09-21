
package net.brianvaughan.scala.ai.games.connect4

import net.brianvaughan.scala.ai.games._

import com.weiglewilczek.slf4s._



import scala.util.matching.Regex
sealed abstract class EvaluatorRegex(val player:Color,val value:Double) {
  val regex:Regex
}

case class TwoOpenEnded(override val player:Color) 
  extends EvaluatorRegex(player,2.0) {
  override val regex = (" " + player.color + player.color + " ").r
}

case class ThreeOpenEnded(override val player:Color) 
  extends EvaluatorRegex(player,6.0) {
  override val regex=(" " + player.color + player.color + player.color + " ").r
}

//this checks for avaialble space to put pieces of a specified size (all either
//our pieces or empty), with at least `required` pieces already being filled 
//with our own pieces.
case class AvailableSpace(override val player:Color,
                          size:Int,
                          required:Int,
                          override val value:Double)
  extends EvaluatorRegex(player,value) {
  override val regex =( "(?=["+player.color+" ]{"+size+"})"+
                ( ".*" + player.color.mkString(".*")+".*" )
                ).r
}


sealed abstract class Color(val color:String) {
  val evaluatorRegexes:Seq[EvaluatorRegex] = 
      List(
           AvailableSpace(this,4,1,0.2),
           AvailableSpace(this,4,2,2.0),
           AvailableSpace(this,4,3,5.0),
           AvailableSpace(this,5,3,5.0),
           AvailableSpace(this,6,4,4.0),
           AvailableSpace(this,7,5,5.0)
           )

  override def toString = color
}
case object Yellow extends Color("y")
case object Red extends Color("r")


/**
 * Models the state of a game of Connect4.
 */
class ConnectFourGameState(board:Seq[Option[Color]],
                  player:Color,
                  override val movesIntoGame:Int=0) 
  extends ComputerPlayableGameState 
{
  val logger = Logger("C4BoardState")


  override def isWinner = isPlayerWinner(player)
 
  override def isLoser = isPlayerWinner(opponent)

  override def isTie = movesIntoGame >= (7*6)


  private def isPlayerWinner(p:Color):Boolean = {
    val winString = p.color * 4
    allStrings.foreach{
      s => if ( s contains winString ) {
        return true
      }
    }
    false
  }

  //get rows as lists of option[color]'s, to scan for winning or good 
  //board positions.
  private val rows = {
    board.grouped(7).toSeq
  }

  //gets the vertical columns to look for winning board states/evaluation
  private val columns:Seq[Seq[Option[Color]]] = {
    (0 to 6) map {
      case column => {
        (0 to 5) map { case row => pieceAt(index(column,row)) }
      }
    }
  }

  //SW to NE diagonals
  private val diagonalsSwNe:Seq[Seq[Option[Color]]] = {
    (-2 to 3) map {
      case startColumn =>
        (0 to 5).foldLeft(List[Option[Color]]()) {
        case (acc,currentRow) => {
          val c = startColumn + currentRow
          if( c < 0 || c > 6 || currentRow < 0 || currentRow > 5 ){
            acc
          } else {
            pieceAt(index(c,currentRow)) :: acc
          }
        }
      }
    }
  }

  //SE to NW
  private val diagonalsSeNw:Seq[Seq[Option[Color]]] = {
    (3 to 8) map {
      case startColumn => 
        (0 to 5).foldLeft(List[Option[Color]]()) {
        case (acc,currentRow) => {
          val c = startColumn - currentRow
          if( c < 0 || c > 6 || currentRow < 0 || currentRow > 5 ){
            acc
          } else {
            pieceAt(index(c,currentRow)) :: acc
          }
        }
      }
    }
  }

  //just concatenate all of the lists of ordered position sequences
  //so all can be scanned for winning/good sequences.
  private def allPieceSequences:Seq[Seq[Option[Color]]] = {
    rows ++ columns ++ diagonalsSwNe ++ diagonalsSeNw
  }

  //convert all of the piece sequences to strings so we can use regexes
  private def allStrings = {
    allPieceSequences map {
      case s => holesToString(s)
    }
  }

  //convert a position (a hole in the connect4 board) to a string
  //for printing or for string matching.
  private def holesToString(holes: Seq[Option[Color]]) = {
    holes.foldLeft("") {
      case (s,p) => s + (p match {
        case None=> " "
        case Some(c) => c.toString
      })
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
    (0 to 6).filter{ x => isLegalMove(x) }
  }

  private def isLegalMove(column:Int) = {
    ! columnFull(column) && ! gameOver
  }

  override def evaluate:Double = {
    if( isWinner ) {
      999999999
    } else if( isLoser ) {
      -999999999
    } else {
      allStrings.foldLeft(0.0) {
        case (acc,s) => {
          acc +
          player.evaluatorRegexes.foldLeft(0.0) {
            case (innerAcc,e) => 
                innerAcc + 
                e.regex.findAllIn(s).length * e.value
          }
        }
      }
    }
  }

  def move(column:Int) = {
    if( ! isLegalMove(column) ) {
      throw new InvalidMoveException("column is full!")
    }
    val destination = (5 to 0 by -1).foldLeft(5){
      (result,row) => {
        pieceAt(index(column,row)) match {
          case None => row
          case Some(x) => result
        }
      }
    }
    val newBoard = board.toArray.clone
    newBoard.update(index(column,destination),Some(player))
    new ConnectFourGameState(newBoard,opponent,movesIntoGame+1)
  }
  
  //"board" is 7 wide and 6 high
  private def index(x:Int,y:Int) = x + y * 7
  private def getX(index:Int) = index % 7
  private def getY(index:Int) = index / 7
  private def pieceAt(index:Int) = board(index):Option[Color]
  
  private def columnFull(column:Int) = pieceAt(index(column,5)) match {
    case Some(x) => true
    case None => false
  }

  private lazy val opponent = player match {
    case Yellow => Red
    case Red => Yellow
  }

  override def toString = {
    var output = "move: " + movesIntoGame
    output += "\nPlayer: " + player.color +
    "\nScore: " + evaluate

    for( j <- 5 to 0 by -1;
         i <- 0 to 6) {
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
}

object ConnectFourGameState {

  private lazy val holes = List.fill(7*6)(None).toArray
  val startingBoard = {
    new ConnectFourGameState(holes,Red)
  }
  
  def watchAnIterativeDeepeningGame(millis:Long) = {
    val player = new MiniMaxGamePlayer
    var board:ComputerPlayableGameState = startingBoard
    while( ! board.gameOver ) {
      board = player.bestMoveIterativeDeepening(board,millis)
      println(board)
    }
    println("Game Over")
    println(board.gameStateDescription)
    board
  }

}

