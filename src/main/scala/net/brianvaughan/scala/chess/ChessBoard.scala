package net.brianvaughan.scala.chess

/*
 *
 * @author Brian Vaughan
 */


/**
 * Enumerates the types of pieces in the game.
 */
object PieceType extends Enumeration {
  type PieceType = Value
  val Pawn = Value("Pa")
  val Rook = Value("Ro")
  val Knight = Value("Kn") 
  val Bishop = Value("Bi")
  val Queen = Value("Qu")
  val King = Value("Ki")
}
import PieceType._

/**
 * Enumerates the players of the game.
 */
object Color extends Enumeration {
  type Color = Value
  val White = Value("w")
  val Black = Value("b")
}
import Color._


/**
 * Enumerates cardinal directions in which pieces can move
 */
object Direction extends Enumeration {
  type Direction = Value
  //the "id" numbers here represent the number that needs to be
  //added to an existing square in our board state to go one 
  //square in the relevant direction.
  val North = Value(16)
  val South = Value(-16)
  val East = Value(1)
  val West = Value(-1)
  val NorthEast = Value(17)
  val NorthWest = Value(15)
  val SouthEast = Value(-15)
  val SouthWest = Value(-17)

  //a utility list of "straight" directions.
  val straight = List(North,South,East,West)

  //the diagonal directions
  val diagonal = List(NorthEast,SouthEast,NorthWest,SouthWest)

}

import Direction._


/**
 * A chess "piece" has properties defining its behavior and 
 * the player (color) controlling that piece.
 */
case class Piece(piece:PieceType, side:Color) {
  override def toString = side.toString + piece.toString
}


/**
 * This class is the meat of the game, it holds the representation of the
 * board state, as well as defining the legal moves of the game.
 * 
 * I have chosen to represent the board in 0x88 style, the board is 
 * represented as an array of length 16*8.  The excess is used to determine
 * if a piece has gone "off-board."  An offboard piece &amp;'d with 0x88 will
 * result in a non-zero value, whereas any legal square &amp;'d with 0x88
 * will result in zero.
 * 
 * I have used a single-dimensional array for simplicity, with helper methods
 * to find the x/y coordinates from an array index.  This simplifies movement
 * on the board: adding Direction.id to an index moves to a square in the 
 * direction indicated.
 *
 * @see http://en.wikipedia.org/wiki/Board_representation_(chess)#0x88_method
 */
class BoardState(val board:List[Option[Piece]], val turn:Color) {

  val zeroX88 = 136:Int // 0x88
  
  /**
   * Find the king of the current player, useful for searching for check.
   */
  def findKingIndex = {
    lazy val king = board.indexWhere( x => x equals Some(Piece(King,turn) ) )
    king
  }

  def findOtherKingIndex = {
    lazy val king = board.indexWhere( 
              x => x equals Some(Piece(King,otherPlayer)))
    king
  }


  /**
   * All possible destination indexes from a piece (if it exists) positioned
   * at the specified starting square.
   */
  def possibleMovesFromSquare(x:Int,y:Int):List[Int] = {
    val start = index(x,y)
    pieceAt(start) match {
      case None=> List()
      case Some(Piece(pieceType,t)) if (t==turn) => {
        var allMoves = Nil:List[Int]
        pieceType match {
          case Queen => {
            Direction.values.foreach{
              case d => allMoves :::= getStraightMoves(start,d)
            }
          }
          case Bishop => {
            Direction.diagonal.foreach{
              case d => allMoves :::= getStraightMoves(start,d)
            }
          }
          case Rook => {
            Direction.straight.foreach{
              case d => allMoves :::= getStraightMoves(start,d)
            }
          }
          case Pawn => {
            t match {
              case White => {
                allMoves :::= getStraightMoves(
                      start,North,if(y==1){2} else {1},true)
                allMoves :::= getStraightMoves(start,NorthEast,1,true)
                allMoves :::= getStraightMoves(start,NorthWest,1,true)
              }
              case Black => {
                allMoves :::= getStraightMoves(
                      start,South,if(y==6){2}else{1},true)
                allMoves :::= getStraightMoves(start,SouthEast,1,true)
                allMoves :::= getStraightMoves(start,SouthWest,1,true)
              }
            }
          }
          case Knight => {
            val relativeSquares = knightMove(North,East,start) :: 
                                  knightMove(North,West,start) :: 
                                  knightMove(South,East,start) :: 
                                  knightMove(South,West,start) :: 
                                  knightMove(East,North,start) ::
                                  knightMove(East,South,start) ::
                                  knightMove(West,North,start) ::
                                  knightMove(West,South,start) :: Nil
            
            allMoves :::= relativeSquares.filter{
              case square => {
                ( (square & zeroX88) == 0) &&
                (pieceAt(square) match {
                  case None => true
                  case Some(Piece(_,c)) => c != turn
                })
              }
            }
          }
          case King => {
            Direction.values.foreach{
              case d => allMoves :::= getStraightMoves(start,d,1)
            }
          }
          case _ => Nil
        }
       
        allMoves
      }
      case Some(Piece(_, t)) if (t != turn) => {
        List()
      }
    }
  }

  def knightMove(direction1:Direction,direction2:Direction,start:Int) = {
    getRelativeSquare(getRelativeSquare(start,1,direction1),2,direction2)
  }

  def getStraightMoves(start:Int,
                       direction:Direction,
                       max:Int=8,
                       pawn:Boolean=false) = 
  {
    var allMoves = Nil:List[Int]
    var takenPiece = false
    allMoves :::= (1 to max)
    .takeWhile(
      squares => {
        val nextSquare = getRelativeSquare(start,squares,direction)  
        ((nextSquare & zeroX88) == 0) && nextSquare > 0  &&
        (pieceAt(nextSquare) match{
          case None if (!pawn || Direction.straight.contains(direction)) => 
                  true
          case Some(Piece(_,t)) 
              if (takenPiece==false && t != turn && 
                (!pawn || Direction.diagonal.contains(direction)) ) => 
               takenPiece=true; true
          case _ => false
         })
       }
    ).toList
    .map {
      case squares => getRelativeSquare(start,squares,direction)
    }
    allMoves
  }

  /**
   * This method scans the board and tries to find all legal moves for the
   * current player, but it also includes moves that are illegal because
   * of landing in check.  This will be filtered later.
   */
  def allLegalMoves = {
    ((board zipWithIndex).map  {
      case (None,i) => Nil
      case (Some(Piece(t,c)),i) => {
        possibleMovesFromSquare(getX(i),getY(i)).map{
          //so that the result is a list of (from,to)
          case index => (i,index)
        }
      }
    }).flatten
  }

  def inCheck = {
    (new BoardState(board,otherPlayer))
    .allLegalMoves
    .contains(findKingIndex)
  }
  
  def opponentInCheck = {
    allLegalMoves.contains(findOtherKingIndex)
  }


  def inCheckMate = {
    inCheck &&
    allPossibleResultingBoardStates.filter( b => ! b.opponentInCheck ).isEmpty
  }


  def isLegalMove(x:Int,y:Int,x2:Int,y2:Int) = {
    pieceAt(x,y) match { 
      case Some(Piece(pieceType,turn)) => {
        if( possibleMovesFromSquare(x,y).contains(index(x2,y2))){
          true
        } else {
          false
        }
      }
      case _ => false
    }
  }

  def allPossibleResultingBoardStates = {
    allLegalMoves.map {
      case (from,to) => movePiece(from,to)
    }
  }


  def movePiece(from:Int,to:Int):BoardState = {
    movePiece(getX(from),getY(from),getX(to),getY(to))
  }
  def movePiece(x:Int,y:Int,x2:Int,y2:Int):BoardState = {
    if ( ! isLegalMove(x,y,x2,y2) ) {
      throw new IllegalArgumentException("Illegal move!")
    }
    val pieceToMove = pieceAt(x,y)
    
    val newBoard = board.updated(index(x,y),None)
                   .updated(index(x2,y2),pieceToMove)
    new BoardState(newBoard,otherPlayer)
  }

  def otherPlayer = turn match {
    case White => Black
    case Black => White
  }

  def getRelativeSquare(start:Int,places:Int,direction:Direction):Int={
    start + (direction.id * places )
  }

  def index(x:Int,y:Int) = x + y * 16;
  def pieceAt(x:Int,y:Int) = board( index(x,y) ):Option[Piece]
  def pieceAt(index:Int) = board(index):Option[Piece]
  def getX(index:Int) = index % 16
  def getY(index:Int) = index / 16
  def coordinates(index:Int) = (getX(index),getY(index))

  override def toString = {
    var output = ""
    for( j <- 7 to 0 by -1;
         i <- 0 to 7) {
      if( i == 0 ) {
        output += "\n"
      }
      output += (board(index(i,j)) match {
        case Some(x)=> x + "|"
        case None=> "   |"
      })
    }
    output
  }
}

object BoardState {
  val empty8 = List.fill(8) (None)
  val backRow = Rook :: Knight :: Bishop :: Queen :: King :: Bishop :: Knight :: Rook :: Nil
  val pawns = List.fill(8)(Pawn)
  val midBoard = List.fill(16 * 4)(None)

  def startingBoard = {
    val boardList = 
        (backRow map { case x:PieceType => Some(new Piece(x,White)) }) ::: 
            empty8 ::: 
        List.fill(8)(Some(new Piece(Pawn,White))) ::: empty8 :::
        midBoard :::
        List.fill(8)(Some(new Piece(Pawn,Black))) ::: empty8 :::
        (backRow map { case x:PieceType => Some(new Piece(x,Black))}) ::: 
            empty8:List[Option[Piece]]
    new BoardState(boardList, White)
  }

  def testBoard = {
    val boardList = 
        (backRow map { case x:PieceType => Some(new Piece(x,White)) }) ::: 
            empty8 ::: 
        List.fill(32)(None) :::
        midBoard :::
        (backRow map { case x:PieceType => Some(new Piece(x,Black))}) ::: 
            empty8:List[Option[Piece]]
    new BoardState(boardList, White)

  }

  def emptyBoard(turn:Color = White) = {
    val board = List.fill(16*8)(None):List[Option[Piece]]
    new BoardState( board, turn )
  }
}








