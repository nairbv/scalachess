package net.brianvaughan.scala.chess

/*
 *
 * @author Brian Vaughan
 */


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
  //square in the relevant direction. (this is a 16x8 representation)
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
class Piece(name:String, val side:Color) {
  override def toString = side.toString + name
}

case class Pawn(override val side:Color) extends Piece("Pa",side)
case class Rook(override val side:Color) extends Piece("Ro",side)
case class Knight(override val side:Color) extends Piece("Kn",side)
case class Bishop(override val side:Color) extends Piece("Bi",side)
case class Queen(override val side:Color) extends Piece("Qu",side)
case class King(override val side:Color) extends Piece("Ki",side)



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
 * This class holds immutable board states, and returns new representations
 * with moved pieces after making a move.  This will better enable mini-max 
 * searches if developed further.
 *
 * @see http://en.wikipedia.org/wiki/Board_representation_(chess)#0x88_method
 */
class BoardState(val board:List[Option[Piece]], val turn:Color) {

  val zeroX88 = 136:Int // 0x88
  
  /**
   * Find the king of the current player, useful for searching for check.
   */
  private[chess] def findKingIndex = {
    lazy val king = board.indexWhere( x => x equals Some( King(turn) ) )
    king
  }

  private[chess] def findOpponentKingIndex = {
    lazy val king = board.indexWhere( 
              x => x equals Some( King(opponentColor)))
    king
  }


  /**
   * All possible destination indexes from a piece (if it exists) positioned
   * at the specified starting square.
   * This is just fetching destination squares by behavior, and checking
   * for obstacles or potential captures.  It does not perform logic
   * for filtering moves that are illegal because of check.
   */
  private def possibleMovesFromSquare(x:Int,y:Int):List[Int] = {
    val start = index(x,y)
    pieceAt(start) match {
      case None=> Nil
      case Some(piece) => {
        piece match {
          case Queen(t) if( t == turn)  => {
            (Direction.values.map{
              case d => getStraightMoves(start,d)
            }).toList
            .flatten
          }
          case Bishop(t) if( t == turn) => {
            (Direction.diagonal.map{
              case d => getStraightMoves(start,d)
            }).flatten
          }
          case Rook(t) if( t == turn) => {
            (Direction.straight.map{
              case d => getStraightMoves(start,d)
            }).flatten
          }
          case Pawn(t) if( t == turn) => {
            var allMoves = List():List[Int]
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
            allMoves
          }
          case Knight(t) if( t == turn) => {
            val relativeSquares = knightMove(North,East,start) :: 
                                  knightMove(North,West,start) :: 
                                  knightMove(South,East,start) :: 
                                  knightMove(South,West,start) :: 
                                  knightMove(East,North,start) ::
                                  knightMove(East,South,start) ::
                                  knightMove(West,North,start) ::
                                  knightMove(West,South,start) :: Nil
            
            relativeSquares.filter{
              case square => {
                ( (square & zeroX88) == 0) &&
                (pieceAt(square) match {
                  case None => true
                  case Some(x) => x.side != turn
                })
              }
            }
          }
          case King(t) if( t == turn) => {
            (Direction.values.map{
              case d => getStraightMoves(start,d,1)
            }).toList.flatten
          }
          case piece:Piece if (piece.side != turn)=> Nil
        }
      }
    }
  }
  
  /**
   * Gets a single possible destination squares for a Knight for a specified
   * pair of one and two square directions.
   */
  def knightMove(direction1:Direction,direction2:Direction,start:Int) = {
    getRelativeSquare(getRelativeSquare(start,1,direction1),2,direction2)
  }

  /**
   * Gets a set of possible destination squares for a "sliding" piece,
   * in a specified direction.
   */
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
          case Some(piece)
              if (takenPiece==false && piece.side != turn && 
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
      case (Some(piece),i) => {
        possibleMovesFromSquare(getX(i),getY(i)).map{
          //so that the result is a list of (from,to)
          case index => (i,index)
        }
      }
    }).flatten
  }

  /** 
   *find out if the current player is in check (the player who's turn it is 
   * now)
   */
  def inCheck = {
    ((new BoardState(board,opponentColor))
    .allLegalMoves map {
      case (from,to) => to 
    })
    .contains(findKingIndex)
  }


  /** 
   * Find out if our opponent is in check.
   * Technically it shouldn't be possible to move into this state, since 
   * your opponent can't legally move himself into check and once you've moved
   * the "opponent" refers to you.  In practice, we create an invalid board
   * state specifically for checking the legality of a move.
   */
  def opponentInCheck = {
    (allLegalMoves map {
      case (from,to) => to
    }).contains(findOpponentKingIndex)
  }

  /** 
   * To determine checkmate, see if we are in check, and if there are 
   * any possible moves that leave us not in check.
   */
  def inCheckMate = {
    inCheck &&
    allPossibleResultingBoardStates.filter( b => ! b.opponentInCheck ).isEmpty
  }


  /**
   * Tests if the specified move is a legal move.
   * This is not a complete check for move legality, it only verifies that
   * the piece at the source moves in the manner required to get from point
   * (x,y) to point (x2,y2).  It does not, for example, ensure that x2,y2
   * keeps the current player out of check.
   */
  def isLegalMove(x:Int,y:Int,x2:Int,y2:Int, strict:Boolean=true) = {
    pieceAt(x,y) match { 
      case Some(piece) => {
        if( possibleMovesFromSquare(x,y).contains(index(x2,y2))){
          //not allowed to move yourself into check
          if( strict && 
              movePieceWithoutValidation(x,y,x2,y2).opponentInCheck ) 
          {
            false
          } else {
            true
          }
        } else {
          false
        }
      }
      case _ => false
    }
  }
  

  /**
   * All of the possible board states that could result from any of the 
   * possible moves eligible to be made right now.
   *
   * includes board states tha are illegal (put the player in check), because
   * we are using the result of this method to check for those illegal moves.
   */
  def allPossibleResultingBoardStates = {
    allLegalMoves.map {
      case (from,to) => movePiece(getX(from),getY(from),getX(to),getY(to),false)
    }
  }


  /** 
   * Get the board resulting from moving a piece from the specified
   * starting point to the specified destination.
   * 
   * @param strict verifies that the move doesn't put the player in check.
   */
  def movePiece(x:Int,y:Int,x2:Int,y2:Int,strict:Boolean=true):BoardState = {
    if ( ! isLegalMove(x,y,x2,y2,strict) ) {
      throw new IllegalArgumentException("Illegal move!")
    }
    movePieceWithoutValidation(x,y,x2,y2)
  }

  private def movePieceWithoutValidation(x:Int,y:Int,x2:Int,y2:Int) ={
    val pieceToMove = pieceAt(x,y)
    val newBoard = board.updated(index(x,y),None)
                   .updated(index(x2,y2),pieceToMove)
    new BoardState(newBoard,opponentColor)
  }

  /**
   * Helper method to fetch the opponent color.  Used for swapping turns
   * after a move.
   */
  private def opponentColor = turn match {
    case White => Black
    case Black => White
  }

  /**
   * Gets the index of a square on the board that is a specified number of 
   * squares from the start position, in a specified direction.
   */
  private def getRelativeSquare(start:Int,places:Int,direction:Direction):Int={
    start + (direction.id * places )
  }

  //helper to get index from cartesian coordinates
  def index(x:Int,y:Int) = x + y * 16;
  //helper to fetch piece at a specified cartesian coordinate
  def pieceAt(x:Int,y:Int) = board( index(x,y) ):Option[Piece]
  //hepler to fetch a piece at a specified index
  def pieceAt(index:Int) = board(index):Option[Piece]
  //helper to convert an index to a cartesian coordinate
  def getX(index:Int) = index % 16
  def getY(index:Int) = index / 16
  def coordinates(index:Int) = (getX(index),getY(index))

  /**
   * Print out an ascii representation of the current chess board.
   *
   */
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

/**
 * Companion object for fetching initial boardstate.
 *
 */
object BoardState {
  val empty8 = List.fill(8) (None)
  lazy val backRowWhite = 
                (Rook(White) :: 
                Knight(White) :: 
                Bishop(White) :: 
                Queen(White) :: 
                King(White) :: 
                Bishop(White) :: 
                Knight(White) :: 
                Rook(White) :: Nil)
                .map{ case x:Piece => Some(x) }:List[Option[Piece]]
  lazy val backRowBlack =  
                (Rook(Black) :: 
                Knight(Black) :: 
                Bishop(Black) :: 
                Queen(Black) :: 
                King(Black) :: 
                Bishop(Black) :: 
                Knight(Black) :: 
                Rook(Black) :: Nil)
                .map{ case x:Piece => Some(x) }:List[Option[Piece]]



  val pawns = List.fill(8)(Pawn)
  val midBoard = List.fill(16 * 4)(None)

  def startingBoard = {
    lazy val boardList = 
        (backRowWhite) ::: 
            empty8 ::: 
        List.fill(8)(Some(Pawn(White))) ::: empty8 :::
        midBoard :::
        List.fill(8)(Some(Pawn(Black))) ::: empty8 :::
        (backRowBlack) ::: 
            empty8:List[Option[Piece]]
    new BoardState(boardList, White)
  }

}



