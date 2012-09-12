package net.brianvaughan.scala.chess


import com.weiglewilczek.slf4s._


/*
 * A representation of a chess game with validation of basic moves.
 *
 * @author Brian Vaughan
 */

/** thrown when an invalid move is attempted */
class InvalidMoveException(msg:String) extends IllegalArgumentException(msg)

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
sealed abstract class Piece(name:String, val side:Color, val value:Int) {
  override def toString = side.toString + name

}

//the pieces
case class Pawn(override val side:Color) extends Piece("Pa",side,1)
case class Rook(override val side:Color) extends Piece("Ro",side,5)
case class Knight(override val side:Color) extends Piece("Kn",side,3)
case class Bishop(override val side:Color) extends Piece("Bi",side,3)
case class Queen(override val side:Color) extends Piece("Qu",side,9)
case class King(override val side:Color) extends Piece("Ki",side,1000000)


private[chess] case class CastlingAbility(direction:Direction, side:Color)


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
 * on the board: adding Direction.id to an index moves it one square in the 
 * direction indicated.
 *
 * This class holds immutable board states, and returns new representations
 * with moved pieces after making a move.  This will better enable mini-max 
 * searches if developed further.
 *
 * @see http://en.wikipedia.org/wiki/Board_representation_(chess)#0x88_method
 */
final class BoardState(val board:Seq[Option[Piece]], 
                       val turn:Color,
                       castlingAbilities:Seq[CastlingAbility],
                       override val movesIntoGame:Int=0,
                       val movesSinceCapture:Int=0,
                       promotionPiece:Piece=Queen(null)
                       )
  extends ComputerPlayableGameState
{
  val logger = Logger("BoardState")

  val zeroX88 = 136:Int // 0x88
  
  /**
   * Find the king of the current player, useful for searching for check.
   * Package private to allow for testing.
   */
  private[chess] lazy val findKingIndex = {
    board.indexWhere( x => x equals Some( King(turn) ) )
  }

  private[chess] lazy val findOpponentKingIndex = {
    board.indexWhere( 
              x => x equals Some( King(opponentColor)))
  }
  
  // these are just internal for tracking what kind of moves we're looking
  // for when searching for legal moves.
  sealed class Purpose
  //if we're evaluating the quality of a position, certain moves aren't useful.
  //also, we might consider moves that attack our own pieces, since they
  //control regions of the board even if they aren't legal moves in the 
  //current turn.
  case object Evaluation extends Purpose
  //just check for legality, filter later if this move lands us in check.
  //diagonal pawn moves for example are not legal unless there's an opposing
  //piece in that square.
  case object Legality extends Purpose
  //when analyzing which destination squares would land an opponent in check,
  //also need to consider diagonal pawn moves even if no opponent piece is 
  //yet in that square.  This is called with purpose "Check" after a resulting
  //board state has been generated to strictly check the legality of a move
  //about to be made.
  case object Check extends Purpose

  /**
   * All possible destination indexes from a piece (if it exists) positioned
   * at the specified starting square.
   * This is just fetching destination squares by behavior, and checking
   * for obstacles or potential captures.  It does not perform logic
   * for filtering moves that are illegal because of check.
   */
  private def possibleMovesFromSquare(x:Int,y:Int, purpose:Purpose=Legality)
      :Seq[Int] = {

    val start = index(x,y)
    val piece = pieceAt(start)
    piece match {
      case None=> Nil
      case Some(piece) => {
        if( piece.side != turn ) {
          Nil
        } else 
        piece match {
          case Queen(t)  => {
            (Direction.values.map{
              case d => getStraightMoves(start,d,purpose)
            }).toList
            .flatten
          }
          case Bishop(t) => {
            (Direction.diagonal.map{
              case d => getStraightMoves(start,d,purpose)
            }).flatten
          }
          case Rook(t) => {
            (Direction.straight.map{
              case d => getStraightMoves(start,d,purpose)
            }).flatten
          }
          //todo: add en pasant move and promotion
          case Pawn(t) => {
            t match {
              case White => {
                //for pawn evaluation, don't count moves that can't
                //kill forward
                getStraightMoves(start,North,purpose,if(y==1){2} else {1}) :::
                //but for diagonal moves, if they can kill a friendly piece,
                //it's still one more attacking move and useful for evaluation.
                getStraightMoves(start,NorthEast,purpose,1) :::
                getStraightMoves(start,NorthWest,purpose,1)
              }
              case Black => {
                getStraightMoves(
                      start,South,purpose,if(y==6){2}else{1}) :::
                getStraightMoves(start,SouthEast,purpose,1) :::
                getStraightMoves(start,SouthWest,purpose,1)
              }
            }
          }
          case Knight(t) => {
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
                ( pieceAt(square) match {
                  case None => true
                  case Some(x) => 
                    (!purpose.equals(Legality)) || 
                    x.side != turn
                })
              }
            }
          }
          //todo: add castling move.
          case King(t)  => {
            (Direction.values.map{
              case d => getStraightMoves(start,d,purpose,1)
            }).toList.flatten :::
            (if( purpose == Legality ) {
              castlingAbilities.filter( _.side == t )
              .map {
                case x=> 
                  getStraightMoves(start,x.direction,Legality,2)
                  .filter{
                    dest => 
                      //filter out moves if it's a left castle and there's
                      //a piece at b1 (or b7) depending on player
                      if( getX(dest) == 2 && 
                          pieceAt( 1, getY(dest))!=None){
                        false
                      } else {
                        true
                      }
                  }

              }.toList.flatten
            } else Nil)
          }
//          case piece:Piece => Nil
        }
      }
    }
  }
  
  /**
   * Gets a single possible destination square for a Knight for a specified
   * pair of one and two square directions.
   */
  private def knightMove(direction1:Direction,direction2:Direction,start:Int)=
  {
    getRelativeSquare(getRelativeSquare(start,1,direction1),2,direction2)
  }

  /**
   * Gets a set of possible destination squares for a "sliding" piece,
   * in a specified direction.
   */
  private def getStraightMoves(start:Int,
                               direction:Direction,
                               purpose:Purpose=Legality,
                               max:Int=7) = 
  {
    //if we're starting from a pawn, there are special rules about
    //when the piece can capture an opponent on the destination square.
    val pawn = pieceAt(start) match {
      case Some(Pawn(x)) => true
      case _ => false
    }
    //once a piece is taken, need to stop progressing in this direction.
    var takenPiece = false
    // 1 to max, with max generally being 7, the number of moves in any 
    // one direction that could be possible.
    (1 to max)
    //set of conditions determining if moving another square in this 
    //direction is still a legal move.
    .takeWhile(
      squares => {
        val nextSquare = getRelativeSquare(start,squares,direction)
        //if we've taken a piece already, break this loop.
        takenPiece == false &&
        //check if this square is off the board
        ((nextSquare & zeroX88) == 0) && nextSquare > 0  &&
        //check for pieces blocking the way or to be captured.
        (pieceAt(nextSquare) match{
          case None if ( ! pawn ) => true
          case None if ( Direction.straight.contains(direction) )=>
            if( ! Check.equals(purpose) ) {
              true
            } else {
              false
            }
          case None if ( Direction.diagonal.contains(direction) )=>
            if( !Legality.equals(purpose) ) {
              true
            } else {
              false 
            }
          case Some(piece) if( ! pawn ) =>
            purpose match {
              case Check => takenPiece = true; true;
              case Legality | Evaluation=>
                    if( piece.side != turn ) {
                      takenPiece=true;true;
                    } else {
                      false
                    }
              case _ => false
            }
          case Some(piece) if( pawn ) =>
            if( Direction.straight.contains(direction) ) {
              false
            } else if( piece.side != turn || ! Legality.equals(purpose) ) {
              true
            } else {
              false
            }
          case _ => false
         })
       }
    ).toList
    .map {
      //convert the resulting list of "number of squares" into board indexes.
      case squares => getRelativeSquare(start,squares,direction)
    }
  }

  private[chess] lazy val allLegalMovesEvaluation:Seq[Tuple2[Int,Int]]={
    getLegalMoves(Evaluation)
  }

  /**
   * This method scans the board and tries to find all legal moves for the
   * current player, but it also includes moves that are illegal because
   * of landing in check.  This will be filtered later.
   * The moves are represented as a tuple of (from_index,to_index)
   */
  private[chess] lazy val allLegalMoves:Seq[Tuple2[Int,Int]] = {
    getLegalMoves(Legality)
  }

  private[chess] lazy val allLegalMovesCheck:Seq[Tuple2[Int,Int]]= {
    getLegalMoves(Check)
  }

  private def getLegalMoves(purpose:Purpose=Legality):Seq[Tuple2[Int,Int]]={
    val unflattened = 
        ( (board.zipWithIndex.collect{case (Some(p),i)=> (p,i)}) map  {
      case (piece,i) => {
        //this is probably the most expensive operation, so making it
        //multi-threaded.
        //strange.. in the current version, if I uncomment this futures
        //code, the compiler just hangs forever.
//        scala.actors.Futures.future {
          possibleMovesFromSquare(getX(i),getY(i),purpose).map {
            //so that the result is a list of (from,to)
            case index => (i,index)
          }
//        }
      }
    })
//    scala.actors.Futures.awaitAll(200,unflattened :_*)
//    val applied = unflattened.map { future => future.apply() }
    unflattened.flatten
  }

  lazy val numberOfPossibleMoves = allLegalMoves.size

  /** 
   * Find out if the current player is in check (the player who's turn it is 
   * now).
   *
   * // todo: need a helper method that generically checks if a square is
   * // "under attack," to help with castling rules.
   */
  lazy val inCheck = squareAttacked(findKingIndex)
  
  private def squareAttacked(index:Int) = {
    ((new BoardState(board,opponentColor,castlingAbilities))
    .allLegalMovesCheck map {
      case (from,to) => to 
    })
    .contains(index)
  }


  /** 
   * Find out if our opponent is in check.
   * Technically it usually shouldn't be possible to move into this state, 
   * since your opponent can't legally move himself into check.  
   * Once you've moved, the "opponent" refers to you.  In practice, we 
   * create an invalid board state specifically for checking the legality of 
   * a move.
   */
  private lazy val opponentInCheck = {
    (allLegalMovesCheck map {
      case (from,to) => to
    }).contains(findOpponentKingIndex)
  }

  /** 
   * To determine checkmate, see if we are in check, and if there are 
   * any possible moves that leave us not in check.
   */
  override def isWinner = {
    false//currently has to be losers turn to check if they lost
  }

  override def isLoser = {
    inCheckMate
  }

  lazy val inCheckMate = {
    inCheck &&
    lazyAllPossibleResultingGameStates.filter( b => ! b.opponentInCheck ).isEmpty
  }


  /** 
   * check if we are in stalemate (there are no legal moves we can make)
   */
  override def isTie = inStaleMate || fiftyMoveDraw
  
  lazy val inStaleMate = {
    
    (allLegalMoves.size == 0 && ! inCheck ) || 
    (
      //not worth checking unless we've got few possible moves.
      //if we've got more than 12 moves and aren't in check, at least
      //one of those 12 moves likely doesn't land us in check.
      (allLegalMoves.size < 12) &&
      ! inCheck &&
      lazyAllPossibleResultingGameStates.filter( b => ! b.opponentInCheck ).isEmpty
    )
  }

  lazy val fiftyMoveDraw = movesSinceCapture > 49


  override def gameStateDescription = {
    if( isWinner ) {
      turn + " has won!"
    } else if( isLoser ) {
      opponentColor + " has won!"
    } else if( inStaleMate ) {
      "Stale Mate!"
    } else if( fiftyMoveDraw ) {
      "No capture for 50 moves, It's a draw!"
    } else {
      "Still playing"
    }
  }

  /** 
   * How "good" is this board state for the current player, for the sake
   * of minimax searching.
   * Todo: support defining custom evaluators.
   */
  override def evaluate:Double = lazyEvaluate
  
  //todo: this code is messy, use temporary variables to clean up readability.
  private lazy val lazyEvaluate:Double = {
    //checking for a winner on each move turns out to be too expensive
    /*if( isWinner ) {
      -999999999.0
    } else if(isTie) {
      0
    } else {*/
    if( fiftyMoveDraw ) {
      0
    } else {
      
     val mobilityAndAttack = allLegalMovesEvaluation.foldLeft(0.0) {
        case (sum, (from,to))=> {
          sum + 
          //constant factor to add per legal move.
          .01 +
          // if this possible move attacks a valuable piece, it's probably
          // a more valuable move
          (pieceAt(to) match {
            case None=> .001
                               // king has infinite value, don't want
                               //to skew this evaluation just for a "check"
            case Some(x) => .001 + (x.value.min(50) / 100.0)
          }) +
          // positions that attack the center of the board are more valuable.
          // but become less valuable later in the game when there are
          // few pieces left and we need to push the opponent king into a 
          // corner to checkmate him.
          distanceFromEdge(to) * ( opponentAllPiecesExKingValue / 1000.0 )
        }
      }

      val gamePhaseRules = (if( endGame ) { 
        //if we're getting into the end game, the opponent should view
        //a good move for us as being one that allows us more space to move
        //and avoid checkmate.
        possibleMovesFromSquare(getX(findKingIndex),
                                getY(findKingIndex)).size / 20.0
      } else if(opening) {
        (castlingAbilities.filter{
          a => a match {
            case CastlingAbility(_,c) if(c==turn) => true
            case _ => false
          }
        }).size / 200.0 // gives .005 point value for each available castling 
                       // direction
      } else {
        0
      })
      
      val pieceSpecificRules = (
        //add up specific rules for specific pieces
        (board.zipWithIndex.map{
          case (Some(piece),i) if(piece.side==turn) =>
            // subtract some points for having the queen across the 
            // board in the opening.
            piece match{
              case Queen(_) => 
                if( opening ) {
                  //queens shouldn't come out into the fray too early.
                  -offSidesRank(i)/50.0
                } else 0.0
              case Pawn(_) =>
                //pawns that are further towards promotion are more valuable
                //the benefit is exponential as they get closer to the
                //other side, so squaring.
                math.pow(offSidesRank(i),2) * 0.001 
              case _ => 0.0
            }
          case _ => 0
        }).sum
      )

      allPiecesExKingValue - opponentAllPiecesExKingValue +
        0.01 * mobilityAndAttack + pieceSpecificRules + gamePhaseRules

    }
  }

  private val endGame = allPiecesExKingValue < 8
  private val opening = allPiecesExKingValue > 35
  private val midGame = ! endGame && ! opening
  
  private[chess] def distanceFromEdge(index:Int):Int = {
    val x = getX(index)
    val y = getY(index)
    (x.min(7-x).max( y.min(7-y) ))
  }
  //higher if further offsides.
  private[chess] def offSidesRank(index:Int):Int ={
    if( turn == White ) {
      getY(index)
    } else {
      7 - getY(index)
    }
  }

  def preFetchShallow = allLegalMovesEvaluation
  def preFetchDeep = allPossibleResultingGameStates


  /**
   * Tests if the specified move is a legal move.
   * This is not a complete check for move legality, it only verifies that
   * the piece at the source moves in the manner required to get from point
   * (x,y) to point (x2,y2).  It does not, for example, ensure that x2,y2
   * keeps the current player out of check.
   */
  private def isLegalMove(x:Int,y:Int,x2:Int,y2:Int, strict:Boolean=true)
    :Boolean = 
  {
    pieceAt(x,y) match { 
      case Some(King(c)) if( (x2 -x).abs > 1 ) =>
        if( possibleMovesFromSquare(x,y).contains(index(x2,y2))) {
          if( strict ) {
            //can't castle out of or through check.
            if( x2 > x ) {
              !(squareAttacked(index(4,y)) ||
              squareAttacked(index(5,y)) ||
              squareAttacked(index(6,y)) ||
              squareAttacked(index(7,y)))
            } else {
              !(squareAttacked(index(0,y)) ||
              squareAttacked(index(1,y)) ||
              squareAttacked(index(2,y)) ||
              squareAttacked(index(3,y)) ||
              squareAttacked(index(4,y)))
            }
          } else {
            true
          }
        } else false
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
   * includes board states that are illegal (put the player in check), because
   * we are using the result of this method to check for those illegal moves.
   */
  override def allPossibleResultingGameStates:Seq[ComputerPlayableGameState] = 
        lazyAllPossibleResultingGameStates

  //for some reason scala doesn't allow abstract lazy val's
  private lazy val lazyAllPossibleResultingGameStates:Seq[BoardState] = {
    (allLegalMoves.filter{
      //without this test, we end up with moves that allow the king
      //to be captured (moves into positions where the king captures a piece,
      //where the resulting position is attacked by another pawn.
      //From the perspective of the opponent, the square was not attacked when
      //it contained an on-side piece.
      //failure case: IndexOutOfBoundsException(-1) after getting kingIndex
      //and looking for moves.
      //need to find more efficient way to handle this case.
      move=> isLegalMove(getX(move._1),getY(move._1),
                          getX(move._2),getY(move._2),true)
    }) 
    .map {
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
      //todo: include reason?  should start/end be attributes of exception?
      throw new InvalidMoveException("Cannot move from "+
                      x + "," + y + " to " + x2 + "," + y2)
    }
    movePieceWithoutValidation(x,y,x2,y2)
  }

  /** 
   * Change what piece will be used for promoting pawns in the next move.
   * The color of the passed in piece will be ignored.
   */
  def changePromotionPiece(piece:Piece):BoardState = {
    //just cloning the board with new piece
    new BoardState(board,
                   turn,
                   castlingAbilities,
                   movesIntoGame,
                   movesSinceCapture,
                   piece)
  }

  private def movePieceWithoutValidation(x:Int,y:Int,x2:Int,y2:Int) ={
    var pieceToMove = pieceAt(x,y)
    val pieceAtDestination = pieceAt(x2,y2)
    //handle with pawn promotion.
    pieceToMove match {
      case Some(Pawn(x)) => 
        if( (x == White && y2 == 7) || (x==Black && y2==0 ) ) {
          //this is ugly, but don't feel like adding method to every subclass
          //right now.
          pieceToMove = Some((promotionPiece.getClass.getConstructors.head)
                        .newInstance(x).asInstanceOf[Piece])
          //i'd like to use this, but apparently it's a 'language feature'
          //and there's no way to tell the parent class that all of the
          //subclasses that will be case classes (which all have 'copy')
          //pieceToMove = promotionPiece.copy(side=x)
        }
      case x =>
    }

    var newBoard = board.updated(index(x,y),None)
                   .updated(index(x2,y2),pieceToMove)

    val newCastlingAbilities = {
      pieceToMove match {
        case Some(King(c)) => {
          
          if(  (x2 - x).abs == 2 ) {
            //this is a castling move, adjust the rook
            if( x2 > x ) {
              val rook = pieceAt(7,y)
              newBoard = newBoard.updated(index(7,y),None)
                            .updated(index(5,y),rook)
            } else {
              val rook = pieceAt(0,y)
              newBoard = newBoard.updated(index(0,y),None)
                            .updated(index(3,y),rook)
            }
          }
          castlingAbilities.filterNot(_==CastlingAbility(East,c))
          .filterNot(_==CastlingAbility(West,c))
        }
        case Some(Rook(c)) =>
          castlingAbilities.filterNot(x==0 && _==CastlingAbility(West,c))
          .filterNot( x==7 && _ == CastlingAbility(East,c))
        case x => castlingAbilities
      }
    }
    new BoardState(newBoard,
                   opponentColor,
                   newCastlingAbilities,
                   movesIntoGame+1,
                   pieceAtDestination match {
                     case Some(p) => 0
                     case None => movesSinceCapture+1
                   })
  }

  /**
   * Helper method to fetch the opponent color.  Used for swapping turns
   * after a move.
   */
  private lazy val opponentColor = turn match {
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
  //helpers to convert an index to a cartesian coordinate
  def getX(index:Int) = index % 16
  def getY(index:Int) = index / 16
  def coordinates(index:Int) = (getX(index),getY(index))

  def allPieces = {
    board.collect{ case Some(square) => square }.filter{
      case piece => piece.side == turn
    }
  }
  def allPiecesExKingValue = {
    allPieces.foldLeft(0){_ + _.value } - 1000000
  }

  def opponentAllPiecesExKingValue = {
    board.collect { case Some(square) => square }.filter{
      case piece=>piece.side != turn 
    }.foldLeft(0) { _ + _.value } - 1000000
  }
  

  /**
   * Print out an ascii representation of the current chess board.
   *
   */
  override def toString = {
    
    var output = "move: " + movesIntoGame
    output += "\nPlayer: " + turn + 
    "\nScore: " + evaluate +
    //don't include king
    "\nPiece Value: " + allPiecesExKingValue
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

  
    //for testing checkmate:    
//    if( allPieces.collect{ case King(x) if( x == turn)=> true }.isEmpty ) {
//      print(output) 
//      System.exit(1)
//    }
    
    output
  }

  /** 
   * Will need to override equals to use transposition tables or to check
   * for any kind of previously evaluated state.
   */
  override def equals(other:Any):Boolean = {
    other match {
      case that: BoardState => 
              this.board == that.board && 
              this.turn == that.turn && 
              this.movesIntoGame == that.movesIntoGame &&
              this.movesSinceCapture == that.movesSinceCapture
      case _ => false
    }
  }

  override def hashCode:Int = {
    41 * (
      41 * (
        41 * (
          41 + board.hashCode
        ) + turn.hashCode
      ) + movesIntoGame.hashCode
    ) + movesSinceCapture.hashCode
  }

}

/**
 * Companion object for fetching initial boardstate.
 *
 */
object BoardState {
  private val empty8 = List.fill(8) (None)
  private lazy val backRowWhite = 
                (Rook(White) :: 
                Knight(White) :: 
                Bishop(White) :: 
                Queen(White) :: 
                King(White) :: 
                Bishop(White) :: 
                Knight(White) :: 
                Rook(White) :: Nil)
                .map{ case x:Piece => Some(x) }:List[Option[Piece]]
  private lazy val backRowBlack =  
                (Rook(Black) :: 
                Knight(Black) :: 
                Bishop(Black) :: 
                Queen(Black) :: 
                King(Black) :: 
                Bishop(Black) :: 
                Knight(Black) :: 
                Rook(Black) :: Nil)
                .map{ case x:Piece => Some(x) }:List[Option[Piece]]

  private lazy val midBoard = List.fill(16 * 4)(None)

  private lazy val boardList:Seq[Option[Piece]] = 
        ((backRowWhite) ::: 
            empty8 ::: 
        List.fill(8)(Some(Pawn(White))) ::: empty8 :::
        midBoard :::
        List.fill(8)(Some(Pawn(Black))) ::: empty8 :::
        (backRowBlack) ::: 
            empty8)

  private val defaultCastlingAbilities = List(
    CastlingAbility(East,White),
    CastlingAbility(West,White),
    CastlingAbility(East,Black),
    CastlingAbility(West,Black))

  /** 
   * This is the default starting board for a normal chess game. 
   * This is a def not a val because otherwise eventually the 
   * cache'd allPossibleGameStates will be populated in a tree-like
   * fashion until we run out of memory.  As a def, we're better 
   * able to drop reference to the head of the game tree and allow
   * garbage collection.
   */
  def startingBoard:BoardState = {
    new BoardState(boardList, White,defaultCastlingAbilities)
  }

  /** for testing */
  lazy val emptyBoard = {
    new BoardState(List.fill(16*8)(None), White, defaultCastlingAbilities)
  }
  

  def watchAGame(depth:Int=2) = {
    var board:ComputerPlayableGameState = startingBoard
    //using print instead of logger for normal game output
    val player = new MiniMaxGamePlayer
    println(board)
    while( ! board.gameOver ) {
      board= player.bestMove(board,depth)
          //board.bestMove(depth)
      println(board)
    }
    println("Game Over")
    println(board.gameStateDescription)
    board
  }
  
  def watchAnIterativeDeepeningGame(millis:Long) = {
    var board:ComputerPlayableGameState = startingBoard
    println(board)
    val player = new MiniMaxGamePlayer
    while( ! board.gameOver ) {
      board = player.bestMoveIterativeDeepening(board,millis)
      println(board)
    }
    println("Game Over")
    println(board.gameStateDescription)
    board
  }
}



