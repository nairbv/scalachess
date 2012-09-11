package net.brianvaughan.scala.chess


import com.weiglewilczek.slf4s._

trait GameState {

  /** has the game been won by the player who's turn it is to move? */
  def isWinner:Boolean
  def isLoser:Boolean
  /** is the game currently in a "draw" state? */
  def isTie:Boolean
  /** has the game ended? */
  def gameOver:Boolean = {
    isWinner || isTie || isLoser
  }
  def preFetchDeep:Any
  def preFetchShallow:Any

  def gameStateDescription:String

}


/**
 * This trait represents the state of an arbitrary board game, and conducts
 * a breadth-first mini-max search in an attempt to find the best move.
 *
 * @author Brian Vaughan
 */
trait ComputerPlayableGameState extends GameState { self =>

  /** 
   * All of the possible game-states that could result from making any 
   * available moves.
   */
  def allPossibleResultingGameStates:Seq[ComputerPlayableGameState]

  /**
   * A Double evaluating how good this ComputerPlayableGame looks from the 
   * perspective of the current player.  Higher numeric values represent 
   * "better" boards.
   */
  def evaluate:Double
}






