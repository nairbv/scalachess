package net.brianvaughan.scala.chess

/**
 * This trait represents the state of an arbitrary board game, and conducts
 * a breadth-first mini-max search in an attempt to find the best move.
 *
 * @author Brian Vaughan
 */ 
trait MiniMaxGame[T<:MiniMaxGame[T]] { self:T =>

  /** has the game been won by the player who's turn it is to move? */
  def isWinner:Boolean
  /** is the game currently in a "draw" state? */
  def isTie:Boolean
  /** has the game ended? */
  def gameOver:Boolean = {
    isWinner || isTie
  }
  
  /** 
   * All of the possible game-states that could result from making any 
   * available moves.
   */
  def allPossibleResultingGameStates:List[T]

  /**
   * An Double evaluating how good this MiniMaxGame looks from the perspective
   * of the current player.  Higher numeric values represent "better" boards.
   */
  def evaluate:Double

  /**
   * Evaluate the quality of the current board state given a minimax 
   * (technically a "Negamax" search of the potential outcomes, using 
   * the above evaluator.
   */
  def miniMaxEvaluate(depth:Int=1):Double = {
    if( depth == 0 ) { this.evaluate }
    else {
      ( (this allPossibleResultingGameStates) map {
        case g => -g.miniMaxEvaluate(depth-1)
      }).max
    }
  }
  
  /**
   * Find the best move (as represented by the resulting MiniMaxGame) after 
   * searching the game tree a specified depth in a breadth-first fashion.
   * Note that depth is the number of recursions, a depth of 0 will still
   * evaluate the current state.  Depth &gt; 2 can cause OutOfMemoryErrors.
   */
  def bestMove(depth:Int=1): T = {
    if( this.gameOver ) {
      println("game over!")
      this
    } else {
      this.allPossibleResultingGameStates reduceLeft {
        (best,current) => {
          if( -best.miniMaxEvaluate(depth) < -current.miniMaxEvaluate(depth ) ){
            current
          } else {
            best
          }
        }
      }
    }
  }
}




