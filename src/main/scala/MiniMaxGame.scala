package net.brianvaughan.scala.chess

import scala.actors.Futures
import scala.actors._
import scala.actors.Actor._

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
  def allPossibleResultingGameStates:List[ComputerPlayableGameState]

  /**
   * A Double evaluating how good this ComputerPlayableGame looks from the perspective
   * of the current player.  Higher numeric values represent "better" boards.
   */
  def evaluate:Double
}


object MiniMaxGamePlayer {
  val logger = Logger("MiniMaxGamePlayer")

  /** 
   * Use a miniMax (technically "Negamax") algorithm to evaluate 
   * the given board state, retreiving the determined score of the optimal 
   * move and the resulting board state for the optimal move.
   */
  def miniMaxEvaluate(actor:Actor,
                      game:ComputerPlayableGameState,
                      depth:Int=1,
                      alpha:Double= -9999999.0,
                      beta:Double=99999999.0)
    :ScoredGame =
  {
    var localalpha = alpha
    var best = -9999999.0
    if( depth == 0 ) {
      ScoredGame(game.evaluate,game)
    } else {
      val states = (game allPossibleResultingGameStates )
      if( states.isEmpty ) {
        if( game.isLoser ) {
          return ScoredGame(-99999999.0,game)
        } else if (game.isWinner ) {
          return ScoredGame(9999999.0,game)
        } else {
          return ScoredGame(0.0,game)
        }
      } else {
      //intialize lazy values in children
      //in a multi-threaded way.
      //at one point this was helping performace, but right now is probably
      //unnecessary.
      val futures = (states.map {
        case state:ComputerPlayableGameState => 
          Futures.future {
            //if we still have depth, we'll need the resulting boardstates
            //but if this is the last iteration, the count of legal moves
            //will be enough.
            if( depth > 1 ) {
              state.preFetchDeep
            } else {
              state.preFetchShallow
            }
          }
      })
      Futures.awaitAll(100,futures:_*)

      val sortedStates = states.sortWith((b1,b2)=>b1.evaluate >= b2.evaluate)
      sortedStates.zipWithIndex.foldLeft( ScoredGame(alpha, states.head ) ) {
        ( bestTuple,  gWithIndex ) => {
          val (bestScore,bestGame) = (bestTuple.score,bestTuple.game)
          
          //hm... he zip makes this a bit complicated, and is only used for
          //debugging info...
          val (g,index) = gWithIndex

          val actResult =   (actor !! 
                  EvaluationParameters(g,depth-1,-beta,-localalpha))

          while( ! actResult.isSet ) {
            if( actor.getState == Actor.State.Terminated ) {
              logger.debug("actor dead, stop waiting for evaluation of board")
              //using a recognizable number so I know it if I see it in the 
              //logs.  this represents an incomplete un-used score that was
              //bypassed for a score at a lower depth.
              return ScoredGame(-1234567, g)
            }
            Futures.awaitAll(100,actResult)
          }
          actResult() match {
            case ScoredGame(scored,gameResult) => 
                val (tmpV,returnedGame)= (scored,gameResult)

            val v = -tmpV
            val localBest = v max best
            //short-circuits the minimax to prune tree.
            //is there a more idiomatic way to do this?
            if( localBest >= beta ) {
 //           println("pruning!")
              
              logger.debug("pruning after "+index + "/"+states.size+
                           " moves analyzed at depth "+depth)

              return if ( v >= best ) { 
                ScoredGame(v,g) 
              } else { 
                ScoredGame(best,bestGame) 
              }
            }
            best = localBest
            localalpha = localBest max localalpha
            if( v >= best ){
              ScoredGame(v,g)
            } else {
              ScoredGame(localBest,bestGame)
            }

            case _=> throw new Exception("bah!")
          }
        }
      }
      }
    }
  }
  


  /**
   * Find the best move (as represented by the resulting ComputerPlayableGame) 
   * after searching the game tree a specified depth in a breadth-first fashion.
   * Note that depth is the number of recursions, a depth of 0 will still
   * evaluate the current state.
   * Since this calls minimax, depth is actually depth+1.
   * Depth &gt; 2 can cause OutOfMemoryErrors.
   */
  def bestMove(game:ComputerPlayableGameState, 
               depth:Int=1, 
               actor:EvaluationActor=new EvaluationActor)
    : ComputerPlayableGameState = 
  {

    actor.start()
    val start = System.currentTimeMillis
    val result = if( game.gameOver ) {
      logger.info("game over!")
      game
    } else {
      val actResult =   ( actor !! 
                  EvaluationParameters(game,depth, -9999999.0,9999999.0) )
      while( ! actResult.isSet ) {
        if( actor.getState == Actor.State.Terminated ) {
          logger.debug("dead actor")
          return game
        }
        Futures.awaitAll(100,actResult)
      }
      actResult() match {
        case ScoredGame(score,board)=> 
              logger.info("found best move for depth: "+
                          depth+" with score "+score)
              board
        case _=> throw new Exception("bah!")
      }
    }
    actor ! "Exit"
    result
  }

  /**
   * This method first does a minimal-depth analysis of the board to find a 
   * "best" move, then iteratively does deeper and deeper negaMax searches
   * until the specified timeout (in miliseconds).
   * 
   * It performs the searches in a separate actor that will be killed when
   * time has run out, this ensures resources are not wasted in the background 
   * continuing down an analysis tree that has already been abandoned.
   */
  def bestMoveIterativeDeepening(
          game:ComputerPlayableGameState,timeout:Long)
    :ComputerPlayableGameState = 
  {
    val start = System.currentTimeMillis
    var currentDepth = 1
    val result = if( game.gameOver ) {
      logger.info("game over!")
      game
    } else {
      var theBestMove:ComputerPlayableGameState = bestMove(game,currentDepth)
      
      Stream.from(2).foreach{
        case i => {
          currentDepth = i
          val actor = new EvaluationActor
          val f = Futures.future {
            bestMove(game,currentDepth,actor)
          }
          while( ! f.isSet ) {
            if( System.currentTimeMillis - start > timeout ) {
              actor ! "Exit"
              logDecisionStats(currentDepth,start)
              return theBestMove
            }
            //causes us to wait 100 ms for future to be set, then we can check
            //again.
            Futures.awaitAll(100,f)
          }
          theBestMove = f()
          if( System.currentTimeMillis - start > (timeout * .85 ) ) {
            //close enough to the timeout, not worth starting a whole other
            //iteration
            actor ! "Exit"
            logDecisionStats(currentDepth,start)
            return theBestMove
          }
        }
      }
      theBestMove
    }
    logDecisionStats(currentDepth,start)
    result
  }

  private def logDecisionStats(depth:Int,start:Long) {
    logger.info("depth at decision: " + depth)
    logger.info("decision after: " + (System.currentTimeMillis - start) + "s")
  }


  //a case class to store parameters to miniMaxEvaluate.
  //avoids some of the unsafe type-erasure matching.
  //might want to add another for the tuple2 if there's a way to 
  //make that work.
  case class EvaluationParameters(game:ComputerPlayableGameState,
                                  depth:Int,
                                  alpha:Double,
                                  beta:Double)

  case class ScoredGame(score:Double,game:ComputerPlayableGameState)


  /** 
   * This actor handles queuing evaluation of miniMaxEvaluate functions.
   * The function is recursive against this actor, so need to be careful
   * not to block.
   * An Actor is used so that progress of the iteratively-deepening 
   * analysis can be stopped via a !"Exit" message once the time available 
   * to make a decision has run-out.
   */
  class EvaluationActor extends Actor {
      def act():Unit = {
        loop { 
        react { //Within(0) {
          case "Exit" => logger.debug("exiting!"); exit();return
          case EvaluationParameters(g,d,a,b) => 
            //catch some weird error case I had at one point
            if( sender == this ) {
              logger.warn(" sending message to 'this' actor?!?!")
              exit()
              System.exit(1)
            }
            receiveWithin(0) {
              case TIMEOUT =>
              case "Exit" => logger.debug("exiting"); exit();return
            }
            actor {
              sender ! MiniMaxGamePlayer.miniMaxEvaluate(this,g,d,a,b)
            }
          case x => 
            logger.warn("this shouldn't happen, msg:"+x+
                                " sent by:"+sender);
            exit()
            System.exit(1)//kill quick to debug the error.
        }}
      }
      //just want to be able to "see" this actor if it gets printed.
      override def toString = 
          "An evaluation actor."
  }
}





