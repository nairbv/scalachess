

I wrote this chess program as part of an interview for a scala programming 
position at Novus (www.novus.com).

I only had about a day to do it, so it is far from perfect.  It doesn't 
implement castling, en-passant, pawn promotion, there is no real UI, and there
is no AI.

My Requirements:
====================================================================
1) You choose the programming language (Scala is a huge plus thou)
2) We care about:
    -class design (under Scala, it also means correct use of immutable
vs mutable class implementations)
    -idiomatic language use
     -code clarity
3) We don't care if the UI is "beautiful" or not.  But we do care if the
code is "beautiful"
4) It should run (as a text-based command line application, as a web
app, or use a GUI)
5) There is no need to implement advanced moves such as castling, en
passant, or promotion, but it should be easy to add these
6) I recommend implementing "check" and "check mate"
====================================================================

To play, just open an `sbt console` and make API calls directly.  The 
coordinates are indexed such that "a1" is (0,0), and "h8" is (7,7).

Example output from sbt is below.  The piece names begin with "w" or "b" to 
signify white or black pieces.  Sample usage can also be found in the unit 
tests.  Exceptions are thrown on invalid moves.


brian@brian-laptop:~/unison/Projects/chess$ sbt test
[info] Set current project to default-64fda4 (in build file:/home/brian/unison/Projects/chess/)
[info] ChessSpec:
[info] A newly created default chess board 
[info] - should have appropriate pieces
[info] - should throw IllegalArgumentException if an illegal move is attempted
[info] - should throw Illegal Argument exception if an opponents piece is moved
[info] - should have 20 possible moves
[info] - should find a king in the right place
[info] a particular game of chess 
[info] - should detect check, but not check mate
[info] a foolsmate game 
[info] - should be detected as checkmate
[info] Passed: : Total 7, Failed 0, Errors 0, Passed 7, Skipped 0
[success] Total time: 1 s, completed Sep 3, 2012 11:09:56 PM
brian@brian-laptop:~/unison/Projects/chess$ 


brian@brian-laptop:~/unison/Projects/chess$ sbt console
[info] Set current project to default-64fda4 (in build file:/home/brian/unison/Projects/chess/)
[info] Starting scala interpreter...
[info] 
Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
Type in expressions to have them evaluated.
Type :help for more information.
scala> import net.brianvaughan.scala.chess._
import net.brianvaughan.scala.chess._

scala> var board = BoardState.startingBoard
board: net.brianvaughan.scala.chess.BoardState = 

bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|bPa|bPa|bPa|bPa|bPa|bPa|bPa|
   |   |   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|wPa|wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|wBi|wKn|wRo|

scala> board = board.movePiece(4,1,4,3)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|bPa|bPa|bPa|bPa|bPa|bPa|bPa|
   |   |   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
   |   |   |   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|wBi|wKn|wRo|

scala> board = board.movePiece(1,6,1,5)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|bPa|bPa|bPa|bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
   |   |   |   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|wBi|wKn|wRo|

scala> board = board.movePiece(5,0,2,3)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|bPa|bPa|bPa|bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
   |   |wBi|   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|   |wKn|wRo|

scala> board = board.movePiece(3,6,3,4)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|   |bPa|bPa|bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |bPa|   |   |   |   |
   |   |wBi|   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|   |wKn|wRo|

scala> board = board.movePiece(2,3,3,4)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|   |bPa|bPa|bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |wBi|   |   |   |   |
   |   |   |   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|   |wKn|wRo|

scala> board = board.movePiece(5,6,5,4)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|   |bPa|   |bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |wBi|   |bPa|   |   |
   |   |   |   |wPa|   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|   |wKn|wRo|

scala> board = board.movePiece(4,3,5,4)
board: net.brianvaughan.scala.chess.BoardState = 
bRo|bKn|bBi|bQu|bKi|bBi|bKn|bRo|
bPa|   |bPa|   |bPa|   |bPa|bPa|
   |bPa|   |   |   |   |   |   |
   |   |   |wBi|   |wPa|   |   |
   |   |   |   |   |   |   |   |
   |   |   |   |   |   |   |   |
wPa|wPa|wPa|wPa|   |wPa|wPa|wPa|
wRo|wKn|wBi|wQu|wKi|   |wKn|wRo|








