package coinGrabber

import util.control.Breaks._

case class State(val Board: Array[Array[Int]], val humanLoc : (Int,Int),
		val computerLoc : (Int,Int) , val HumanScore: Int ,  val computerScore: Int) {
}
//    /** Constructor for virgin items. */
//  def this( Board1: List[List[Int]],  humanLoc1 : (Int,Int),
//     computerLoc1 : (Int,Int) ,   computerScore1: Int,  HumanScore1: Int)  {
//    this(Board1, humanLoc1, computerLoc1, computerScore1, HumanScore1)


object main{
	def main(args: Array[String]): Unit = {

			val Board = Array(
					Array(0,5,10,5 ,25),
					Array(5,10,25,10,5),
					Array(10,25,1, 25,10),
					Array(5,10,25,10,5),
					Array(25,5,10,5,0))

					for (i<- 0 to 4){
						println(Board(i).mkString(" "));
					}




			var state  =  new State(Board, (0,0), (4,4), 0,0)
			var bl =true;
			var line : String =""

					while(bl){
						println("Which player should go first? (h/c) " )
						line = Console.readLine
						if (line.toLowerCase().charAt(0) == 'h'){
							bl = false;
						}	
						else if (line.toLowerCase().charAt(0) == 'c'){
							bl = false;
						}
						else println("please enter a correct player")
					}
			var bol =true;

			while(bol){
				breakable{
					if (line.toLowerCase().charAt(0) == 'h'){
						var list  = possibleMoves('h', state)
								print("Which move do you wanna choose ? (U/D/L/R)")
								var move = Console.readLine

								if (!((move.toUpperCase().charAt(0)== 'U')||(move.toUpperCase().charAt(0)== 'D')||
										(move.toUpperCase().charAt(0)== 'L')||(move.toUpperCase().charAt(0)== 'R'))){
									println("Invalid move")
									break
								}
						if(!(list.contains(move.toUpperCase().charAt(0))))
						{
							println("Invalid move")
							break
						}

						state  =  nextState('h', move.toUpperCase().charAt(0), state)
								if (state.humanLoc == (2,2) && state.HumanScore > 100 &&
								(state.HumanScore > state.computerScore)){
									println("human win!")
									System.exit(0)
								}
							    if (state.humanLoc == (2,2) && state.HumanScore > 100 &&
								(state.HumanScore < state.computerScore)){
									println("human lose!")
									System.exit(0)
								}
								else if (state.humanLoc == (2,2) && state.HumanScore < 100) {
									println("human lose!")
									System.exit(0)
								}

						// computers move
						println("Its the computers move now")
						val CompMove  = chooseMove(8, state)

						state  =  nextState('c', CompMove, state)

						if (state.computerLoc == (2,2) && state.computerScore > 100 &&
								(state.HumanScore < state.computerScore)){
							println("human lose!")
							System.exit(0)
						}
						if (state.computerLoc == (2,2) && state.computerScore > 100 &&
								(state.HumanScore > state.computerScore)){
							println("human win!")
							System.exit(0)
						}
						else if (state.computerLoc == (2,2) && state.computerScore < 100) {
							println("human win!")
							System.exit(0)
						}
						// print the board after computers move
						for (i<- 0 to 4){
							println(state.Board(i).mkString(" "));
						}
					}
					if (line.toLowerCase().charAt(0) == 'c'){
						// computers move
						println("Its the computers move now")
						val CompMove  = chooseMove(8, state)
						println("computers move " + CompMove)

						state  =  nextState('c', CompMove, state)

					if (state.computerLoc == (2,2) && state.computerScore > 100 &&
								(state.HumanScore < state.computerScore)){
							println("human lose!")
							System.exit(0)
						}
						if (state.computerLoc == (2,2) && state.computerScore > 100 &&
								(state.HumanScore > state.computerScore)){
							println("human win!")
							System.exit(0)
						}
						else if (state.computerLoc == (2,2) && state.computerScore < 100) {
							println("human win!")
							System.exit(0)
						}
						// print the board after computers move
						for (i<- 0 to 4){
							println(state.Board(i).mkString(" "));
						}
						line = "h"
					}
				}
			}
	}

	def possibleMoves(player: Char, state: State): List[Char]= {
			//Given a player, 'h' or 'c', and a State, this method returns a list of operators 
			//('U', 'D', 'L', or 'R') that can be applied.
			var hx = state.humanLoc._1 ;
			var hy  = state.humanLoc._2 ;
			var cx = state.computerLoc._1 ;
			var cy  = state.computerLoc._2 ;
			var possMoves:List[Char] = Nil
					player match {
					case 'h' => 

					if (!(hy == 0 || (cy == (hy -1) && ( cx == hx)))){
						possMoves ++= List('L') 
					}
					if (!(hy == 4 || (cy == (hy +1) && ( cx == hx)))){
						possMoves ++= List('R') 
					}
					if (!(hx == 0 || (cx == (hx -1) && ( cy == hy)))){
						possMoves ++= List('U') 
					}
					if (!(hx == 4 || (cx == (hx +1) && ( cy == hy)))){
						possMoves ++= List('D') 
					}

					case 'c' =>
					if (!(cy == 0 || (hy == (cy -1) && ( hx == cx)))){
						possMoves ++= List('L') 
					}
					if (!(cy == 4 || (hy == (cy +1) && ( hx == cx)))){
						possMoves ++= List('R') 
					}
					if (!(cx == 0 || (hx == (cx -1) && ( hy == cy)))){
						possMoves ++= List('U') 
					}
					if (!(cx == 4 || (hx == (cx +1) && ( hy == cy)))){
						possMoves ++= List('D') 
					}
			}

			possMoves 
	}
	def nextState(player: Char, direction: Char, state: State): State = { 
			//This method returns the State that results from moving the 
			//player in the given direction. 

			/** The original state is not changed.*/

			player match {
			case 'h' =>
			direction match { 
			case 'U' => var stateNew = copy(state,(state.humanLoc._1 -1,state.humanLoc._2 ),state.computerLoc); stateNew
			case 'L' => var stateNew = copy(state,(state.humanLoc._1 ,state.humanLoc._2 -1 ),state.computerLoc); stateNew
			case 'R' => var stateNew = copy(state,(state.humanLoc._1 ,state.humanLoc._2 + 1),state.computerLoc); stateNew
			case 'D' => var stateNew = copy(state,(state.humanLoc._1 +1,state.humanLoc._2 ),state.computerLoc);  stateNew
			}
			case 'c' =>
			direction match { 
			case 'U' => var stateNew = copy(state,state.humanLoc,(state.computerLoc._1 -1,state.computerLoc._2)); stateNew
			case 'L' => var stateNew = copy(state,state.humanLoc,(state.computerLoc._1 ,state.computerLoc._2 -1 )); stateNew
			case 'R' => var stateNew = copy(state,state.humanLoc,(state.computerLoc._1 ,state.computerLoc._2 +1 )); stateNew
			case 'D' => var stateNew = copy(state,state.humanLoc,(state.computerLoc._1 + 1,state.computerLoc._2 )); stateNew
			}
			}

	}
	def copy(state:State,hloc:(Int,Int),cloc:(Int,Int)): State  = {
		val tempBoard  = state.Board.map(_.clone)
				val tempHumanScore = state.HumanScore  + state.Board(hloc._1)(hloc._2)
				val tempComputerScore = state.computerScore + state.Board(cloc._1)(cloc._2) 
				tempBoard(hloc._1)(hloc._2) = 0;
		tempBoard(cloc._1)(cloc._2) = 0;

		val CopiedState  = new State( tempBoard , hloc, cloc, tempHumanScore , tempComputerScore)
		CopiedState
	}
	def chooseMove(depth: Int, state: State): Char ={

		//Finds all possible moves (up to 4 of them) for the computer.
		//Evaluates each possible move.
		//Returns (as a Char) the best move.

		val possMoves = possibleMoves('c', state) 
		var maxScore = Int.MinValue;
		var bestMove: Char = 'L';
		for (move <- possMoves){
			val score = evaluateMove('c', move, depth, state)
			println(move +  "" + score)
					if (score >= maxScore) {
						maxScore  = score
						bestMove = move
					}
		}
		bestMove
		
	}

	def evaluateMove(player: Char, direction: Char, depth: Int, state: State): Int ={
		var score: Int = heuristic(player, state,depth)
				if(depth == 0){
					return score
				}
	val changedState = nextState(player, direction, state)
	 score = heuristic(player, changedState,depth)
			player match{
			case 'h' => 
			if(changedState.humanLoc._1 == 2 && changedState.humanLoc._2 == 2)
				return score
			case 'c' =>
			if(changedState.computerLoc._1 == 2 && changedState.computerLoc._2 == 2)
				return score
	}



	var list:List[Int] = Nil
			val otherPlayerMoves = possibleMoves(otherPlayer(player), changedState)
			for ( moves <- otherPlayerMoves ){
				val best = evaluateMove(otherPlayer(player), moves, depth-1, changedState)
						list ++= List(best)
			}
			val bestest = list.max
					return	-1*bestest
	}

	def otherPlayer( player: Char): Char = {
		player match {
		case 'h'=> 'c'
		case 'c'=> 'h'  
		}
	}

	def heuristic(player: Char, state: State, depth: Int): Int ={


		player match{
		case 'h' => 
		if((state.humanLoc == (2,2)) &&
				(state.HumanScore >= state.computerScore) && state.HumanScore >= 100)
			return (8 - depth + 1)*300
			else if  ((state.humanLoc == (2,2))&&
					!((state.HumanScore >= state.computerScore) && state.HumanScore >= 100))
				Int.MinValue
			else 
				(state.computerScore - state.HumanScore ) 

		case 'c' =>
		if((state.computerLoc == (2,2))&&
				(state.HumanScore <= state.computerScore) && state.computerScore >= 100)
			return (8 - depth + 1)*300
		else if((state.computerLoc == (2,2)) &&
					!((state.HumanScore <= state.computerScore) && state.computerScore >= 100))
			 Int.MinValue
		else 
			 (state.computerScore - state.HumanScore ) 
		}
	}

}





