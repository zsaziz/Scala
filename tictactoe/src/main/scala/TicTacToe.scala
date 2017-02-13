import hw.tictactoe._

class Game(turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike[Game] {
	def isFinished(): Boolean = {
		isFinHelper(0, 0, dim, turn)
	}

	def isFinHelper(i: Int, j: Int, dim: Int, turn: Player): Boolean = {
		if (i < dim){
			if (j < dim){
				if ((board((i, j)) == turn))
					isFinHelper(i, j + 1, dim, turn)
				else
					false
			}
			else
				isFinHelper(i + 1, 0, dim, turn)
		}
		true
	}
	def getWinner(): Option[Player] = {
		if (isFinished() == true){
			if (getHorizontal(board, 0, 0, dim).forall(x => x == O)) Some(O)
			else if (getHorizontal(board, 0, 0, dim).forall(x => x == X)) Some(X)
			else if (getVertical(board, 0, 0, dim).forall(x => x == O)) Some(O)
			else if (getVertical(board, 0, 0, dim).forall(x => x == X)) Some(X)
			else if (getDiagonal(board, 0, 0, dim).forall(x => x == O)) Some(O)
			else if (getDiagonal(board, 0, 0, dim).forall(x => x == X)) Some(X)
			else None
		}
		else None
	}

	def getHorizontal(board: Map[(Int, Int), Player], rows: Int, cols: Int, N: Int): List[Player] = (rows, cols) match{
		case (_, N) => List(board(rows, cols))
		case (x, y) => List(board(x, y)) ++ getHorizontal(board, x, y + 1, N)
	}

	def getVertical(board: Map[(Int, Int), Player], rows: Int, cols: Int, N: Int): List[Player] = (rows, cols) match{
		case(N, _) => List(board(rows, cols))
		case(x, y) => List(board(x, y)) ++ getVertical(board, x + 1, y, N)
	}

	def getDiagonal(board: Map[(Int, Int), Player], rows: Int, cols: Int, N: Int): List[Player] = (rows, cols) match{
		case(N, N) => List(board(rows, cols))
		case(x, y) => List(board(x, y)) ++ getVertical(board, x + 1, y + 1, N)
	}

	def nextBoards(): List[Game] = {
		nextBoardsHelp(0,0,dim)
	}

	def nextBoardsHelp(rows: Int, cols: Int, N: Int): List[Game] = {
		if (rows < N && cols < N){
			if (cols < N){
				if (board((rows, cols)) == turn)
					List(new Game(turn, dim, board)) ++ nextBoardsHelp(rows, cols + 1, N)
				else
					nextBoardsHelp(rows, cols + 1, N)
				}
			else
				nextBoardsHelp(rows + 1, 0, N)
		}
		else
			List(new Game(turn, dim, board))
	}

	def getTurn(): Player = turn
}

object Solution extends MinimaxLike {
	type T = Game

	def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
		new Game(turn, dim, boardHelp(0,0,dim,turn))
	}

	def boardHelp(rows: Int, cols: Int, N: Int, turn: Player): Map[(Int, Int), Player] = (rows, cols) match{
		case (N, N) => Map((rows, cols) -> turn)
		case (_, N) => boardHelp(rows + 1, 0, N, turn)
		case (_, _) => Map((rows, cols) -> turn) ++ boardHelp(rows, cols + 1, N, turn)
	}

	def minimax(board: Game): Option[Player] = {
		if (board.getTurn() == X){
			if (board.getWinner() == Some(X)) Some(X)
			else if (board.getWinner == None) None
			else if (board.nextBoards().head.getWinner() == X) Some(X)
			else if (board.nextBoards().head.getWinner() == None) None
			else Some(O)
		}
		else{
			if (board.getWinner() == Some(O)) Some(O)
			else if (board.getWinner == None) None
			else if (board.nextBoards().head.getWinner() == O) Some(O)
			else if (board.nextBoards().head.getWinner() == None) None
			else Some(X)
		}
	}
}