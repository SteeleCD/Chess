#===================== Play game ================================

playChess <- function()

	{

	info <- createBoard()

	nturn = 1

	# If no check mate

	#while(end!=TRUE)

	for(i in 1:20)

		{

		# Ask for a move from the player

		move <- getMove()

		# If player has clicked on an empty square ask for another move

		if(info$board[move$currY,move$currX]=="") 

			{

			print("Not a piece")

			flush.console()

			next

			}

		# Move the desired piece

		info <- movePiece(info$board,info$numMoves,move$currY,move$currX,move$futureY,move$futureX)

		displayBoard(info$board)

		# Move turn counter one along

		nturn = nturn + 1

		}

	}





#===================== Create the board ============================

createBoard <- function() 

	{

	# Create 8 * 8 board

	board <- matrix(data=rep("",times=8*8),ncol=8,nrow=8)

	# Add the pieces to the board

	board <- populateBoard(board)

	moves <- populateMoves(board)

	displayBoard(board)

	return(list(board=board,numMoves=moves))

	}



#===================== Populate the board ============================

populateBoard <- function(board) 

	{

	specials = c("R","N","B","K","Q","B","N","R")

	pawns = rep("P",times=8)

	board[1,] = specials

	board[8,] = tolower(rev(specials))

	board[2,] = pawns

	board[7,] = tolower(pawns)

	return(board)

	}



#===================== Populate numMoves ============================

populateMoves <- function(board) 

	{

	moves = matrix(data=rep(NA,times=8*8),ncol=8)

	for(i in 1:nrow(board))

		{

		for(j in 1:ncol(board))

			{

			if(board[i,j]!="") moves[i,j] = 0

			}

		}

	return(moves)

	}



#===================== Move a piece ============================

movePiece <- function(board,numMoves,currRow,currCol,nextRow,nextCol) 

	{

	# Check move is within the board row-wise

	if((nextRow>8)|(nextRow<1)) 

		{

		print("Move is not within the bounds of the board")

		flush.console()

		return(board)

		}

	# Check move is within the board col-wise

	if((nextCol>8)|(nextCol<1)) 

		{

		print("Move is not within the bounds of the board")

		flush.console()

		return(board)

		}	

	# Get the legal moves of the current piece

	movementMatrix <- movementRules(board,numMoves,currRow,currCol)

	# If the proposed next move is legal

	if(movementMatrix[nextRow,nextCol]==TRUE)

		{ 

		# Move the current piece to its next location

		board[nextRow,nextCol] = board[currRow,currCol]

		# Replace its last position with an empty space

		board[currRow,currCol] = ""

		# Repeat for numMoves (but add one)

		numMoves[nextRow,nextCol] = numMoves[currRow,currCol] + 1

		numMoves[currRow,currCol] = NA

		# If pawn has reached end of board swap

		condition1 = (board[nextRow,nextCol]=="P")|(board[nextRow,nextCol]=="p")

		condition2 = (nextRow==1)|(nextRow==8)

		if(condition1&condition2) board = pawnSwap(board,nextRow,nextCol)

		print(board)

		return(list(board=board,numMoves=numMoves))

		} else {

		# If it is not a legal move throw error

		print("Invalid move")

		print(board)

		return(list(board=board,numMoves=numMoves))

		}

	}



#===================== Display board ============================

displayBoard <- function(board) 

	{

	# Transpose the board so that it displays with x

	# and y axes mirrored. This is because a matrix counts

	# in the opposite orientation to the plotter

	board <- t(board)

	#Plot a white background

	plot(1:8,1:8,col="white",ylim=c(0.5,8.5),xlim=c(0.5,8.5))

	# Overlay a grid

	grid(8,8)

	# Loop over every row and column and display the current square

	for(i in 1:nrow(board))

		{

		for(j in 1:ncol(board))

			{

			text(board[i,j],x=i,y=j)

			}

		}

	}



#===================== Get move coordinates ============================

getMove <- function() 

	{

	# Get the position of two mouse clicks

	coords = locator(2)

	# Round to get whole numbers

	coords$x = round(coords$x)

	coords$y = round(coords$y)

	return(list(currX = coords$x[1], 

			currY = coords$y[1], 

			futureX = coords$x[2], 

			futureY=coords$y[2]))

	}





#===================== Movement rules ============================

movementRules <- function(board,numMoves,currRow,currCol) 

	{

	# Set the enemy as the opposite of the current piece

	if(length(grep(board[currRow,currCol],letters)==1)) enemy = LETTERS else enemy = letters



	# Pawn movements

	if((board[currRow,currCol]=="P")|(board[currRow,currCol]=="p")) allowed = PawnMovements(board,numMoves,currRow,currCol)



	# Rook movements

	if((board[currRow,currCol]=="R")|(board[currRow,currCol]=="r")) allowed = RookMovements(board,currRow,currCol,enemy)



	# Knight movements

	if((board[currRow,currCol]=="N")|(board[currRow,currCol]=="n")) allowed = KnightMovements(board,currRow,currCol,enemy)



	# Bishop movements

	if((board[currRow,currCol]=="B")|(board[currRow,currCol]=="b")) allowed = BishopMovements(board,currRow,currCol,enemy)



	# Queen movements

	if((board[currRow,currCol]=="Q")|(board[currRow,currCol]=="q")) allowed = QueenMovements(board,currRow,currCol,enemy)





	# King movements

	if((board[currRow,currCol]=="K")|(board[currRow,currCol]=="k")) allowed = KingMovements(board,numMoves,currRow,currCol,enemy)



	return(allowed)

	}



#===================== Pawn movements ============================

PawnMovements <- function(board,numMoves,currRow,currCol) 

	{

	# Pawn movements

	allowed = matrix(rep(FALSE,times=8*8),ncol=8)

	# If pawn is upper case

	if(board[currRow,currCol]=="P")

		{

		# Can move one forward if empty

		condition = board[currRow+1,currCol]==""

		if(condition) allowed[currRow+1,currCol] = TRUE

		# Can move forward 2 places if empty and first move

		condition1 = (numMoves[currRow,currCol]==0)

		if(condition1)

			{

			condition2 = (board[currRow+2,currCol]=="")

			if(condition2) allowed[currRow+2,currCol] = TRUE

			}

		# Can move one diagonal if enemy there

		nextColPoss = currCol+1

		if((nextColPoss>0)&(nextColPoss<8))

			{

			# Diagonal right

			condition = (board[currRow+1,nextColPoss]!="")&(length(grep(board[currRow+1,nextColPoss],letters))==1)

			if(condition) allowed[currRow+1,nextColPoss] = TRUE

			}

		nextColPoss = currCol-1

		if((nextColPoss>0)&(nextColPoss<8))

			{

			# Diagonal left

			condition = (board[currRow+1,nextColPoss]!="")&(length(grep(board[currRow+1,nextColPoss],letters))==1)

			if(condition) allowed[currRow+1,nextColPoss] = TRUE

			}

		} 

	# If pawn is lower case

	if(board[currRow,currCol]=="p")

		{

		# Can move one forward if empty

		condition = board[currRow-1,currCol]==""

		if(condition) allowed[currRow-1,currCol] = TRUE

		# Can move forward 2 places if empty and first move

		condition1 = (numMoves[currRow,currCol]==0)

		if(condition1)

			{

			condition2 = (board[currRow-2,currCol]=="")

			if(condition2) allowed[currRow-2,currCol] = TRUE

			}

		# Can move one diagonal if enemy there

		nextColPoss = currCol+1

		if((nextColPoss>0)&(nextColPoss<8))

			{

			# Diagonal right

			condition = (board[currRow-1,nextColPoss]!="")&(length(grep(board[currRow-1,nextColPoss],LETTERS))==1)

			if(condition) allowed[currRow-1,nextColPoss] = TRUE

			}

		nextColPoss = currCol-1

		if((nextColPoss>0)&(nextColPoss<8))

			{

			# Diagonal left

			condition = (board[currRow-1,nextColPoss]!="")&(length(grep(board[currRow-1,nextColPoss],LETTERS))==1)

			if(condition) allowed[currRow-1,nextColPoss] = TRUE

			}

		} 

	allowed

	}



#===================== Rook movements ============================

RookMovements <- function(board,currRow,currCol,enemy) 

	{

	# Rook movements

	allowed = matrix(rep(FALSE,times=8*8),ncol=8)	

	# Rook can move vertically

	# Verically down

	if(currRow<8) # if rook is not at top of board

		{

		for(i in 1:(8-currRow))

			{

			nextRowPoss = currRow+i

			# Can move if empty

			condition = board[nextRowPoss,currCol]==""

			if(condition) allowed[nextRowPoss,currCol] = TRUE

			if(!condition) 

				{

				# Can move if enemy in this position, cannot

				# if ally in this position

				if(length(grep(board[nextRowPoss,currCol],enemy))==1)

					{ 

					allowed[nextRowPoss,currCol] = TRUE

					} else {break}

				}

			}

		}

	# And vertically up

	if(currRow>1) # if rook is not at bottom of board

		{

		for(i in 1:(currRow-1))

			{

			nextRowPoss = currRow-i

			# Can move if empty

			condition = board[nextRowPoss,currCol]==""

			if(condition) allowed[nextRowPoss,currCol] = TRUE

			if(!condition) 

				{

				# Can move if enemy in this position, cannot

				# if ally in this position

				if(length(grep(board[nextRowPoss,currCol],enemy))==1)

					{ 

					allowed[nextRowPoss,currCol] = TRUE

					} else {break}

				}

			}

		}

	# Rook can move horizontally

	# To the right

	if(currCol<8) # if rook is not on right hand square

		{

		for(i in 1:(8-currCol))

			{

			nextColPoss = currCol+i

			# Can move if empty

			condition = board[currRow,nextColPoss]==""

			if(condition) allowed[currRow,nextColPoss] = TRUE

			if(!condition) 

				{

				# Can move if enemy in this position, cannot

				# if ally in this position

				if(length(grep(board[currRow,nextColPoss],enemy))==1)

					{ 

					allowed[currRow,nextColPoss] = TRUE

					} else {break}

				}

			}

		}

	# And to the left

	if(currCol>1) # if rook is not on left hand square

		{

		for(i in 1:(currCol-1))

			{

			nextColPoss = currCol-i

			# Can move if empty

			condition = board[currRow,nextColPoss]==""

			if(condition) allowed[currRow,nextColPoss] = TRUE

			if(!condition) 

				{

				# Can move if enemy in this position, cannot

				# if ally in this position

				if(length(grep(board[currRow,nextColPoss],enemy))==1)

					{ 

					allowed[currRow,nextColPoss] = TRUE

					} else {break}

				}

			}

		}

	allowed

	}



#===================== Knight movements ============================

KnightMovements <- function(board,currRow,currCol,enemy) 

	{

	# Knight movements

	allowed = matrix(rep(FALSE,times=8*8),ncol=8)

	# Possible knight moves

	possibleMoves <- matrix(c(

			-2, -1, 

			-2, +1, 

			-1, +2, 

			-1, -2, 

			+1, +2, 

			+1, -2, 

			+2, +1, 

			+2, -1),ncol=2,byrow=TRUE)

	for(i in 1:nrow(possibleMoves))

		{

		# Get coordinates of next potential move 

		nextRowPoss = currRow+possibleMoves[i,1]

		nextColPoss = currCol+possibleMoves[i,2]

		# if potential next move is outside of board skip it

		if((nextRowPoss>8)|(nextRowPoss<1)) next

		if((nextColPoss>8)|(nextColPoss<1)) next

		# is next move square empty?

		condition = board[nextRowPoss,nextColPoss]==""

		if(condition==TRUE) 

			{

			# Can move if spot is empty

			allowed[nextRowPoss,nextColPoss] = TRUE

			} else {

			# Can move if an enemy is in that spot

			if(length(grep(board[nextRowPoss,nextColPoss],enemy))==1) allowed[nextRowPoss,nextColPoss] = TRUE

			}

		}

	allowed

	}



#===================== Bishop movements ============================

BishopMovements <- function(board,currRow,currCol,enemy) 

	{

	# Bishop movements

	allowed = matrix(rep(FALSE,times=8*8),ncol=8)

	# Distance from edge

	upDist <- 8 - currRow

	downDist <- currRow - 1

	rightDist <- 8 - currCol

	leftDist <- currCol - 1

	# Rook can move diagonally in four directions

	# Set up directions

	coldirections <- c("currCol+i","currCol+i","currCol-i","currCol-i")

	rowdirections <- c("currRow+i","currRow-i","currRow-i","currRow+i")

	coldistances <- c(rightDist,rightDist,leftDist,leftDist)

	rowdistances <- c(upDist,downDist,downDist,upDist)

	# k = 1 : up and right

	# k = 2 : up and left

	# k = 3 : down and left

	# k = 4 : down and right

	for(k in 1:4)

		{

		for(i in 1:min(c(coldistances[k],rowdistances[k])))

			{

			nextRowPoss = eval(parse(text=rowdirections[k]))

			nextColPoss = eval(parse(text=coldirections[k]))

			if((nextRowPoss>8)|(nextRowPoss<1)) next

			if((nextColPoss>8)|(nextColPoss<1)) next

			# Can move if empty

			condition = board[nextRowPoss,nextColPoss]==""

			if(condition) allowed[nextRowPoss,nextColPoss] = TRUE

			if(!condition) 

				{

				# Can move if enemy in this position, cannot

				# if ally in this position

				if(length(grep(board[nextRowPoss,nextColPoss],enemy))==1)

					{ 

					allowed[nextRowPoss,nextColPoss] = TRUE

					} else {break}

				}

			}

		}

	allowed

	}





#===================== Queen movements ============================

QueenMovements <- function(board,currRow,currCol,enemy) 

	{

	# Queen movements

	# Combination of bishop and rook

	allowed = BishopMovements(board,currRow,currCol,enemy)

	# vector of bishop movements

	allowedVector = which(allowed==TRUE)

	allowed = RookMovements(board,currRow,currCol,enemy)

	# append vector of rook movements to vector ofbishop movements

	allowedVector = c(allowedVector, which(allowed==TRUE))

	# create vector of FALSE

	allowed <- rep(FALSE,times=8*8)

	# replace allowed moves with TRUE

	allowed[allowedVector]=TRUE

	# turn vector into matrix

	allowed <- matrix(data=allowed,ncol=8)

	allowed

	}



#===================== King movements ============================

# Currently the king can take an enemy, even if that puts him in check

KingMovements <- function(board,numMoves,currRow,currCol,enemy) 

	{

	# King movements

	# Matrix of possible moves for king

	allowed <- matrix(rep(FALSE,times=8*8),ncol=8)

	# Can move to any adjacent square

	possibleMoves <- matrix(c(

			 0,  1,

			 0, -1,

			 1,  0,

			-1,  0,

			 1,  1,

			 1, -1,

			-1, -1,

			-1,  1),ncol=2,byrow=TRUE)

	# Matrix of where enemy team can move

	enemyMoves = opposingTeamMoves(board,numMoves,enemy)

	for(i in 1:nrow(possibleMoves))

		{

		# Get coordinates of next potential move 

		nextRowPoss = currRow+possibleMoves[i,1]

		nextColPoss = currCol+possibleMoves[i,2]

		# if potential next move is outside of board skip it

		if((nextRowPoss>8)|(nextRowPoss<1)) next

		if((nextColPoss>8)|(nextColPoss<1)) next

		# is next move square empty?

		condition = board[nextRowPoss,nextColPoss]==""

		if(condition==TRUE) 

			{

			# Can move if spot is empty

			# But not if any other enemy can take that spot

			if(enemyMoves[nextRowPoss,nextColPoss]==0)

				{ 

				allowed[nextRowPoss,nextColPoss] = TRUE

				}

			} else {

			# Can move if an enemy is in that spot

			if(length(grep(board[nextRowPoss,nextColPoss],enemy))==1)

				{

				# But not if any other enemy can take that spot

				if(enemyMoves[nextRowPoss,nextColPoss]==0)

					{ 

					allowed[nextRowPoss,nextColPoss] = TRUE

					}

				}

			}

		}

	return(allowed)

	}









#===================== Moves allowed by the opposing team =============

opposingTeamMoves <- function(board,numMoves, enemy) 

	{

	allowed <- matrix(rep(FALSE,times=8*8),ncol=8)

	test = NULL; testi = 1;

	# Loop over every position on the board

	for(i in 1:nrow(board))

		{

		for(j in 1:ncol(board))

			{

			# If an enemy is on that spot, find its allowed moves

			if(length(grep(board[i,j],enemy)==1))

				{

				# Skip if spot is empty

				if(board[i,j]!="")

					{

					# skip if king for now

					# This is because opposingTeamMoves calls

					# KingMovements, and KingMovements calls

					# opposingTeamMoves, so there is infinite 

					# recursion.

					if((board[i,j]!="K")&(board[i,j]!="k"))

						{

						allowed = allowed + movementRules(board,numMoves,i,j)

						} 

					}

				}



			}

		}

	return(allowed)

	}



#===================== Pawn swap =============

pawnSwap <- function(board,Row,Col)

	{

	choice = 99

	choices <- c("R","N","B","Q")

	if(board[Row,Col]=="p") choices = tolower(choices)

	while(choice>4)

		{

		# Ask player what piece they want

		cat("\n","Please choose piece","\n",choices,"\n",c(1:4),"\n")

		# Scan for players entry 

		choice<-scan(n=1)

		}

	board[Row,Col] = choices[choice]

	return(board)

	}



#===================== Castling =============

castling <- function()

	{



	}
