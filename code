rm(list = ls())

four.in.a.row = function(player, v, debug=FALSE) { #to judge if connected by four
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  if ( length(v) < 4) {
    return(FALSE)
  }
  for (i in 1 : (length(v)-3)) {
    if(all(c(v[i], v[i+1], v[i+2], v[i+3]) == player)) {
      return(TRUE)
    }
  }
  return(FALSE)#
}


won = function(player, board, r, c, debug=FALSE) { # to test win
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  stopifnot((0 <= r & r <= nrow(board)))
  stopifnot((0 <= c & c <= ncol(board)))
  test.vector <- c(board[r, ], board[, c], board[row(board) - col(board) == r-c], board[row(board) + col(board) == r+c])
  test_row <- four.in.a.row(player, board[r, ])
  test_col <- four.in.a.row(player, board[, c])
  test_dia <- four.in.a.row(player, board[row(board) - col(board) == r-c])
  test_red <- four.in.a.row(player, board[row(board) + col(board) == r+c])
  if(any(c(test_row, test_col, test_dia, test_red) == TRUE)) {
    return(TRUE)
  }
  return(FALSE)
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  if (!('' %in% board)) {
    return(NULL)
  }
  i <- 1
  while (board[i, col] != 'X' & board[i, col] != 'O') {
    i <- i+1
    if (i > nrow(board)){
      break
    }
  }
  return(i-1)
}


par(pty="s") # square plot type
y = rep(1:6, each = 7)
x = rep(1:7, times = 6)
symbols(x, y, squares=rep(1, times=42),
        inches=FALSE, # match squares to axes
        xlim=c(0,8),
        ylim=c(7,0),
        main = 'Connect Four' 
)


board = matrix(data=rep("", 42), nrow=6, ncol=7)

board.is.full = function(board) {
  return(!("" %in% c(board)))
}


choose_symbol <- function() {
  repeat{
  cat("Which symbol do you like: X or O")
  man <- scan(what = character(), n = 1)
  if (man != 'X' & man != "O") {
    cat('Error, please type X or O')
  } else {
    break
  }
}
  return(man)
}

human <- choose_symbol()

choose_order <- function() {
  repeat{
  cat('Who is the first: ME or PC')
  decision <- scan(what = character(), n = 1)
  if (decision != 'ME' & decision != 'PC') {
    cat('Error, please type ME or PC')
  } else {
    break
  }
}
  return(decision)

}


if (choose_order() == 'ME') {
  repeat{
    index = identify(x, y, n=1, plot=FALSE)
    col = x[index]
    row = largest.empty.row(board, col)
    board[row, col] <- human
    print(board)
    text(x = col, y = row, label = human, col = 'blue')
    if (won(player = human, board, r = row, c = col)) {
      text(x=3.5, y=3, labels=paste(" You won!"), col = "blue", cex = 3)
      break
    }
    
    repeat {
    indices.empty = which(c(board) == "")
    index = sample(indices.empty, size=1)
    row = y[index]
    col = x[index]
    if (largest.empty.row(board, col) == row) {
       break
      }
    }
    pc <- ifelse(test = (human == 'X'), yes = 'O', no = "X")
    board[row, col] = pc
     if (board.is.full(board) == TRUE) {
      break
    }
    print(board)
    text(x = col, y = row, label = pc, col = 'red')
    if (won(player = pc, board, r = row, c = col)) {
      text(x=3.5, y=3, labels=paste(" PC won!"), col = "blue", cex = 3)
      break
    }
  }
 
} else {
  repeat {
    repeat {
    indices.empty = which(c(board) == "")
    index = sample(indices.empty, size=1)
    row = y[index]
    col = x[index]
    if (largest.empty.row(board, col) == row) {
      break
     }
    }
    
    pc <- ifelse(test = (human == 'X'), yes = 'O', no = "X")
    board[row, col] = pc
     if (board.is.full(board) == TRUE) {
      break
    }
    print(board)
    text(x = col, y = row, label = pc, col = 'red')
    if (won(player = pc, board, r = row, c = col)) {
      text(x=3.5, y=3, labels=paste(" PC won!"), col = "red", cex = 3)
      break
    }
    
    repeat { 
      index = identify(x, y, n=1, plot=FALSE)
      row = y[index]
      col = x[index]
      if (largest.empty.row(board, col) == row) {
        break
     }
    }
    board[row, col] <- human
     if (board.is.full(board) == TRUE) {
      break
    }
    print(board)
    text(x = col, y = row, label = human, col = 'blue')
    if (won(player = human, board, r = row, c = col)) {
      text(x=3.5, y=3, labels=paste(" You won!"), col = "blue", cex = 3)
      break
    }
  }
}
