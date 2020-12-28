# advent of code
# day 15

# part 1
gameNumbers <- c(9, 12, 1, 4, 17, 0, 18)

nextGameNumber <- function(gameNumbers = gameNumbers){
  if(length(which(gameNumbers == gameNumbers[length(gameNumbers)])) == 1){
    return(0)
  } else {
    return(length(gameNumbers) - which(gameNumbers == gameNumbers[length(gameNumbers)])[length(which(gameNumbers == gameNumbers[length(gameNumbers)])) - 1])
  }
}

for(i in 1:30000000){
  gameNumbers <- c(gameNumbers, nextGameNumber(gameNumbers))
}

gameNumbers[2020]
