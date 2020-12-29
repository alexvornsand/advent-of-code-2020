# advent of code
# day 15

# part 1
gameNumbers <- list(
  '9' = 1,
  '12' = 2,
  '1' = 3,
  '4' = 4,
  '17' = 5,
  '0' = 6,
  '18' = 7
)
last <- 18

nextGameNumber <- function(last, gameNumbers = gameNumbers){
  if(length(gameNumbers[[as.character(last)]]) == 1){
    return(0)
  } else {
    gameNumbers[[as.character(last)]] <- rev(rev(gameNumbers[[as.character(last)]])[1:2])
    lastTimes <- rev(gameNumbers[[as.character(last)]])
    return(lastTimes[1] - lastTimes[2])
  }
}

for(i in 8:30000000){
  if(i %% 10000 == 0){print(i)}
  nextNumber <- nextGameNumber(last, gameNumbers)
  gameNumbers[[as.character(nextNumber)]] <- c(gameNumbers[[as.character(nextNumber)]], i)
  last <- nextNumber
}
