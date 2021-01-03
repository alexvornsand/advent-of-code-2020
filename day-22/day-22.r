# advent of code
# day 22

# part 1
hands <- strsplit(unlist(strsplit(sub(',,', '\n', gsub('Player\\s\\d:\\,', '', paste(readLines('day-22.txt'), collapse = ',')), fixed = T), split = '\n')), split = ',')
handA <- hands[[1]]
handB <- hands[[2]]

playCombat <- function(x, y){
  handA <- x
  handB <- y
  while(min(length(handA), length(handB)) > 0){
    p1card <- as.integer(handA[1])
    p2card <- as.integer(handB[1])
    winner <- which(c(p1card > p2card, p1card < p2card))
    if(winner == 1){
      handB <- handB[which(!(handB == p2card))]
      handA <- c(handA[which(!handA == p1card)], p1card, p2card)
    } else {
      handA <- handA[which(!(handA == p1card))]
      handB <- c(handB[which(!handB == p2card)], p2card, p1card)
    }
  }
  return(sum(as.integer(rev(c(handA, handB))) * as.integer(which(rev(c(handA, handB)) == rev(c(handA, handB))))))
}

playCombat(handA, handB)

# part 2
playRecursiveCombat <- function(handA, handB){
  previousHands <- c() 
  while(min(length(handA), length(handB)) > 0){
    handState <- paste(paste(handA, collapse = '-'), '!', paste(handB, collapse = '-'), sep = '')
    if(handState %in% previousHands){
      return(c(1, sum(as.integer(rev(handA)) * as.integer(which(rev(handA) == rev(handA))))))
    } else {
      previousHands <- c(previousHands, handState)
      p1card <- as.integer(handA[1])
      p2card <- as.integer(handB[1])
      if(as.integer(p1card) <= length(handA) - 1 & as.integer(p2card) <= length(handB) - 1){
        winner <- playRecursiveCombat(handA[2:length(handA)], handB[2:length(handB)])[1]
      } else {
        winner <- which(c(p1card > p2card, p1card < p2card))
      }
      if(winner == 1){
        handB <- handB[which(!(handB == p2card))]
        handA <- c(handA[which(!handA == p1card)], p1card, p2card)
      } else {
        handA <- handA[which(!(handA == p1card))]
        handB <- c(handB[which(!handB == p2card)], p2card, p1card)
      }
    }
  }
  gameWinner <- which(c(length(handA) != 0, length(handB) != 0))
  winnerScore <- sum(as.integer(rev(c(handA, handB))) * as.integer(which(rev(c(handA, handB)) == rev(c(handA, handB)))))
  return(c(gameWinner, winnerScore))
}

playRecursiveCombat(handA, handB)
