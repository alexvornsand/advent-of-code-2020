# advent of code
# day 22

# part 1
input <- readLines('day-22.txt')

findCombatWinner <- function(input, partTwo = F){
  hands <- strsplit(unlist(strsplit(sub(',,', '\n', gsub('Player\\s\\d:\\,', '', paste(input, collapse = ',')), fixed = T), split = '\n')), split = ',')
  handA <- as.integer(hands[[1]])
  handB <- as.integer(hands[[2]])
  playCombat <- function(handA, handB, partTwo){
    playStates <- c()
    while(length(handA) > 0 & length(handB > 0)){
      playState <- paste(paste(handA, collapse = '-'), '!', paste(handB, collapse = '-'), sep = '')
      if(partTwo == T & playState %in% playStates){
        return(c(1, sum(rev(handA) * which(rev(handA) == rev(handA)))))
      } else {
        playStates <- c(playStates, playState)
        cardA <- handA[1]
        cardB <- handB[1]
        if(partTwo == T & length(handA) - 1 >= cardA & length(handB) - 1 >= cardB){
          winner <- playCombat(handA[2:(cardA + 1)], handB[2:(cardB + 1)], partTwo = T)[1]
        } else {
          winner <- which.max(c(cardA, cardB))
        }
        if(winner == 1){
          handB <- handB[which(!(handB == cardB))]
          handA <- c(handA[which(!handA == cardA)], cardA, cardB)
        } else {
          handA <- handA[which(!(handA == cardA))]
          handB <- c(handB[which(!handB == cardB)], cardB, cardA)
        }
      }
    }
    winner <- which.max(c(length(handA), length(handB)))
    score <- sum(rev(handA) * which(rev(handA) == rev(handA)))
    return(c(winner, score))
  }
  return(playCombat(handA, handB, partTwo = partTwo)[2])
}

findCombatWinner(input, partTwo = T)

findCombatWinner <- function(input, partTwo = F){
  hands <- strsplit(unlist(strsplit(sub(',,', '\n', gsub('Player\\s\\d:\\,', '', paste(input, collapse = ',')), fixed = T), split = '\n')), split = ',')
  handA <- hands[[1]]
  handB <- hands[[2]]
  playCombat <- function(handA, handB, partTwo){
    previousHands <- c() 
    while(min(length(handA), length(handB)) > 0){
      handState <- paste(paste(handA, collapse = '-'), '!', paste(handB, collapse = '-'), sep = '')
      if(partTwo == T & handState %in% previousHands){
        return(c(1, sum(as.integer(rev(handA)) * as.integer(which(rev(handA) == rev(handA))))))
      } else {
        previousHands <- c(previousHands, handState)
        p1card <- as.integer(handA[1])
        p2card <- as.integer(handB[1])
        if(partTwo == T & as.integer(p1card) <= length(handA) - 1 & as.integer(p2card) <= length(handB) - 1){
          winner <- playCombat(handA[2:(p1card + 1)], handB[2:(p2card + 1)], partTwo = T)[1]
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
      winner <- which(c(p1card > p2card, p1card < p2card))
      if(winner == 1){
        handB <- handB[which(!(handB == p2card))]
        handA <- c(handA[which(!handA == p1card)], p1card, p2card)
      } else {
        handA <- handA[which(!(handA == p1card))]
        handB <- c(handB[which(!handB == p2card)], p2card, p1card)
      }
    }
    gameWinner <- which(c(length(handA) != 0, length(handB) != 0))
    winnerScore <- sum(as.integer(rev(c(handA, handB))) * as.integer(which(rev(c(handA, handB)) == rev(c(handA, handB)))))
    return(c(gameWinner, winnerScore))
  }
  return(playCombat(handA, handB, partTwo = partTwo)[2])
}

findCombatWinner(input, partTwo = T)

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
        winner <- playRecursiveCombat(handA[2:(p1card + 1)], handB[2:(p2card + 1)])[1]
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
