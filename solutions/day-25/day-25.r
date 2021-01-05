# advent of code
# day 25

# part 1
cardPublicKey <- as.integer(readLines('day-25.txt')[1])
doorPublicKey <- as.integer(readLines('day-25.txt')[2])

findLoopSize <- function(subjNo, key){
  findKey <- 1
  loopSize <- 0
  while(findKey != key){
    loopSize <- loopSize + 1
    findKey <- (findKey * subjNo) %% 20201227
  }
  return(loopSize)
}

cardLoopSize <- findLoopSize(7, cardPublicKey)
doorLoopSize <- findLoopSize(7, doorPublicKey)

transformSubjNo <- function(subjNo, loopSize){
  val <- 1
  for(i in 1:loopSize){
    val <- (val * subjNo) %% 20201227
  }
  return(val)
}

encryptionKeyC <- transformSubjNo(doorPublicKey, cardLoopSize)
encryptionKeyD <- transformSubjNo(cardPublicKey, doorLoopSize)