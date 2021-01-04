# advent of code
# day 2

# part 1
library(tictoc)
input <- readLines('day-02.txt')

countValidPasswords <- function(passwordData, partTwo = F){
  evaluatePassword <- function(pw, partTwo){
    pwData <- read.table(text = gsub('-', '\t', gsub(':', '', gsub(' ', '\t', pw))), col.names = c('min', 'max', 'character', 'password'))
    pwChars <- unlist(strsplit(pwData$password, split = ''))
    if(partTwo == F){
      count <- sum(pwChars == pwData$character)
      return(count >= pwData$min & count <= pwData$max)
    } else {
      return(sum(c(pwData$character == pwChars[pwData$min], pwData$character == pwChars[pwData$max])) == 1)
    }
  }
  return(sum(sapply(passwordData, evaluatePassword, partTwo = partTwo)))
}

tictoc::tic()
countValidPasswords(input)
tictoc::toc()

# part 2
tictoc::tic()
countValidPasswords(passwordData, partTwo = T)
tictoc::toc()
