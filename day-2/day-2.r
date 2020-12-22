# advent of code
# day 2

# part 1
passwordData <- readLines('day-2.txt')

evaluatePassword <- function(pw){
  pwData <- read.table(text = gsub('-', '\t', gsub(':', '', gsub(' ', '\t', pw))), col.names = c('min', 'max', 'character', 'password'))
  pwChars <- unlist(strsplit(pwData$password, split = ''))
  count <- sum(pwChars == pwData$character)
  return(count >= pwData$min & count <= pwData$max)
}

sum(sapply(passwordData, evaluatePassword))

# part 2

evaluatePassword <- function(pw){
  pwData <- read.table(text = gsub('-', '\t', gsub(':', '', gsub(' ', '\t', pw))), col.names = c('min', 'max', 'character', 'password'))
  pwChars <- unlist(strsplit(pwData$password, split = ''))
  return(sum(c(pwData$character == pwChars[pwData$min], pwData$character == pwChars[pwData$max])) == 1)
}

sum(sapply(passwordData, evaluatePassword))
