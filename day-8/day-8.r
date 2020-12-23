# advent of code
# day 8

# part 1
instructions <- readLines('day-8.txt')

testInstructions <- function(instructions, returnAcc = F){
  index <- 1
  instructionsRun <- c()
  accumulator <- 0
  while(index <= length(instructions)){
    if(index %in% instructionsRun | index < 1){
      if(returnAcc == T){
        return(paste(F, accumulator, sep = ': '))
      } else {
        return(F)
      }
      break
    } else {
      if(substr(instructions[index], 1, 3) == 'nop') {
        instructionsRun <- c(instructionsRun, index)
        index <- index + 1
      } else if (substr(instructions[index], 1, 3) == 'acc') {
        if(substr(instructions[index], 5, 5) == '+'){
          accumulator <- accumulator + as.integer(substr(instructions[index], 6, nchar(instructions[index])))
        } else {
          accumulator <- accumulator - as.integer(substr(instructions[index], 6, nchar(instructions[index])))
        }
        instructionsRun <- c(instructionsRun, index)
        index <- index + 1
      } else {
        instructionsRun <- c(instructionsRun, index)
        if(substr(instructions[index], 5, 5) == '+'){
          index <- index + as.integer(substr(instructions[index], 6, nchar(instructions[index])))
        } else {
          index <- index - as.integer(substr(instructions[index], 6, nchar(instructions[index])))
        }
      }
    }
  }
  if(returnAcc == T){
    return(paste(T, accumulator, sep = ': '))
  } else {
    return(T)
  }
}

testInstructions(instructions)

# part 2
for(i in 1:length(instructions)){
  trialInstructions <- instructions
  if(substr(trialInstructions[i], 1, 3) == 'jmp'){
    substr(trialInstructions[i], 1, 3) <- 'nop'
    if(testInstructions(trialInstructions) == T){
      print(testInstructions(trialInstructions, T))
      break
    }
  } else if(substr(trialInstructions[i], 1, 3) == 'nop'){
    substr(trialInstructions[i], 1, 3) <- 'jmp'
    if(testInstructions(trialInstructions) == T){
      print(testInstructions(trialInstructions, T))
      break
    }
  }
}


