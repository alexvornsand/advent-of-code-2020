# advent of code
# day 5

# part 1
library(compositions)
library(microbenchmark)

input <- readLines('day-05.txt')

findSeatID <- function(input, partTwo = F){
  getSeatID <- function(boardingPass){
    row <- unbinary(substring(gsub('B', '1', gsub('F', '0', boardingPass)), 1, 7))
    column <- unbinary(substring(gsub('R', '1', gsub('L', '0', boardingPass)), 8, 10))
    return(8 * row + column)
  }
  if(partTwo == T){
    seats <- apply(do.call(expand.grid, list('row' = 1:128, 'column' = 0:7)), 1, function(x){return(x[[1]] * 8 + x[[2]])})
    takenSeats <- unlist(sapply(input, getSeatID))
    for(seat in seats){
      if(!(seat %in% takenSeats) & (seat + 1) %in% takenSeats & (seat - 1) %in% takenSeats){
        return(seat)
        break
      }
    }
  } else {
    return(max(unlist(sapply(input, getSeatID))))
  }
}

microbenchmark(findSeatID(input))

# part 2
microbenchmark(findSeatID(input, partTwo = T))
