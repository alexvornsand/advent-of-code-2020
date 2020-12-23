# advent of code
# day 5

# part 1
library(compositions)
boardingPasses <- readLines('day-5.txt')

getSeatID <- function(boardingPass){
  row <- unbinary(substring(gsub('B', '1', gsub('F', '0', boardingPass)), 1, 7))
  column <- unbinary(substring(gsub('R', '1', gsub('L', '0', boardingPass)), 8, 10))
  return(8 * row + column)
}

max(unlist(sapply(boardingPasses, getSeatID)))

# part 2
seats <- c()
for(r in 1:128){
  for(c in 0:7){
    seats <- c(seats, (r * 8) + c)
  }
}

takenSeats <- unlist(sapply(boardingPasses, getSeatID))

for(seat in seats){
  if(!(seat %in% takenSeats) & (seat + 1) %in% takenSeats & (seat - 1) %in% takenSeats){
    print(seat)
    break
  }
}
