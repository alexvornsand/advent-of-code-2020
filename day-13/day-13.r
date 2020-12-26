# advent of code
# day 13

# part 1
departureTime <- as.integer(readLines('day-13.txt')[1])
busRoutes <- as.integer(unlist(strsplit(readLines('day-13.txt')[2], split = ',')))

waitTimes <- busRoutes - (departureTime %% busRoutes)
nextBus <- busRoutes[which(waitTimes == min(waitTimes, na.rm = T))]

nextBus * min(waitTimes, na.rm = T)

# part 2

 correctOffset <- function(t, rte, busRoutes = busRoutes){
  if(is.na(rte)){
    return(NA)
  } else {
    return(t %% rte ==  rte - (which(busRoutes == rte) - 1))
  }
}

t <- 0
while(T){
  print(t)
  inSequence <- unlist(sapply(busRoutes, correctOffset, t = t, busRoutes = busRoutes))
  if(all(inSequence, na.rm = T)){
    print(as.string(t))
    break
  } else {
    t <- t + prod(busRoutes[which(inSequence == T)])
  }
}
