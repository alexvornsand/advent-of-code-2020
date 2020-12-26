# advent of code
# day 11

# part 1
library(pbapply)
grid <- do.call(expand.grid, list(x = 1:97, y = 1:93))
grid$value <- as.integer(gsub('L', 1, gsub('.', NA, unlist(strsplit(readLines('day-11.txt'), split = '')), fixed = T)))

gridStateChange <- function(i, grid){
  x <- grid$x[i]
  y <- grid$y[i]
  value <- grid$value[i]
  neighbors <- sum(sum(merge(do.call(expand.grid, list(x = (x-1):(x+1), y = (y-1):(y+1))), grid, by = c('x', 'y'), all = F)$value, na.rm = T), -value, na.rm = T)
  if(is.na(value)){
    return(NA)
  } else if(value == 1){
    if(neighbors >= 4){
      return(0)
    } else {
      return(1)
    }
  } else {
    if(neighbors == 0){
      return(1)
    } else {
      return(0)
    }
  }
}

findSteadyGrid <- function(grid){
  oldGrid <- cbind(do.call(expand.grid, list(x = 1:97, y = 1:93)), NA)
  newGrid <- grid
  while(!identical(oldGrid, newGrid)){
    oldGrid <- newGrid
    newGridValues <- unlist(pbsapply(1:nrow(oldGrid), gridStateChange, grid = oldGrid))
    newGrid <- oldGrid
    newGrid$value <- newGridValues
  }
  return(sum(newGrid$value, na.rm = T))
}

findSteadyGrid(grid)

# part 2
grid <- do.call(expand.grid, list(x = 1:97, y = 1:93))
grid$value <- as.integer(gsub('L', 1, gsub('.', NA, unlist(strsplit(readLines('day-11.txt'), split = '')), fixed = T)))

gridStateChange2 <- function(i, grid){
  x <- grid$x[i]
  y <- grid$y[i]
  value <- grid$value[i]
  i <- 1
  N <- while()
  if(is.na(value)){
    return(NA)
  } else if(value == 1){
    if(neighbors >= 4){
      return(0)
    } else {
      return(1)
    }
  } else {
    if(neighbors == 0){
      return(1)
    } else {
      return(0)
    }
  }
}

findSteadyGrid(grid)
