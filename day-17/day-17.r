# advent of code
# day 17

# part 1
values <- as.integer(unlist(strsplit(gsub('.', '0', gsub('#', '1', readLines('day-17.txt')), fixed = T), split = '')))

evaluateNeighbors3D <- function(row, grid){
  x <- grid$x[row]
  y <- grid$y[row]
  z <- grid$z[row]
  neighbors <- do.call(expand.grid, list(x = (x-1):(x+1), y = (y-1):(y+1), z = (z-1):(z+1)))
  testNeighbor <- function(i, neighbors){
    if(length(which(grid$x == neighbors$x[i] & grid$y == neighbors$y[i] & grid$z == neighbors$z[i])) != 0){
      return(grid[which(grid$x == neighbors$x[i] & grid$y == neighbors$y[i] & grid$z == neighbors$z[i]), 'value'])
    } else {
      return(0)
    }
  }
  numNeighbors <- sum(unlist(sapply(1:nrow(neighbors), testNeighbor, neighbors = neighbors))) - grid$value[which(grid$x == x & grid$y == y & grid$z == z)]
  if(grid$value[row] == 1){
    if(numNeighbors == 2 | numNeighbors == 3){
      return(1)
    } else {
      return(0)
    }
  } else {
    if(numNeighbors == 3){
      return(1)
    } else {
      return(0)
    }
  }
}

runCycles3D <- function(values = values, n){
  # set up initial grid
  xmin <- 0
  xmax <- 7
  ymin <- 0
  ymax <- 7
  zmin <- 0
  zmax <- 0
  newGrid <- do.call(expand.grid, list(x = xmin:xmax, y = ymin:ymax, z = zmin:zmax))
  newGrid <- cbind(newGrid, values)
  colnames(newGrid) <- c('x', 'y', 'z', 'value')
  for(i in 1:n){
    # expand grid
    oldGrid <- newGrid
    xmin <- xmin - 1
    xmax <- xmax + 1
    ymin <- ymin - 1
    ymax <- ymax + 1
    zmin <- zmin - 1
    zmax <- zmax + 1
    expandedGrid <- do.call(expand.grid, list(x = xmin:xmax, y = ymin:ymax, z = zmin:zmax))
    expandedGrid <- merge(expandedGrid, oldGrid, by = c('x', 'y', 'z'), all.x = T)
    expandedGrid$value[which(is.na(expandedGrid$value))] <- 0
    colnames(expandedGrid) <- c('x', 'y', 'z', 'value')
    # test each grid position
    newVals <- unlist(sapply(1:nrow(expandedGrid), evaluateNeighbors3D, grid = expandedGrid))
    # write new grid
    newGrid <- expandedGrid
    newGrid$value <- newVals
  }
  # evaluate final grid
  return(sum(newGrid$value))
}

runCycles4D(values, 6)

# part 2
evaluateNeighbors4D <- function(row, grid){
  w <- grid$w[row]
  x <- grid$x[row]
  y <- grid$y[row]
  z <- grid$z[row]
  neighbors <- do.call(expand.grid, list(w = (w-1):(w+1), x = (x-1):(x+1), y = (y-1):(y+1), z = (z-1):(z+1)))
  testNeighbor <- function(i, neighbors){
    if(length(which(grid$w == neighbors$w[i] & grid$x == neighbors$x[i] & grid$y == neighbors$y[i] & grid$z == neighbors$z[i])) != 0){
      return(grid[which(grid$w == neighbors$w[i] & grid$x == neighbors$x[i] & grid$y == neighbors$y[i] & grid$z == neighbors$z[i]), 'value'])
    } else {
      return(0)
    }
  }
  numNeighbors <- sum(unlist(sapply(1:nrow(neighbors), testNeighbor, neighbors = neighbors))) - grid$value[which(grid$w == w & grid$x == x & grid$y == y & grid$z == z)]
  if(grid$value[row] == 1){
    if(numNeighbors == 2 | numNeighbors == 3){
      return(1)
    } else {
      return(0)
    }
  } else {
    if(numNeighbors == 3){
      return(1)
    } else {
      return(0)
    }
  }
}

runCycles4D <- function(values = values, n){
  # set up initial grid
  wmin <- 0
  wmax <- 0
  xmin <- 0
  xmax <- 7
  ymin <- 0
  ymax <- 7
  zmin <- 0
  zmax <- 0
  newGrid <- do.call(expand.grid, list(w = wmin:wmax, x = xmin:xmax, y = ymin:ymax, z = zmin:zmax))
  newGrid <- cbind(newGrid, values)
  colnames(newGrid) <- c('w', 'x', 'y', 'z', 'value')
  for(i in 1:n){
    print(i)
    # expand grid
    oldGrid <- newGrid
    wmin <- wmin - 1
    wmax <- wmax + 1
    xmin <- xmin - 1
    xmax <- xmax + 1
    ymin <- ymin - 1
    ymax <- ymax + 1
    zmin <- zmin - 1
    zmax <- zmax + 1
    expandedGrid <- do.call(expand.grid, list(w = wmin:wmax, x = xmin:xmax, y = ymin:ymax, z = zmin:zmax))
    expandedGrid <- merge(expandedGrid, oldGrid, by = c('w', 'x', 'y', 'z'), all.x = T)
    expandedGrid$value[which(is.na(expandedGrid$value))] <- 0
    colnames(expandedGrid) <- c('w', 'x', 'y', 'z', 'value')
    # test each grid position
    newVals <- unlist(sapply(1:nrow(expandedGrid), evaluateNeighbors4D, grid = expandedGrid))
    # write new grid
    newGrid <- expandedGrid
    newGrid$value <- newVals
  }
  # evaluate final grid
  return(sum(newGrid$value))
}

runCycles4D(values, 6)


