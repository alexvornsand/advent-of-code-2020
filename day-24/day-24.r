# advent of code
# day 24

# part 1
directions <- readLines('day-24.txt')

simplifyDirections <- function(direction){
  direction <- gsub('se', 'SE,', direction)
  direction <- gsub('sw', 'SW,', direction)
  direction <- gsub('ne', 'NE,', direction)
  direction <- gsub('nw', 'NW,', direction)
  direction <- gsub('e', 'E,', direction)
  direction <- gsub('w', 'W,', direction)
  direction <- unlist(strsplit(direction, split = ','))
  netDirections <- table(direction)
  for(d in c('E', 'W', 'NE', 'NW', 'SE', 'SW')){
    if(!(d %in% names(netDirections))){
      netDirections[d] <- 0
    }
  }
  netEastWest <- netDirections['E'] + 0.5 * netDirections['NE'] + 0.5 * netDirections['SE'] - netDirections['W'] - 0.5 * netDirections['NW'] - 0.5 * netDirections['SW']
  netNorthSouth <- 0.5 * netDirections['NE'] + 0.5 * netDirections['NW'] - 0.5 * netDirections['SE'] - 0.5 * netDirections['SW']
  return(paste(as.character(netEastWest), as.character(netNorthSouth), sep = ', '))
}

flipTiles <- sapply(directions, simplifyDirections)

tileColor <- data.frame('x' = numeric(), 'y' = numeric(), 'color' = integer())
for(tile in flipTiles){
  tileX <- as.numeric(unlist(strsplit(tile, split = ', '))[1])
  tileY <- as.numeric(unlist(strsplit(tile, split = ', '))[2])
  if(length(which(tileColor$x == tileX & tileColor$y == tileY)) == 1){
    tileColor[which(tileColor$x == tileX & tileColor$y == tileY), 'color'] <- tileColor[which(tileColor$x == tileX & tileColor$y == tileY), 'color'] * -1
  } else {
    tileColor <- rbind(tileColor, data.frame('x' = tileX, 'y' = tileY, 'color' = -1))
  }
}

sum(tileColor$color == -1)

# part 2
threatenToFlip <- function(tile){
  x <- tile[1]
  y <- tile[2]
  color <- tile[3]
  if(length(which(tileColor$x == x + 1 & tileColor$y == y)) == 1){
    e <- tileColor$color[which(tileColor$x == x + 1 & tileColor$y == y)]
  } else {e <- 1}
  if(length(which(tileColor$x == x - 1 & tileColor$y == y)) == 1){
    w <- tileColor$color[which(tileColor$x == x - 1 & tileColor$y == y)]
  } else {w <- 1}
  if(length(which(tileColor$x == x + .5 & tileColor$y == y + .5)) == 1){
    ne <- tileColor$color[which(tileColor$x == x + .5 & tileColor$y == y + .5)]
  } else {ne <- 1}
  if(length(which(tileColor$x == x - .5 & tileColor$y == y + .5)) == 1){
    nw <- tileColor$color[which(tileColor$x == x - .5 & tileColor$y == y + .5)]
  } else {nw <- 1}
  if(length(which(tileColor$x == x + .5 & tileColor$y == y + .5)) == 1){
    se <- tileColor$color[which(tileColor$x == x + .5 & tileColor$y == y - .5)]
  } else {se <- 1}
  if(length(which(tileColor$x == x - .5 & tileColor$y == y - .5)) == 1){
    sw <- tileColor$color[which(tileColor$x == x - .5 & tileColor$y == y - .5)]
  } else {sw <- 1}
  neighbors <- c(e, w, ne, nw, se, sw)
  print(paste(color, paste(neighbors, collapse = ' '), sep = ': '))
  if(color == 1){
    if(sum(neighbors) == 2){
      return(-1)
    } else {
      return(1)
    }
  } else {
    if(sum(neighbors) == 4){
      return(-1)
    } else {
      return(1)
    }
  }
}

apply(tileColor, 1, threatenToFlip)

for(i in 1:100){
  tileColor$color <- apply(tileColor, 1, threatenToFlip)
}
sum(tileColor$color == -1)
