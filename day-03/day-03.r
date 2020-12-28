# advent of code
# day 3

# part 1
map <- readLines('day-3.txt')

binaryMap <- function(str){
  return(1 * (unlist(strsplit(str, split = '')) == '#'))
}

bMap <- matrix(sapply(map, binaryMap), ncol = 31, byrow = T)

traverseMap <- function(xDelta, yDelta){
  x <- 1
  y <- 1
  trees <- 0
  while(y < 323){
    y <- y + yDelta
    if((x + xDelta) != 31){
      x <- (x + xDelta) %% 31
    } else {
      x <- 31
    }
    trees <- trees + bMap[y, x]
  }
  return(trees)
}

traverseMap(3, 1)

# part 2
traverseMap(1, 1) * traverseMap(3, 1) * traverseMap(5, 1) * traverseMap(7, 1) * traverseMap(1, 2)
