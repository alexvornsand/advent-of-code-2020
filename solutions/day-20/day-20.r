# advent of code
# day 20

# part 1
images <- readLines('day-20.txt')

findBorders <- function(input){
  tileList <<- list()
  sapply(seq(from = 1, to = length(images), by = 12), function(x){tileList[[sub(':', '', images[x], fixed = T)]] <<- matrix(unlist(strsplit(images[(x + 1):(x + 10)], split = '')), nrow = 10, byrow = T)})
  
  evaluatePairOfTiles <- function(matchup){
    tileNameA <- matchup[1] 
    tileNameB <- matchup[2]
    tileA <- tileList[[tileNameA]]
    tileB <- tileList[[tileNameB]]
    N <- tileA[1,]
    W <- tileA[, 1]
    E <- tileA[, 10]
    S <- tileA[10,]
    n <- tileB[1,]
    nR <- rev(tileB[1,])
    w <- tileB[, 1]
    wR <- rev(tileB[, 1])
    e <- tileB[, 10]
    eR <- rev(tileB[, 10])
    s <- tileB[10,]
    sR <- rev(tileB[10,])
    matchUps <- do.call(expand.grid, list(tileNameA = c('N', 'W', 'E', 'S'), tileNameB = c('n', 'nR', 'w', 'wR', 'e', 'eR', 's', 'sR')))
    matchUps <- sapply(matchUps, as.character)
    matchUpResults <- apply(matchUps, 1, function(x){return(identical(eval(parse(text = eval(x[1]))), eval(parse(text = eval(x[2])))))})
    links <- matchUps[which(matchUpResults == T),]
    if(length(links > 0)){
      response <- paste(unlist(apply(as.data.frame(t(links)), 1, function(x){paste(x[1], x[2], sep = ': ')})), collapse = '; ')
    } else {
      response <- 'NA'
    }
    return(paste(tileNameA, '-', tileNameB, ':: ', response, sep = ''))
  }
  
  tileMatchups <- do.call(expand.grid, list('A' = names(tileList), 'B' = names(tileList)))
  tileMatchups <- sapply(tileMatchups[which(tileMatchups[, 1] != tileMatchups[, 2]),], as.character)
  
  matchUpResults <- apply(tileMatchups, 1, evaluatePairOfTiles)
  gridNeighbors <- data.frame(
    tile = character(), 
    N = character(),
    E = character(),
    S = character(),
    W = character()
  )
  for(i in 1:length(matchUpResults)){
    lineVals <- unlist(strsplit(gsub(': ', '!', gsub(':: ', '!', gsub('-', '!', gsub('Tile ', '', matchUpResults[i]), fixed = T), fixed = T), fixed = T), split = '!'))
    if(length(lineVals) == 4){
      if(lineVals[1] %in% gridNeighbors$tile){
        gridNeighbors[which(gridNeighbors$tile == lineVals[1]), lineVals[3]] <- lineVals[2]
      } else {
        gridNeighborsEntry <- data.frame(
          tile = NA, 
          N = NA,
          E = NA,
          S = NA,
          W = NA
        )
        gridNeighborsEntry['tile'] <- lineVals[1]
        gridNeighborsEntry[lineVals[3]] <- lineVals[2]
        gridNeighbors <- rbind(gridNeighbors, gridNeighborsEntry)
      }
    }
  }
  return(matchUpResults)
}

nwCorner <- gridNeighbors[which(is.na(gridNeighbors$N) & is.na(gridNeighbors$W)), 'tile']


