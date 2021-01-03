# advent of code
# day 23

# part 1
input <- '135468729'

predictCups <- function(input, k = 100, l = 9){
  sequence <- 1:l
  header <- as.integer(unlist(strsplit(input, '')))
  sequence[1:length(header)] <- header
  max <- max(sequence)
  for(i in 1:k){
    if(i %% 1000 == 0){print(i)}
    current <- sequence[1]
    removed <- sequence[2:4]
    if(current == 1){
      target <- max
    } else {
      target <- current - 1
    }
    while(target %in% removed){
      if(target == 1){
        target <- max
      } else {
        target <- target - 1
      }
    }
    if(which(sequence == target) < length(sequence)){
      post <- sequence[(which(sequence == target) + 1):length(sequence)]
    } else {
      post <- integer()
    }
    sequence <- c(sequence[5:which(sequence == target)], removed, post, current)
  }
  if(k == 100 & l == 9){
    if(which(sequence == 1) == 1){
      post <- integer()
    } else {
      post <- sequence[1:(which(sequence == 1) - 1)]
    }
    if(which(sequence == 1) == length(sequence)){
      pre <- integer()
    } else {
      pre <- sequence[(which(sequence == 1) + 1):length(sequence)]
    }
    return(paste(c(pre, post), collapse = ''))
  } else {
    if(which(sequence == 1) == 1){
      post <- integer()
    } else {
      post <- sequence[1:(which(sequence == 1) - 1)]
    }
    if(which(sequence == 1) == length(sequence)){
      pre <- integer()
    } else {
      pre <- sequence[(which(sequence == 1) + 1):length(sequence)]
    }
    sequence <- c(pre, post)
    return(as.character(prod(sequence[1:2])))
  }
}

predictCups(input)

# part 2

predictCups(input, k = 10000000, l = 1000000)
