# advent of code
# day 12

# part 1
directions <- readLines('day-12.txt')


posx <- 0
posy <- 0
r <- 0
for(direction in directions){
  action <- unlist(strsplit(direction, split = ''))[1]
  magnitude <- as.integer(paste(unlist(strsplit(direction, split = ''))[2:nchar(direction)], collapse = ''))
  if(action == 'N'){
    posy <- posy + magnitude
  } else if(action == 'S'){
    posy <- posy - magnitude
  } else if(action == 'E'){
    posx <- posx + magnitude
  } else if(action == 'W'){
    posx <- posx - magnitude
  } else if(action == 'L'){
    r <- (r + magnitude) %% 360
  } else if(action == 'R'){
    r <- (r - magnitude) %% 360
  } else {
    posx <- posx + cos(2 * pi * r / 360) * magnitude
    posy <- posy + sin(2 * pi * r / 360) * magnitude
  }
}

abs(posx) + abs(posy)

# part 2

xtar <- 10
ytar <- 1
xpos <- 0
ypos <- 0
for(direction in directions){
  action <- unlist(strsplit(direction, split = ''))[1]
  magnitude <- as.integer(paste(unlist(strsplit(direction, split = ''))[2:nchar(direction)], collapse = ''))
  if(action == 'N'){
    ytar <- ytar + magnitude
  } else if(action == 'S'){
    ytar <- ytar - magnitude
  } else if(action == 'E'){
    xtar <- xtar + magnitude
  } else if(action == 'W'){
    xtar <- xtar - magnitude
  } else if(action == 'L'){
    if(magnitude == 180){
      xtar <- -xtar
      ytar <- -ytar
    } else if(magnitude == 90){
      oldX <- xtar
      oldY <- ytar
      xtar <- -oldY
      ytar <- oldX
    } else {
      oldX <- xtar
      oldY <- ytar
      xtar <- oldY
      ytar <- -oldX
    }
  } else if(action == 'R'){
    if(magnitude == 180){
      xtar <- -xtar
      ytar <- -ytar
    } else if(magnitude == 90){
      oldX <- xtar
      oldY <- ytar
      xtar <- oldY
      ytar <- -oldX
    } else {
      oldX <- xtar
      oldY <- ytar
      xtar <- -oldY
      ytar <- oldX
    }
  } else {
    xpos <- xpos + xtar * magnitude
    ypos <- ypos + ytar * magnitude
  }
  print(paste('move:', direction, 'xpos:', xpos, 'ypos:', ypos, 'xtar:', xtar, 'ytar:', ytar))
}

abs(xpos) + abs(ypos)
