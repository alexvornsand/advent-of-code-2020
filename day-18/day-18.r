# advent of code
# day 18

# part 1
problems <- readLines('day-18.txt')

evaluateProblem <- function(prob){
  if('(' %in% unlist(strsplit(prob, split = ''))){
    start <- regexpr(pattern = '\\((\\d*\\s(\\*|\\+)\\s)*\\d*)', prob)[1]
    end <- start + attr(regexpr(pattern = '\\((\\d*\\s(\\*|\\+)\\s)*\\d*)', prob), "match.length") - 1
    interiorSection <- paste(unlist(strsplit(prob, split = ''))[(start + 1):(end - 1)], collapse = '')
    interiorSolution <- evaluateProblem(interiorSection)
    prob <- gsub(paste('(', interiorSection, ')', sep = ''), interiorSolution, prob, fixed = T)
    return(evaluateProblem(prob))
  } else {
    exp <- unlist(strsplit(prob, split = ' '))
    val <- 0
    for(i in 1:length(exp)){
      if(i == 1){
        val <- val + as.integer(exp[i])
      } else if(exp[i] == '*'){
        val <- val * as.integer(exp[i + 1])
      } else if(exp[i] == '+'){
        val <- val + as.integer(exp[i + 1])
      }
    }
    return(val)
  }
}

as.character(sum(unlist(sapply(problems, evaluateProblem))))

# part 2
evaluateProblem <- function(prob){
  if('(' %in% unlist(strsplit(prob, split = ''))){
    start <- regexpr(pattern = '\\((\\d*\\s(\\*|\\+)\\s)*\\d*)', prob)[1]
    end <- start + attr(regexpr(pattern = '\\((\\d*\\s(\\*|\\+)\\s)*\\d*)', prob), "match.length") - 1
    interiorSection <- paste(unlist(strsplit(prob, split = ''))[(start + 1):(end - 1)], collapse = '')
    interiorSolution <- evaluateProblem(interiorSection)
    prob <- gsub(paste('(', interiorSection, ')', sep = ''), interiorSolution, prob, fixed = T)
    return(evaluateProblem(prob))
  } else if('+' %in% unlist(strsplit(prob, split = ''))){
    start <- regexpr(pattern = '\\d*\\s\\+\\s\\d*', prob)[1]
    end <- start + attr(regexpr(pattern = '\\d*\\s\\+\\s\\d*', prob), "match.length") - 1
    interiorSection <- paste(unlist(strsplit(prob, split = ''))[(start):(end)], collapse = '')
    interiorSolution <- eval(parse(text = interiorSection))
    prob <- gsub(interiorSection, interiorSolution, prob, fixed = T)
    return(evaluateProblem(prob))
  } else {
    return(eval(parse(text = prob)))
  }
}

as.character(sum(sapply(problems, evaluateProblem)))