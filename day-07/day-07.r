# advent of code
# day 7

# part 1
bagRules <- readLines('day-7.txt')

ruleList <- list()

for(rule in bagRules){
  keyValue <- trimws(unlist(strsplit(gsub('bag', '', gsub('bags', 'bag', rule)), split = ' contain ')))
  bags <- trimws(gsub('.', '', unlist(strsplit(keyValue[2], split = ',')), fixed = T))
  if(bags[1] == 'no other'){
    ruleList[[keyValue[1]]] <- 'no other'
  } else {
    ruleList[[keyValue[1]]] <- list()
    for(bag in bags){
      bagCount <- as.integer(substring(bag, 1, 1))
      bagId <- trimws(substring(bag, 3, nchar(bag)))
      ruleList[[keyValue[1]]][[bagId]] <- bagCount
    }
  }
}

checkForShinyGold <- function(bag){
  if(any(names(ruleList[[bag]]) == 'shiny gold')){
    return(T)
  } else if(ruleList[[bag]][1] == 'no other'){
    return(F)
  } else {
    return(any(unlist(sapply(names(ruleList[[bag]]), checkForShinyGold))))
  }
}

sum(unlist(sapply(names(ruleList), checkForShinyGold)))

# part 2
countBags <- function(bag){
  if(ruleList[bag] == 'no other'){
    return(0)
  } else {
    return(sum(unlist(sapply(names(ruleList[[bag]]), function(x){return(ruleList[[bag]][[x]] * (1 + countBags(x)))}))))
  }
}

countBags('shiny gold')
