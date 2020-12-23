# advent of code
# day 6

# part 1
declarations <- strsplit(gsub('\n', ' ', unlist(strsplit(paste(readLines('day-6.txt'), collapse = '\n'), split = '\n\n'))), split = ' ')

countDeclarations <- function(dec){
  return(length(unique(unlist(strsplit(paste(dec, collapse = ''), split = '')))))
}

sum(unlist(sapply(declarations, countDeclarations)))

# part 2
countDeclarations <- function(dec){
  return(length(names(which(table(unlist(strsplit(paste(dec, collapse = ''), split = ''))) == length(dec)))))
}

sum(unlist(sapply(declarations, countDeclarations)))
