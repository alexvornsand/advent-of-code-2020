# advent of code
# day 14

# part 1
program <- readLines('day-14.txt')

binToDec <- function(v){
  return(sum(v * 2 ^ ((length(v) - 1):0)))
}

evaluateLine <- function(line, mem = mem){
  instrType <- unlist(strsplit(line, split = ' = '))[1]
  if(instrType == 'mask'){
    mask <<- unlist(strsplit(unlist(strsplit(line, split = ' = '))[2], split = ''))
  } else {
    n <- as.integer(gsub('mem[', '', gsub(']', '', unlist(strsplit(line, split = ' = '))[1], fixed = T), fixed = T))
    val <- as.integer(unlist(strsplit(line , split = ' = '))[2])
    binSepRev <- as.integer(intToBits(val))
    maskSepRev <- unlist(strsplit(rev(mask), split = ''))
    binSepRev[which(maskSepRev == '0')] <- 0
    binSepRev[which(maskSepRev == '1')] <- 1
    binSepRev[which(is.na(binSepRev))] <- 0
    mem[[as.character(n)]] <<- binToDec(rev(binSepRev))
  }
}

mem <- list()
for(line in program){
  evaluateLine(line)
}

as.character(sum(unlist(mem), na.rm = T))

# part 2
generateAddresses <- function(binAddress, mask){
  binAddressRev <- rev(binAddress)
  maskRev <- rev(mask)
  binAddressRev[which(maskRev == '1')] <- '1'
  binAddressRev[which(maskRev == 'X')] <- 'X'
  binAddressRev[which(is.na(binAddressRev))] <- '0'
  binAddressNew <- rev(binAddressRev)
  ndigits <- length(which(binAddressNew == 'X'))
  xDigits <- t(sapply(0:(2 ^ (ndigits) - 1), function(x){rev(as.integer(intToBits(x))[1:ndigits])}))
  replaceDigits <- function(digits, address){
    add <- address
    add[which(add == 'X')] <- digits
    return(add)
  }
  addresses <- t(apply(xDigits, 1, replaceDigits, address = binAddressNew))
  return(addresses)
}

writeToAddress <- function(address, val){
  decAddress <- binToDec(as.integer(address))
  mem[[as.character(decAddress)]] <<- val
}

evaluateLine <- function(line, mem = mem){
  instrType <- unlist(strsplit(line, split = ' = '))[1]
  if(instrType == 'mask'){
    mask <<- unlist(strsplit(unlist(strsplit(line, split = ' = '))[2], split = ''))
  } else {
    n <- as.integer(gsub('mem[', '', gsub(']', '', unlist(strsplit(line, split = ' = '))[1], fixed = T), fixed = T))
    val <- as.integer(unlist(strsplit(line , split = ' = '))[2])
    binAddress <- rev(as.integer(intToBits(n)))
    addresses <- generateAddresses(binAddress, mask)
    apply(addresses, 1, writeToAddress, val = val)
  }
}

mem <- list()
for(line in program){
  evaluateLine(line)
}

as.character(sum(unlist(mem), na.rm = T))



