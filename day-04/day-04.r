# advent of code
# day 4

# part 1

passports <- gsub('\n', ' ', unlist(strsplit(paste(readLines('day-04.txt'), collapse = '\n'), split = '\n\n')))

evaluatePassport <- function(pp){
  kvs <- unlist(strsplit(pp, split = ' '))
  passportKeyValues <- list()
  for(kv in kvs){
    kvPair <- unlist(strsplit(kv, split = ':'))
    passportKeyValues[[kvPair[1]]] <- kvPair[2]
  }
  criticalFields <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')
  if(sum(criticalFields %in% names(passportKeyValues)) == 7){
    return(T)
  } else {
    return(F)
  }
}

sum(unlist(sapply(passports, evaluatePassport)))

# part 2

evaluatePassport <- function(pp){
  kvs <- unlist(strsplit(pp, split = ' '))
  passportKeyValues <<- list()
  for(kv in kvs){
    kvPair <- unlist(strsplit(kv, split = ':'))
    passportKeyValues[[kvPair[1]]] <<- kvPair[2]
  }
  criticalFields <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid')
  if(sum(criticalFields %in% names(passportKeyValues)) == 7){
    byr <- as.integer(passportKeyValues[['byr']]) >= 1920 & as.integer(passportKeyValues[['byr']]) <= 2002
    iyr <- as.integer(passportKeyValues[['iyr']]) >= 2010 & as.integer(passportKeyValues[['iyr']]) <= 2020
    eyr <- as.integer(passportKeyValues[['eyr']]) >= 2020 & as.integer(passportKeyValues[['eyr']]) <= 2030
    if(substr(passportKeyValues[['hgt']], nchar(passportKeyValues[['hgt']]) - 1, nchar(passportKeyValues[['hgt']])) == 'in'){
      hgt <- as.integer(substr(passportKeyValues[['hgt']], 1, nchar(passportKeyValues[['hgt']]) - 2)) >= 59 & as.integer(substr(passportKeyValues[['hgt']], 1, nchar(passportKeyValues[['hgt']]) - 2)) <= 76
    } else if(substr(passportKeyValues[['hgt']], nchar(passportKeyValues[['hgt']]) - 1, nchar(passportKeyValues[['hgt']])) == 'cm'){
      hgt <- as.integer(substr(passportKeyValues[['hgt']], 1, nchar(passportKeyValues[['hgt']]) - 2)) >= 150 & as.integer(substr(passportKeyValues[['hgt']], 1, nchar(passportKeyValues[['hgt']]) - 2)) <= 193
    } else {
      hgt <- F
    }
    hcl <- unlist(strsplit(passportKeyValues[['hcl']], split = ''))[1] == '#' & length(unlist(strsplit(passportKeyValues[['hcl']], split = ''))[2:7] %in% letters | !is.na(as.numeric(unlist(strsplit(passportKeyValues[['hcl']], split = ''))[2:7]))) == 6
    ecl <- passportKeyValues[['ecl']] %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
    pid <- nchar(passportKeyValues[['pid']]) == 9 & sum(!is.na(as.integer(unlist(strsplit(passportKeyValues[['pid']], split = ''))))) == 9
    return(byr & iyr & eyr & hgt & hcl & ecl & pid)
  } else {
    return(F)
  }
}

sum(unlist(sapply(passports, evaluatePassport)))

