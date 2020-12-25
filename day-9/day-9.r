# advent of code
# day 9

# part 1
numbers <- as.numeric(readLines('day-9.txt'))

findPair <- function(i, nums){
  nums <- nums[(i-25):i]
  for(j in 1:24){
    for(k in (j+1):25){
      if(nums[j] + nums[k] == nums[26]){
        return(T)
      }
    }
  }
  return(F)
}

targetNumber <- numbers[25 + min(which(unlist(sapply(26:length(numbers), findPair, nums = numbers)) == F))]

# part 2

findContiguousSum <- function(i, target = targetNumber){
  sum <- numbers[i]
  n <- i
  while(sum < target){
    n <- n + 1
    sum <- sum + numbers[n]
  }
  if(sum == target){
    key <- min(numbers[i:n]) + max(numbers[i:n])
    print(key)
  }
}

for(i in 1:560){
  findContiguousSum(i)
}
