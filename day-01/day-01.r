# advent of code
# day 1

# part 1
numbers <- scan('day-1.txt')

for(i in 1:length(numbers)){
  for(j in i:length(numbers)){
    if(numbers[i] + numbers[j] == 2020){
      print(numbers[i] * numbers[j])
    }
  }
}

# part 2

for(i in 1:length(numbers)){
  for(j in i:length(numbers)){
    for(k in j:length(numbers)){
      if(numbers[i] + numbers[j] + numbers[k] == 2020){
        print(numbers[i] * numbers[j] * numbers[k])
      }
    }
  }
}
