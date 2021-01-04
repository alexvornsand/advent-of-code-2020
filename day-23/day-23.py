# advent of code
# day 23

# part 1
import numpy as np

input = '135468729'

def predictCups(input, partTwo = False):
    if partTwo == True:
        l = 1000000
        k = 10000000
    else:
        l = 9
        k = 100
    sequence = list(range(1, l + 1))
    sequence[:9] = [int(char) for char in input]
    max = l
    for i in range(k):
        if(i % 1000 == 0):
            print(i)
        current = sequence[0]
        removed = sequence[1:4]
        for p in range(3):
            sequence.pop(1)
        if(current == 1):
            target = max
        else:
            target = current - 1
        while(target in removed):
            if(target == 1):
                target = max
            else:
                target -= 1
        for x in removed[::-1]:
            if(sequence.index(target) == len(sequence) - 1):
                sequence.append(x)
            else:
                sequence.insert(sequence.index(target) + 1, x)
        sequence.pop(0)
        sequence.append(current)
    oneLoc = sequence.index(1)
    sequence = sequence[oneLoc:] + sequence[:oneLoc]
    sequence.pop(0)
    if partTwo == True:
        return(np.prod(sequence[:2]))
    else:
        strAnswer = ''.join([str(x) for x in sequence])
        return(strAnswer)

print(predictCups(input, partTwo = False))

# part 2
predictCups(input, partTwo = True)
