# advent of code
# day 15

# part 1 & 2
import sys
import time
start_time = time.time()

input = [9, 12, 1, 4, 17, 0, 18]

def play(end):
    lastPlayed = {}
    for i in range(len(input) - 1):
        lastPlayed[input[i]] = i
    next = input[-1]
    for i in range(len(input), end + 1):
        last = next
        if(last in lastPlayed.keys()):
            next = i - 1 - lastPlayed[last]
            lastPlayed[last] = i - 1
        else:
            lastPlayed[last] = i - 1
            next = 0
    print(last)

if(len(sys.argv) == 2):
    play(int(sys.argv[1]))
else:
    play(2020)

print("--- %s seconds ---" % (time.time() - start_time))
