xx = 1
totalcycles = 0
strengthsum = 0
fp = open("input.dat", "r")
for line in fp:
    words = line.split()
    command = words[0]
    if command == "addx":
        value = int(words[1])
        commandcycles = 2
    else:
        commandcycles = 1
        value = 0
    for icc in range(commandcycles):
        totalcycles += 1
        if totalcycles  % 40 == 20:
            strengthsum += totalcycles * xx
    xx += value
fp.close()

print(f"Sum of signal strengths: {strengthsum}")