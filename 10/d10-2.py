xx = 1
totalcycles = 0
fp = open("input.dat", "r")
for line in fp:
    words = line.split()
    command = words[0]
    if command == "addx":
        xincr = int(words[1])
        commandcycles = 2
    else:
        commandcycles = 1
        xincr = 0
    for icc in range(commandcycles):
        totalcycles += 1
        row, col = divmod(totalcycles - 1, 40)
        pixel = "#" if abs(xx - col) <= 1 else "."
        endchar = "\n" if col == 39 else ""
        print(pixel, end=endchar)
    xx += xincr
fp.close()
