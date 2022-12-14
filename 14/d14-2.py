import numpy as np

def print_field(occupied):
    "Helper function to print field if needed for debugging"
    print()
    for row in occupied:
        print("".join(["#" if occ else "." for occ in row]))

xstart = 500
ystart = 0

paths = []
with open("input.dat", "r") as fp:
    for line in fp:
        if not line.strip():
            break
        curpath = []
        words = line.split()
        for word in words[::2]:
            tokens = word.split(",")
            curpath.append((int(tokens[0]), int(tokens[1])))
        paths.append(np.array(curpath, dtype=int))

ymin = ystart
ymax = np.max([np.max(path, axis=0)[1] for path in paths])

height = ymax - ymin + 3
width = 2 * height + 1
shift = np.array((xstart - (width - 1) / 2, 0), dtype=int)

occupied = np.zeros((height, width), dtype=bool)
occupied[-1, :] = True

for path in paths:
    startpoint = path[0] - shift
    for point in path[1:]:
        endpoint = point - shift
        if startpoint[0] == endpoint[0]:
            start = min(startpoint[1], endpoint[1])
            end = max(startpoint[1], endpoint[1]) + 1
            occupied[start:end, startpoint[0]] = True
        else:
            start = min(startpoint[0], endpoint[0])
            end = max(startpoint[0], endpoint[0]) + 1
            occupied[startpoint[1], start:end] = True
        startpoint = endpoint

nunits = 0
while True:
    xx, yy = np.array((xstart, ystart)) - shift
    nunits += 1
    while yy < height - 1:
        if not occupied[yy + 1, xx]:
            newy, newx = yy + 1, xx
        elif not occupied[yy + 1, xx - 1]:
            newy, newx = yy + 1, xx - 1
        elif not occupied[yy + 1, xx + 1]:
            newy, newx = yy + 1, xx + 1
        else:
            break
        occupied[yy, xx] = False
        yy, xx = newy, newx
        occupied[yy, xx] = True
    if yy == ystart:
        break

print(f"Units needed to stop flow: {nunits}")
