import numpy as np

with open("input.dat", "r") as fp:
    lines = fp.readlines()

heightvals = []
for line in lines:
    if line.strip():
        heightvals.append([ord(c) - ord('a') for c in line.strip()])

heights = np.array(heightvals, dtype=int)
startpos = np.argwhere(heights == ord('S') - ord('a'))[0]
heights[startpos[0], startpos[1]] = 0
endpos = np.argwhere(heights == ord('E') - ord('a'))[0]
heights[endpos[0], endpos[1]] = np.max(heights) + 1

pathdistances = []
for mystartpos in np.argwhere(heights == 0):
    distances = np.ones(heights.shape, dtype=int) * -1
    front = [[mystartpos[0], mystartpos[1]]]
    distances[mystartpos[0], mystartpos[1]] = 0

    found = False
    istep = 0
    while True:
        istep += 1
        newfront = []
        for frontelem in front:
            pos = np.array(frontelem)
            for step in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
                newpos = pos + step
                if np.any(newpos < [0, 0]) or np.any(newpos >= heights.shape):
                    continue
                if distances[newpos[0], newpos[1]] != -1:
                    continue
                if heights[newpos[0], newpos[1]] - heights[pos[0], pos[1]] > 1:
                    continue
                distances[newpos[0], newpos[1]] = istep
                found = np.all(newpos == endpos)
                if found:
                    founddist = istep
                    break
                newfront.append(newpos)
            if found:
                break
        if found or not newfront:
            break
        front = newfront
    if found:
        pathdistances.append(founddist)

mindist = min(*pathdistances)
print(f"Minimal distance found: {mindist}")
