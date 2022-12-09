import numpy as np

nknots = 2

shifts = {
    "U": np.array([1, 0]),
    "D": np.array([-1, 0]),
    "R": np.array([0, 1]),
    "L": np.array([0, -1])
}

lines = open("input.dat", "r").readlines()

knotpos = np.zeros((nknots, 2), dtype=int)
visitedpos = set([tuple(knotpos[-1])])

for line in lines:
    words = line.split()
    direction, nsteps = words[0], int(words[1])
    for _ in range(nsteps):
        knotpos[0] += shifts[direction]
        for iknot in range(1, len(knotpos)):
            posdiff = knotpos[iknot - 1] - knotpos[iknot]
            if np.any(np.abs(posdiff) == 2):
                knotpos[iknot] += np.sign(posdiff)
        visitedpos.add(tuple(knotpos[-1]))

print(f"Nr. of visited tail positions: {len(visitedpos)}")
