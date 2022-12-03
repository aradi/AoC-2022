import numpy as np

with open("input.dat", "r") as fp:
    lines = [line.strip() for line in fp]

curcal = 0
cals = []
for line in lines:
    if line:
        curcal = curcal + int(line)
    else:
        cals.append(curcal)
        curcal = 0

tops = np.zeros(3, dtype=int)
for cal in cals:
    pos = np.argwhere(cal > tops)
    if len(pos):
        itop = pos[0, 0]
        tops[itop + 1 : ] = tops[itop : -1]
        tops[itop] = cal

print(f"Sum of top {len(tops):d} calories {np.sum(tops)}")
