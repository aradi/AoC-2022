import numpy as np

curcal = 0
cals = []
with open("input.dat", "r") as fp:
    lines = [line.strip() for line in fp]
for line in lines:
    if line:
        curcal = curcal + int(line)
    else:
        cals.append(curcal)
        curcal = 0

imaxcal = np.argmax(cals, axis=0)
maxcal = cals[imaxcal]
print(f"Elf with most calories: {imaxcal + 1:d}, calories: {maxcal:d}")
