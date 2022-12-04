import re
import numpy as np

INTERVAL_REGEXP = re.compile(r"(\d+)-(\d+),(\d+)-(\d+)", re.MULTILINE)

with open("input.dat", "r") as fp:
    lines = fp.readlines()

overlaps = 0
for line in lines:
    match = INTERVAL_REGEXP.match(line)
    intervalbounds = np.reshape(np.array(match.groups(), dtype=int), [2, 2])
    fully_contained = (
        np.product(intervalbounds[0, :] - intervalbounds[1, :]) <= 0
    )
    if fully_contained:
        overlaps += 1

print(f"Number of pairs with full overlap: ", overlaps)
