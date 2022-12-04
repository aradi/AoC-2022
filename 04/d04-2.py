import re
import numpy as np

INTERVAL_REGEXP = re.compile(r"(\d+)-(\d+),(\d+)-(\d+)", re.MULTILINE)

with open("input.dat", "r") as fp:
    lines = fp.readlines()

overlaps = 0
for line in lines:
    match = INTERVAL_REGEXP.match(line)
    intervalbounds = np.reshape(np.array(match.groups(), dtype=int), [2, 2])
    # Whether the lower bound of the first interval is within the bounds of the second interval,
    # or the lower bound of the second interval is within the bounds of the first interval
    partially_contained = (
        np.product(intervalbounds[1, :] - intervalbounds[0, 0]) <= 0
        or  np.product(intervalbounds[0, :] - intervalbounds[1, 0]) <= 0
    )
    if partially_contained:
        overlaps += 1

print(f"Number of pairs with at least partial overlap: ", overlaps)
