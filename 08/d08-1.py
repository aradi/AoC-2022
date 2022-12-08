import numpy as np

with open("input.dat", "r") as fp:
    heights = np.array(
        [[int(c) for c in line.strip()] for line in fp], dtype=int)

nn = heights.shape[0]
score = np.zeros((nn, nn), dtype=int)

visible = np.ones((nn, nn), dtype=bool)
for irow in range(1, score.shape[0] - 1):
    for icol in range(1, score.shape[1] - 1):
        visible[irow, icol] = (
            np.all(heights[irow, icol] > heights[irow, : icol])
            or np.all(heights[irow, icol] > heights[irow, icol + 1 :])
            or np.all(heights[irow, icol] > heights[: irow, icol])
            or np.all(heights[irow, icol] > heights[irow + 1 :, icol])
        )

print(f"Nr. of visible trees: {np.count_nonzero(visible)}")
