import numpy as np

with open("input.dat", "r") as fp:
    heights = np.array(
        [[int(c) for c in line.strip()] for line in fp], dtype=int)

nn = heights.shape[0]
score = np.zeros((nn, nn), dtype=int)

for irow in range(1, score.shape[0] - 1):
    for icol in range(1, score.shape[1] - 1):
        bleft = np.nonzero(heights[irow, : icol] >= heights[irow, icol])[0]
        bleft = bleft[-1] if len(bleft) else 0
        bright = np.nonzero(heights[irow, icol + 1 :] >= heights[irow, icol])[0]
        bright = bright[0] + icol + 1 if len(bright) else nn - 1
        bup = np.nonzero(heights[: irow, icol] >= heights[irow, icol])[0]
        bup = bup[-1] if len(bup) else 0
        bdown = np.nonzero(heights[irow + 1 :, icol] >= heights[irow, icol])[0]
        bdown = bdown[0] + irow + 1 if len(bdown) else nn - 1
        score[irow, icol] = (
            (icol - bleft) * (bright - icol) * (irow - bup) * (bdown - irow))

print(f"Maximal view score: {np.max(score)}")
