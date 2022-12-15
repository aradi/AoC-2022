import re

ROWS_TO_CHECK = (0, 4000000 + 1)
COLS_TO_CHECK = (0, 4000000 + 1)

INPUT_REGEXP = re.compile(
    r".*?(-?\d+),.*?(-?\d+).*?(-?\d+).*?(-?\d+)", re.MULTILINE)

inp = [tuple(int(s) for s in INPUT_REGEXP.match(line).groups())
       for line in open("input.dat") if line.strip()]

tuningfreq = 0
for yy in range(*ROWS_TO_CHECK):
    excluded = []
    for sensorx, sensory, beaconx, beacony in inp:
        dist = abs(beaconx - sensorx) + abs(beacony - sensory)
        if sensory - dist > yy or sensory + dist < yy:
            continue
        rowdist = dist - abs(yy - sensory)
        start = max(sensorx - rowdist, COLS_TO_CHECK[0])
        end = min(sensorx + rowdist + 1, COLS_TO_CHECK[1])
        excluded.append((start, end))

    excluded.sort()
    end = excluded[0][0]
    nonexclx = COLS_TO_CHECK[0] - 1
    for newstart, newend in excluded:
        if newend > end:
            if newstart > end:
                nonexclx = newstart - 1
                break
            start = max(end, newstart)
            end = newend

    if nonexclx >= COLS_TO_CHECK[0]:
        tuningfreq = nonexclx * 4000000 + yy
        break

print(f"Tuning frequency of the distress bacon: {tuningfreq}")
