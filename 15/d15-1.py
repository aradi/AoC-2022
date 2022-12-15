import re

ROW_TO_CHECK = 2000000

INPUT_REGEXP = re.compile(
    r".*?(-?\d+),.*?(-?\d+).*?(-?\d+).*?(-?\d+)", re.MULTILINE)

inp = [tuple(int(s) for s in INPUT_REGEXP.match(line).groups())
       for line in open("input.dat") if line.strip()]

excluded = []
for sensorx, sensory, beaconx, beacony in inp:
    dist = abs(beaconx - sensorx) + abs(beacony - sensory)
    if sensory - dist > ROW_TO_CHECK or sensory + dist < ROW_TO_CHECK:
        continue
    rowdist = dist - abs(ROW_TO_CHECK - sensory)
    start = sensorx - rowdist
    end = sensorx + rowdist + 1
    if ROW_TO_CHECK == beacony:
        start = start + 1 if beaconx < sensorx else start
        end = end - 1 if beaconx > sensorx else end
    excluded.append((start, end))

excluded.sort()
end = excluded[0][0]
nexcluded = 0
for newstart, newend in excluded:
    if newend > end:
        start = max(end, newstart)
        end = newend
        nexcluded += end - start

print(f"Nr. of excluded positions in row {ROW_TO_CHECK:d}: {nexcluded}")
