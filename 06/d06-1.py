marker_length = 4

with open("input.dat", "r") as fd:
    line = fd.readline()

for ipos in range(4, len(line)):
    if len(frozenset(line[ipos - marker_length : ipos])) == marker_length:
        break

print(f"Position of the end of the marker: {ipos}")
