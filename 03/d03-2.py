#!/usr/bin/env python
import numpy as np

with open("input.dat", "r") as fp:
    lines = fp.readlines()
total_priority = 0
for iline in range(0, len(lines), 3):
    triple_candidate = np.ones(52, dtype=bool)
    for ishift in range(3):
        charcodes = np.array([ord(c) for c in lines[iline + ishift].rstrip()])
        priorities = np.where(
            charcodes >= ord("a"), charcodes - ord("a"), charcodes - ord("A") + 26)
        found = np.zeros(52, dtype=bool)
        found[priorities] = True
        triple_candidate = np.logical_and(triple_candidate, found)
    total_priority = total_priority + np.argwhere(triple_candidate)[0, 0] + 1

print("Sum of group triple priorities: ", total_priority)
