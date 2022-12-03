#!/usr/bin/env python
import numpy as np

total_priority = 0
fp = open("input.dat", "r")
for line in fp:
    charcodes = np.array([ord(c) for c in line.rstrip()])
    priorities = np.where(
        charcodes >= ord("a"), charcodes - ord("a"), charcodes - ord("A") + 26)
    found1 = np.zeros(52, dtype=bool)
    found1[priorities[: len(priorities) // 2]] = True
    found2 = np.zeros(52, dtype=bool)
    found2[priorities[len(priorities) // 2 :]] = True
    total_priority = total_priority + np.argwhere(np.logical_and(found1, found2))[0, 0] + 1

print("Total priority of double items: ", total_priority)
