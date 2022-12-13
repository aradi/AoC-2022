import json

def compare(lhs, rhs):
    """Compares two lists"""
    if isinstance(lhs, int) and isinstance(rhs, int):
        diff = lhs - rhs
        return diff // abs(diff) if diff else 0
    if isinstance(lhs, list) and isinstance(rhs, int):
        res = compare(lhs, [rhs])
        return res
    if isinstance(lhs, int) and isinstance(rhs, list):
        res = compare([lhs], rhs)
        return res
    for ll, rr in zip(lhs, rhs):
        comp = compare(ll, rr)
        if comp:
            return comp
    diff = len(lhs) - len(rhs)
    return diff // abs(diff) if diff else 0

ind = 0
indsum = 0
fp = open("input.dat", "r")
while True:
    line1 = fp.readline().strip()
    line2 = fp.readline().strip()
    fp.readline()
    if not line1 or not line2:
        break
    ind += 1
    lhs = json.loads(line1)
    rhs = json.loads(line2)
    if compare(lhs, rhs) != 1:
        indsum += ind
fp.close()

print(f"Index sum of packages in right order: {indsum}")
