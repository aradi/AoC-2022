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

packages = []
fp = open("input.dat", "r")
while True:
    line1 = fp.readline().strip()
    line2 = fp.readline().strip()
    fp.readline()
    if not line1 or not line2:
        break
    packages += [json.loads(line1), json.loads(line2)]
fp.close()

divpackages = [[[2]], [[6]]]
divpackinds = [1, 2]
for package in packages:
    for idivpack, divpack in enumerate(divpackages):
        if compare(package, divpack) == -1:
            divpackinds[idivpack] += 1

print(f"Encoder key for distress signal: {divpackinds[0] * divpackinds[1]}")
