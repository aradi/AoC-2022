import re

INPUT_REGEXP = re.compile(r"""
^Monkey\ (?P<id>\d+):\n
\s*Starting\ items:\ (?P<itemlist>.*)\n
\s*Operation:\ new\ =\ old\ (?P<op>[+*])\ (?P<val>\d+|\w+)\n
\s*Test:\ divisible\ by\ (?P<divisor>\d+)\n
\s*If\ true:\ throw\ to\ monkey\ (?P<truetarget>\d+)\n
\s*If\ false:\ throw\ to\ monkey\ (?P<falsetarget>\d+)\n
""", re.VERBOSE | re.MULTILINE)

operations = {
    "*": lambda x, y: x * y,
    "+": lambda x, y: x + y
}

txt = open("input.dat", "r").read()
monkeydata = {
    int(match.group("id")): {
        "items": [int(ss) for ss in match.group("itemlist").split(",")],
        "op": match.group("op"),
        "opval": match.group("val"),
        "divisor": int(match.group("divisor")),
        "truetarget": int(match.group("truetarget")),
        "falsetarget": int(match.group("falsetarget")),
        "ninspections": 0,
    }
    for match in INPUT_REGEXP.finditer(txt)
}

for _ in range(20):
    for imonkey in range(len(monkeydata)):
        mydata = monkeydata[imonkey]
        for item in mydata["items"]:
            mydata["ninspections"] += 1
            worrylevel = item
            opval = worrylevel if mydata["opval"] == "old" else int(mydata["opval"])
            worrylevel = operations[mydata["op"]](worrylevel, opval)
            worrylevel //= 3
            testcond = worrylevel % mydata["divisor"] == 0
            target = mydata["truetarget"] if testcond else mydata["falsetarget"]
            monkeydata[target]["items"].append(worrylevel)
        mydata["items"] = []

inspections = [mydata["ninspections"] for mydata in monkeydata.values()]
inspections.sort()
print(f"Monkey business level: {inspections[-2] * inspections[-1]}")
