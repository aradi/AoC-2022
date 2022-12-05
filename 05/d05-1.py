with open("input.dat", "r") as fp:
    lines = fp.readlines()

stacklines = []
nstacks = 0
for line in lines:
    if line.lstrip()[0] == '[':
        stacklines.append(line)
        # Note: line contains trailing '\n' character
        nstacks = max(nstacks, (len(line) // 4))
    else:
        break
stacklines.reverse()

stacks = [[] for _ in range(nstacks)]
for line in stacklines:
    for istack, char in enumerate(line[1::4]):
        if char != " ":
            stacks[istack].append(char)

operations = []
for line in lines[len(stacklines) + 2 :]:
    if not line.strip():
        break
    words = line.split()
    operations.append((int(words[1]), int(words[3]), int(words[5])))

for amount, source, target in operations:
    stacks[target - 1].extend(stacks[source - 1][-amount:][::-1])
    del stacks[source - 1][-amount:]

topcrates = [stack[-1] for stack in stacks]
print("Top crates: ", "".join(topcrates))
