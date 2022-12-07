limit = 100000
with open("input.dat", "r") as fp:
    lines = fp.readlines()
paths = [""]
sizes = {}
for line in lines:
    if not line.strip():
        break
    if line.startswith("$ cd "):
        newpath = line[5:-1]
        if newpath == "/":
            paths = [paths[0]]
        elif newpath == "..":
            paths.pop(-1)
        else:
            paths.append(newpath)
    elif not line.startswith("dir") and not line.startswith("$ ls"):
        word0, word1 = line.split(maxsplit=2)
        name, size = word1, int(word0)
        for ipath in range(len(paths)):
            path = "/".join(paths[0 : ipath + 1])
            sizes[path] = sizes.get(path, 0) + size

summed_sizes = sum([value for value in sizes.values() if value <= limit])
print(f"Summed directory sizes: {summed_sizes}")
