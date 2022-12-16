# Note: this implementation is rather inefficient, it takes a few minutes
# to finish the calculation (ca. 8 min. on my 6 years old laptop)
#
# There are much more efficient Python implementations available, e.g.
# https://github.com/davearussell/advent2022/blob/master/day16/solve.py
#
import re
import numpy as np

TOTAL_TIME = 26

INPUT_PAT = re.compile(r"^Valve ([A-Z]+).*?(\d+).*?valves? (.*)$", re.MULTILINE)

def read_valve_data(fname):
    valvedata = {}
    for line in open(fname, "r"):
        match = INPUT_PAT.match(line)
        if not match:
            break
        valvedata[match.group(1)] = {
            "rate": int(match.group(2)),
            "neighs": tuple((s.strip() for s in match.group(3).split(",")))
        }
    ind2valve = [valve for valve in valvedata]
    valve2ind = {valve: ivalve for ivalve, valve in enumerate(ind2valve)}
    neighs = []
    rates = []
    for valve in ind2valve:
        vdata = valvedata[valve]
        rates.append(vdata["rate"])
        neighs.append(np.array([valve2ind[neigh] for neigh in vdata["neighs"]]))
    inddata = {
        "neighs": neighs,
        "rates": np.array(rates, dtype=int),
        "ind2valve": ind2valve,
        "valve2ind": valve2ind
    }
    return inddata


def get_min_dists(source, inddata):
    dists = np.ones((len(inddata["neighs"]),), dtype=int) * -1
    dists[source] = 0
    frontnodes = [source]
    get_min_dists_helper(inddata, 0, dists, frontnodes)
    return dists


def get_min_dists_helper(inddata, curdist, dists, frontnodes):
    curdist += 1
    #if curdist == TOTAL_TIME:
    #    return
    newfrontnodes = []
    neighs = inddata["neighs"]
    for frontnode in frontnodes:
        for neigh in neighs[frontnode]:
            if dists[neigh] == -1:
                dists[neigh] = curdist
                newfrontnodes.append(neigh)
    if newfrontnodes:
        get_min_dists_helper(inddata, curdist, dists, newfrontnodes)


def get_optimal_path(inddata, startnode):
    dists = np.array([get_min_dists(ii, inddata)
                      for ii in range(len(inddata["neighs"]))], dtype=int)
    valveclosed = np.zeros((len(inddata["rates"],)), dtype=bool)
    valveclosed[inddata["rates"] > 0] = True
    paths = get_optimal_path_helper(inddata, startnode, valveclosed, dists, TOTAL_TIME)
    return paths


def get_optimal_path_helper(inddata, curnode, valveclosed, dists, time):
    isclosed = valveclosed[curnode]
    if isclosed:
        valveclosed[curnode] = False
        time -= 1
    result = []
    for nextnode in np.nonzero(valveclosed)[0]:
        reachedat = time - dists[curnode, nextnode]
        if reachedat > 1:
            subpath = get_optimal_path_helper(
                inddata, nextnode, valveclosed, dists, reachedat)
            result += subpath
    valveclosed[curnode] = isclosed
    if result:
        result = [[(time, curnode)] + subpath for subpath in result]
    else:
        result = [[(time, curnode)]]
    return result


def get_score(inddata, path):
    score = 0
    rates = inddata["rates"]
    valveclosed = np.ones((len(rates),), dtype=bool)
    for time, node in path:
        if valveclosed[node]:
            score += time * rates[node]
        valveclosed[node] = False
    return score


def main():
    inddata = read_valve_data("input.dat")
    paths = get_optimal_path(inddata, inddata["valve2ind"]["AA"])
    rates = inddata["rates"]
    numpaths = np.zeros((len(paths), len(rates,)), dtype=int)
    for ipath, path in enumerate(paths):
        for time, node in path:
            numpaths[ipath, node] = time
    score = 0
    for ipath1 in range(len(numpaths)):
        if not ipath1 % 100:
            print(ipath1, len(numpaths))
        for ipath2 in range(ipath1 + 1, len(numpaths)):
            score = max(score, np.dot(np.maximum(numpaths[ipath1], numpaths[ipath2]), rates))
    print(score)



if __name__ == "__main__":
    main()
