import re
import numpy as np

TOTAL_TIME = 30

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
    if curdist == TOTAL_TIME:
        return
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
    score = get_optimal_path_helper(inddata, startnode, valveclosed, dists, TOTAL_TIME, 0)
    return score


def get_optimal_path_helper(inddata, curnode, valveclosed, dists, time, score):
    isclosed = valveclosed[curnode]
    if isclosed:
        valveclosed[curnode] = False
        time -= 1
        score += time * inddata["rates"][curnode]
    bestscore = score
    for nextnode in np.nonzero(valveclosed)[0]:
        reachedat = time - dists[curnode, nextnode]
        if reachedat > 1:
            newscore = get_optimal_path_helper(
                inddata, nextnode, valveclosed, dists, reachedat, score)
            bestscore = max(bestscore, newscore)
    valveclosed[curnode] = isclosed
    return bestscore


def main():
    inddata = read_valve_data("input.dat")
    score = get_optimal_path(inddata, inddata["valve2ind"]["AA"])
    print(f"Score of best path: {score}")


if __name__ == "__main__":
    main()