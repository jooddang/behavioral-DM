#!/usr/bin/env python

blocks = range(0,10)

data = {} # block -> list of error values at each iteration

for b in blocks:
    data[b] = []
    f = open("final_data/error.%d" % b)
    for line in f:
        fields = line.translate(None, "[]").split()
        iteration = int(fields[3])
        error = float(fields[5])
        if iteration < 300:
            data[b].append(error)
            #print "%s %s" % (len(data[b]), iteration)
            assert(len(data[b]) == iteration+1)
    f.close()

f = open("final_data/errorVSiteration2.csv", "w")
for i in range(0,300):
    vals = []
    for b in blocks:
        vals.append(str(data[b][i]))
    f.write(",".join(vals) + "\n")
f.close()
