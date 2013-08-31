#!/usr/bin/env python

# Creates the average ROC plot and computes AUC
import sys

def file_to_val_dict(block):
    fprs = []
    tprs = []
    f = open("roc/roc.%d.plot" % block)
    for line in f:
        fields = line.split()
        tpr, fpr = float(fields[2]), float(fields[4])
        fprs.append(fpr)
        tprs.append(tpr)
    f.close()
    return (fprs, tprs)

def generate_hist(fprs, tprs, step):
    hist_fprs = []
    hist_tprs = []

    # sort by fprs, low to high
    (s_fprs, s_tprs) = [list(x) for x in zip(*sorted(zip(fprs, tprs), key=lambda pair: pair[0]))]

    print "Len fprs: %d" % len(s_fprs)

    total_seen = 0
    step_count = 0
    step_sum = 0.0
    tic = 0.0
    last_val = 0.0
    for i in range(0, len(s_fprs)):
        fpr = s_fprs[i]
        tpr = s_tprs[i]
        if fpr >= tic and fpr < (tic + step):
            step_sum += tpr
            step_count += 1
            total_seen += 1
        elif fpr >= (tic + step):
            # record
            avg = step_sum / step_count
            hist_fprs.append(tic)
            hist_tprs.append(avg)

            tic += step  # jump to next value
            while (fpr >= tic + step):
                # until we find the bin again, keep setting it to our last avg
                hist_fprs.append(tic)
                hist_tprs.append(avg)
                tic += step  # jump to next value

            # we're in the right bin, so reset
            step_count = 1
            total_seen += 1
            step_sum = tpr

        else:
            print fpr, tic, step
            assert False # should never get here if sorted...
    while (tic <= 1.0+step):
        hist_fprs.append(tic)
        hist_tprs.append(avg)
        tic += step  # jump to next value

    assert len(hist_fprs) == len(hist_tprs)
    print total_seen

    return hist_fprs, hist_tprs

def main(step = 0.001):
    blocks = range(0,9)
    all_fprs = []
    all_tprs = []
    for b in blocks:
        block_fprs, block_tprs = file_to_val_dict(b)
        all_fprs += block_fprs
        all_tprs += block_tprs
    final_fprs, final_tprs = generate_hist(all_fprs, all_tprs, step)
    for idx in range(0, len(final_fprs)):
        print final_fprs[idx], final_tprs[idx]

    auc = sum(final_tprs) / len(final_tprs)
    print "AUC is: %f" % auc

if __name__ == "__main__":
    main(float(sys.argv[1]))
