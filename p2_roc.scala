import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.math
import scala.io.Source
import scala.util.Random
import java.io.File
import java.io.IOException
import java.io.FileInputStream
import java.io.DataInputStream
import java.io.BufferedInputStream

// Steps
// 1. Load the Beta, X, and Y matrices
// 2. Calculate BX.
// 3. Make Y binary: if Yn == 5, it's positive, otherwise negative.
// 4. For threshold X from 0.1 to 5.0 in increments of 0.1,
//      5. Discretize BX.
//      6. Compare BX to Y:
//          7. Multipy Y by 2.0, then subtract Y - BX
//          8. False positive is 0.0 + 1.0 = 1
//          9. False negative is 2.0 + 0.0 = 2
//          10. True positive is 2.0 + 1.0 = 3
//          11. True negative is 0.0 + 0.0 = 0
//      12. Count up the number of false pos, neg, etc. Save to file.

def read_matrix(num: Int): (BIDMat.FMat, BIDMat.FMat) = {
    val beta = load("final_data/beta."+num+".200.mat", "b").asInstanceOf[FMat]
    val X = load("final_data/doc_matrix."+num+".mat", "d").asInstanceOf[SMat]
    val y = load("final_data/ratings."+num+".mat", "r").asInstanceOf[FMat]
    println("b.nrows="+beta.nrows+" b.ncols="+beta.ncols+" X.nrows="+X.nrows+" X.ncols="+X.ncols + " y.nrows="+y.nrows+ " y.ncols="+y.ncols)

    val betaX = beta.t * X
    return (betaX, y)
}

// create a matrix of elements either 0 or 1, based on a threshold
def discretize(matrix: BIDMat.FMat, threshold: Float): BIDMat.IMat = {
    val res = matrix + 0f
    return IMat(res.ffMatOpScalar(threshold, (orig,_) => {if (orig >= threshold) { 1 } else { 0 }}, res))
}

// return the number of elements in matrix that have the given value
def sumifeq(matrix: BIDMat.IMat, target: Int): Int = {
    val res = matrix + 0
    res.iiMatOpScalar(target, (orig,_) => {if (orig == target) { 1 } else {0}}, res)
    return sum(res)(0,0).toInt
}

def make_roc(block: Int) = {
    val output_file = new java.io.FileWriter("roc."+block+".plot", true)

    val (bX, y) = read_matrix(block)
    val discrete_ratings = 2 *@ discretize(y, 5.0f)

    val (s, d) = sortrows(bX.t)
    val sorted = s.t
    //println(sorted.nrows + " " + sorted.ncols)
    for (i <- (0 to sorted.ncols-1)) {
        val col = sorted(?,i)
        val score = col(0)
        val predictions = discrete_ratings + discretize(bX, score).t
        val tn = sumifeq(predictions, 0)
        val fp = sumifeq(predictions, 1)
        val fn = sumifeq(predictions, 2)
        val tp = sumifeq(predictions, 3)
        val tpr = tp.toFloat / (tp + fn)
        val fpr = fp.toFloat / (fp + tn)
        //val msg = score + " TN "+tn+" FP "+fp + " FN "+fn+ " TP "+tp+"\n"
        val msg = "TPR " + tpr + " FPR " + fpr + "\n"
        output_file.write(msg)
    }
    output_file.close()
}

def main() {
    for (block <- (0 to 9).par) {
        make_roc(block)
    }
}
