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
import scala.util.control.Breaks._
import util.Random.nextInt


val stopWordsString = Array("a", "across", "am", "an", "and", "any", "are", "as", "at", "be", "been", "being", "but", "by", "can", "could", "did", "do", "does", "each", "for", "from", "had", "has", "have", "in", "into", "is", "isn't", "it", "it'd", "it'll", "it's", "its", "of", "on", "or", "that", "that's", "thats", "the", "there", "there's", "theres", "these", "this", "those", "to", "under", "until", "up", "were", "will", "with", "would" )


// It's like running ls on the directory specified
def ls(dir: String): List[String] = {
    val files = new ArrayBuffer[String]
    for (file <- new File(dir).list) {
        files += (dir+"/"+file)
    }
    return files.toList
}

// show the type of something (debugging)
def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]

// converts big endian to little endian
def ntoh(int: Int) : Int = {
    return (0x000000ff & (int>>24)) | (0x0000ff00 & (int>> 8)) | (0x00ff0000 & (int<< 8)) | (0xff000000 & (int<<24));
}

//read the reviews, generate doc matrix and ratings map
// doc_matrix: columns are doc IDs, rows are feature IDs.
// ratings: doc_id -> rating
def read_token_bin(vocab: mutable.Map[Int,Int], dict:mutable.Map[String,Int], rdict:mutable.Map[Int,String]): (BIDMat.SMat, BIDMat.FMat) = {
    var ii_rows_terms = new ArrayBuffer[Int]
    var jj_cols_docs = new ArrayBuffer[Int]
    var vv_vals_counts = new ArrayBuffer[Int]
    var ratings = mutable.Map[Int,Float]()


    var in_review_text = false
    var in_rating = false
    var index = 0
    var words = mutable.Map[Int,Int]() // per-document temp featureID -> # occurences

    val fis = new FileInputStream("tokens.bin")
    val dis = new DataInputStream(new BufferedInputStream(fis, 4194304))
    try {
        while (true) {
            var line = ntoh(dis.readInt)
            var pos = ntoh(dis.readInt)
            var token = ntoh(dis.readInt)

            //println("line: " + line + " pos: " + pos + " token: " + token)

            if (vocab contains token) {
                if (vocab(token) == dict("<rating>")) {
                    //println("begin rating")
                    in_rating = true
                } else if (vocab(token) == dict("<review_text>")) {
                    in_review_text = true
                } else if (vocab(token) == dict("</review_text>")) {
                    if (index % 10000 == 0) {
                        println(ii_rows_terms.length)
                    }
                    // we're done with this doc!
                    in_review_text = false

                    // update the lists
                    ii_rows_terms ++= words.keys.toList
                    jj_cols_docs ++= (for (i <- List.range(0,words.keys.toList.length)) yield index)
                    vv_vals_counts ++= words.values.toList

                    // reset
                    index += 1
                    words = mutable.Map[Int,Int]()
                } else if (in_rating) {
                    // extract the numerical rating value
                    //println("in_rating")
                    if (vocab(token) == dict("1")) {
                        ratings(index) = 1.0.toFloat
                    } else if (vocab(token) == dict("2")) {
                        ratings(index) = 2.0.toFloat
                    } else if (vocab(token) == dict("3")) {
                        ratings(index) = 3.0.toFloat
                    } else if (vocab(token) == dict("4")) {
                        ratings(index) = 4.0.toFloat
                    } else if (vocab(token) == dict("5")) {
                        ratings(index) = 5.0.toFloat
                    } else {
                        println(rdict(vocab(token)))
                        throw new IllegalArgumentException("Only ratings between 0-5")
                    }
                    in_rating = false // got the rating
                } else if (in_review_text) {
                    val idx = vocab(token)
                    if (words contains idx) {
                        words(idx) = 1
                    } else {
                        words(idx) = 1
                    }
                }
            }
        }
    } catch {
        case ioe: IOException => true
        case oob: ArrayIndexOutOfBoundsException => oob.printStackTrace()
    }

    println("done! creating matrix...")
    //icol(ii_rows_terms.toList)
    //println("rows: "+ii_rows_terms.toList)
    //icol(jj_cols_docs.toList)
    //println("cols: "+jj_cols_docs.toList)
    //icol(vv_vals_counts.toList)
    //println("vals: "+vv_vals_counts.toList)


    val doc_matrix = sparse(icol(ii_rows_terms.toList), icol(jj_cols_docs.toList), icol(vv_vals_counts.toList))
    val ratings_matrix = full(sparse(icol(ratings.keys.toList), icol((for (i <- List.range(0,ratings.keys.toList.length)) yield 0)), col(ratings.values.toList)))
    return (doc_matrix, ratings_matrix)
}


// dict.txt index to feature id. only considers words that appear at least frequency_threshold times.
// vocab: dictID -> featureID
// dict: word -> featureID
// rdict: featureID -> word
def loadDict(frequency_threshold: Int) : (mutable.Map[Int,Int], mutable.Map[String,Int], mutable.Map[Int,String]) = {
    var curr_id = 0
    var vocab = mutable.Map[Int,Int]() // dict.txt index to feature id
    var dict = mutable.Map[String,Int]() // word to feature id
    var rdict = mutable.Map[Int,String]() // feature id to word
    val f = scala.io.Source.fromFile("dict.txt")
    for (line <- f.getLines) {
        val fields = line.split("""\t""")
        if (fields.length == 4) {
            val rank = fields(1).toInt
            val freq = fields(2).toInt
            val word = fields(3)
            if (freq >= frequency_threshold) {
                vocab(rank) = curr_id
                dict(word) = curr_id
                rdict(curr_id) = word
                curr_id += 1
            }
        }
    }
    f.close
    return (vocab, dict, rdict)
}

// y: a matrix of ratings in (review m by 1) size. (m = # of reviews)
//    It is needed to be transposed for calculation
// X: word occurrence matrix in (review m) by (word n) size.
// return value: beta matrix, which contains weighting values of each word in (word n) * 1 size.


// doc_matrix is a matrix w/ # rows = # features and # columns = # documents
// beta is a column w/ # rows = # features
// ratings is a column w/  # rows = # documents
// gamma is learning rate
// lambda is regularizaton parameter
class LinearRegression(num_features: Int, gamma: Double, lambda: Double, set_iteration:Int) {
    var beta:FMat = zeros(num_features,1) // column w/ # rows = # number of features
    var X = sparse(0)
    var y = FMat(0)
    var train_set = List[Int](0)
    val threshold = 10.0
	var TP = 0; var TN = 0; var FP = 0; var FN = 0

    // re-compute beta
    // training_group is a list of document ind
    def train(training_group: List[Int]) = {
        // setup the training -- reset beta
        println("train")
        train_set = training_group
        beta = zeros(num_features,1)
        var iter = 0
		var mean_of_error = 0.0
		var prev_mean_of_error = 0.0
		var error_term = 0.0

        while (true) { // termination??
            // calculate gradients for each block
            for (block_num <- train_set) {
                // we read in each block from disk one at a time (don't worry
                // about access times, they'll be cached if there is space in
                // RAM). For each block, we compute the gradient with respect
                // to our current beta value and update beta accordingly. This
                // is the SGD where we consider a block of reviews at a time.
                //println("loading X")
                X = load("data/doc_matrix."+block_num+".mat", "d").asInstanceOf[SMat]
                //println("loading y")
                y = load("data/ratings."+block_num+".mat", "r").asInstanceOf[FMat]
                if (X.ncols != y.nrows) { println("FATAL: X y dimension mismatch") }
                //println("success")

                // update beta
                beta_update(y, X)

                // TODO: we need to track the appropriate error function to figure out how well we're doing and convergence.
				error_term = error(y, X)
                if (block_num % 5 == 0) {
                    println("block: " + block_num + " error=" + error_term)
                }
				mean_of_error = mean_of_error + error_term
            }

            var msg = "sumsqr_error at iter ["+iter + "] is [" + error_term + "]\n"
			val out = new java.io.FileWriter("data/error." + set_iteration, true)
			out.write(msg)
			out.close
            //println(residual(y,X).t)

			mean_of_error = mean_of_error / train_set.size.toDouble
            println("mean of error=" + mean_of_error)
			var diff = mean_of_error - prev_mean_of_error
			diff = abs(diff)(0, 0).toDouble
			println("mean diff = " + diff)
			if (diff < 10.0) {
				break
			}
			else if (diff < 30.0) {
				saveAs("data/beta."+set_iteration + "." +iter+".mat", beta, "b")
			}
			prev_mean_of_error = mean_of_error
            iter += 1
        }
    }

    def classify(validation_set: Int) = {
        // TODO: run the other blocks through, classify them, generate statistics.
		X = load("data/doc_matrix."+validation_set+".mat", "d").asInstanceOf[SMat]
        y = load("data/ratings."+validation_set+".mat", "r").asInstanceOf[FMat]
		var expected = (beta.t * X).t

		// if (both R and y are 5.0) -> TP. 
		// if (y = 5.0 but R != 5.0) -> FP. 
		// if (y != 5.0 but R = 5.0) -> FN. 
		// if (both R and y != 5.0) -> TN.
		for (i <- 0 to y.nrows - 1) {
			if (y(i, 0) == 5.0 && expected(i, 0) >= 4.5) {
				TP += 1
			}
			else if (y(i, 0) == 5.0 && expected(i, 0) < 4.5) {
				FP += 1
			}
			else if (y(i, 0) != 5.0 && expected(i, 0) >= 4.5) {
				FN += 1
			}
			else if (y(i, 0) != 5.0 && expected(i, 0) < 4.5) {
				TN += 1
			}
		}
		saveAs("data/TP."+set_iteration + ".mat", TP.toString, "tp")
		saveAs("data/FP."+set_iteration + ".mat", FP.toString, "fp")
		saveAs("data/FN."+set_iteration + ".mat", FN.toString, "fn")
		saveAs("data/TN."+set_iteration + ".mat", TN.toString, "tn")
    }

    def residual(y: FMat, X: SMat): FMat = y - X.t * beta

    // this calculates the gradient function
    def ridge_gradient(y: FMat, X: SMat): FMat = {
        // these lines were for debug
        //println("ridge1: "+beta.t.nrows+"x"+beta.t.ncols+" * "+X.nrows+"x"+X.ncols)
        //val gr1  = beta.t * X
        //println("ridge2: "+gr1.t.nrows+"x"+gr1.t.ncols+" - "+y.nrows+"x"+y.ncols)
        //val gr2 = gr1.t - y
        //println("ridge3: "+gr2.t.nrows+"x"+gr2.t.ncols+" * "+X.t.nrows+"x"+X.t.ncols)
        //val gradient = 2.0 *@ (gr2.t * X.t)

        val gradient = 2.0 *@ ((beta.t * X).t - y).t * X.t
        val ridge = 2.0 * lambda * beta
        return gradient.t + ridge
    }

    def beta_update(y: FMat, X: SMat) = {
        beta -= gamma * ridge_gradient(y, X)
        //println(beta)
    }

    def error(y: FMat, X: SMat) : Double = {
        return sum_sqrd_error(y, X)
    }

    def sum_sqrd_error(y: FMat, X: SMat) : Double = {
        val err = residual(y, X)
        return (err.t * err)(0,0)
    }
}

// creates the block files
// if you don't already have the block files, you have to run prep(true);
// otherwise run prep(false) to load existing block files. you need to make
// sure the "./data" directory exists.
def prep(generate: Boolean) : (mutable.Map[Int,Int], mutable.Map[String,Int], Int) = {
    val (vocab, dict, rdict) = loadDict(1000)

    var num_features = 0

    // TODO: only generate if the files don't exist.
    if (generate) {
        val (doc_matrix, ratings) = read_token_bin(vocab, dict, rdict)
        num_features = doc_matrix.nrows
        var i = 1
        val num_blocks = 10
        for (i <- 0 to (num_blocks - 1)) { // 100 blocks
            val slice_start = i * (doc_matrix.ncols / num_blocks)
            val slice_stop = math.min((i+1) * (doc_matrix.ncols / num_blocks), doc_matrix.ncols-1)
            saveAs("data/doc_matrix."+i+".mat", doc_matrix(?,slice_start to slice_stop), "d")
            saveAs("data/ratings."+i+".mat", ratings(slice_start to slice_stop,?), "r")
        }
    } else {
        val doc_matrix = load("data/doc_matrix.0.mat", "d").asInstanceOf[SMat] // just to get num_features
        //val ratings = load("ratings.mat", "r").asInstanceOf[SMat]
        num_features = doc_matrix.nrows
    }
    return (vocab, dict, num_features)
}

def generate_training_set(count: Int): (List[Int], Int) = {
	var list = List[Int]()
    while (list.size < count) {
        var ran = Stream.continually(nextInt(count + 1)).take(1)
        if (list.contains(ran.toList(0)) == false) {
			list = ran.toList(0) :: list
        }
    }
	var vali = -1
	for (i <- 0 to count) {
		if (list.contains(i) == false) {
			assert (vali == -1)
			vali = i
		}
	}
println("training: " + list)
println("validation: " + vali)
return (list, vali)
}

def main() = {
    tic
    val (vocab, dict, num_feat) = prep(false) // we don't need the vocab and dict stuff anymore.

	// TODO: Instead of doing it in a one loop, can we do it concurrently?
	//for (i <- 0 to 9) {
		// 1. Randomly select 90 of the 100 blocks to use as training set.
		// arg: # of elements you want
		val (training_set, validation_set) = generate_training_set(9)

		// 2. For each training set, train a classifier.
		// It doesn't converge /at all/ if gamma is >= 0.000001
		//TODO: i is distinct number for each p2-i.scala. Each p2-i.scala should have distinct i's. it's for concurrent running.
		var i = 0
		val lr = new LinearRegression(num_feat, 0.0000005, 0.1, i)
		lr.train(training_set)

		// 3. Validate the classifier on the held out blocks.
		lr.classify(validation_set)
	//}

//    println("doc cols: " + doc_matrix.ncols + " terms: " + doc_matrix.nrows)
//    println("rating cols: " + ratings.ncols + " terms: " + ratings.nrows)
//    println("beta cols: " + beta.ncols + " terms: " + beta.nrows)

    println(toc)
}
