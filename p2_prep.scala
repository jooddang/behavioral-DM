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

prep(true)
