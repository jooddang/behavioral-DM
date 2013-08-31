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

val stopWordsString = Array("a", "across", "am", "an", "and", "any", "are", "as", "at", "be", "been", "being", "but", "by", "can", "could", "did", "do", "does", "each", "for", "from", "had", "has", "have", "in", "into", "is", "isn't", "it", "it'd", "it'll", "it's", "its", "of", "on", "or", "that", "that's", "thats", "the", "there", "there's", "theres", "these", "this", "those", "to", "under", "until", "up", "were", "will", "with", "would" )

val files = Array ("beta.0.320.mat", "beta.1.326.mat", "beta.2.300.mat", "beta.3.300.mat", "beta.4.325.mat", "beta.5.320.mat", "beta.6.318.mat", "beta.7.321.mat", "beta.8.299.mat", "beta.9.307.mat")

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

def run_posneg(file1: String, rdict: mutable.Map[Int,String]) = {
	var pos_rows = List[Int]()
	var pos_value = -1000000.0
	var pos_row = -1
	var pos_terms = List[String]()
	var neg_rows = List[Int]()
	var neg_value = 1000000.0
	var neg_row = -1
	var neg_terms = List[String]()
	var flag = 0
	
	while (flag == 0) {
	for (file <- files) {
		var beta = load("final_data/" + file, "b").asInstanceOf[FMat]
		for (i <-0 to beta.nrows - 1) {
			if (beta(i) > pos_value && pos_terms.contains(rdict(i)) == false && stopWordsString.contains(rdict(i)) == false) {
				pos_value = beta(i)
				pos_row = i
			}
			if (beta(i) < neg_value && neg_terms.contains(rdict(i)) == false && stopWordsString.contains(rdict(i)) == false) {
				neg_value = beta(i)
				neg_row = i
			}
		}
	}
		//pos row = [" + pos_row + "]   "neg row = [" + neg_row + "]
		var msg = "POS: " + rdict(pos_row) + " : " + pos_value + "  NEG: " + rdict(neg_row) + " : " + neg_value + "\n"
		val out = new java.io.FileWriter("output/posneg." , true)
		out.write(msg)
		out.close
		pos_rows = pos_rows :+ pos_row
		pos_terms = pos_terms :+ rdict(pos_row)
		neg_rows = neg_rows :+ neg_row
		neg_terms = neg_terms :+ rdict(neg_row)
		assert(pos_rows.size == neg_rows.size)
		if (pos_rows.size == 30)
			flag = 1

		pos_value = -1000000.0
		neg_value = 1000000.0
	}
}

def main() = {
    val (vocab, dict, rdict) = loadDict(1000)
	
	//for (file <- files) {
		run_posneg("Asdf", rdict)
	//}
}
