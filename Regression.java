package regression;

import java.io.*;
import java.util.*;
import java.util.Map.Entry;
//import regression.SparseMatrix;
import net.sf.javaml.clustering.mcl.*;

//import net.sf.json.*;

public class Regression {

  /**
	 * @param args
	 */
	static String[] stopWordsString = {"a", "across", "am", "an", "and", "any", "are", "as", "at", "be", "been", "being", "but", "by", "can", "could", "did", "do", "does", "each", "for", "from", "had", "has", "have", "in", "into", "is", "isn't", "it", "it'd", "it'll", "it's", "its", "of", "on", "or", "that", "that's", "thats", "the", "there", "there's", "theres", "these", "this", "those", "to", "under", "until", "up", "were", "will", "with", "would" };
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		// Build matrix and prepare data...
		HashMap<String, Integer> wordMap = BuildWordArray("/Users/jooddang/Downloads/dict.txt");
//		BuildWordMatrix("/Users/jooddang/Downloads/sorted_data/books/all.review", wordMap);
		ArrayList<Integer> rating = null;
		try {
			rating = getRating("rating");
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (rating == null) {
			System.out.println("y is null");
			return;
		}
		
		// iteration...
		double gamma = 1.;
		SparseMatrix beta = new SparseMatrix(wordMap.size(), 1);
		SparseMatrix yT = new SparseMatrix();
		for (int i = 0; i < rating.size(); i++) {
			yT.set(1, i, rating.get(i).doubleValue());
		}
		
		try {
			for (int i = 0; i <9; i++) {
				File file = new File("wordIndexMatrix" + Integer.valueOf(i).toString());
				if (file.isFile() == false) {
					throw new Exception();
				}
				FileInputStream f = new FileInputStream(file);
				ObjectInputStream s = new ObjectInputStream(f);
				System.out.println("i = " + i + " and s = " + s.toString());
				ArrayList<ArrayList<Integer>> wordIndexMatrix = (ArrayList<ArrayList<Integer>>) s.readObject();
				s.close();
				f.close();
				
				SparseMatrix X = new SparseMatrix(wordIndexMatrix.size(), wordMap.size());
				SparseMatrix XT = new SparseMatrix(wordMap.size(), wordIndexMatrix.size());
				
				// build matrix X, XT
				for (int reviewIndex = 0; reviewIndex < wordIndexMatrix.size(); reviewIndex++) {
					ArrayList<Integer> wordIndex = wordIndexMatrix.get(reviewIndex);
					for (Integer wi : wordIndex) {
						X.set(reviewIndex, wi.intValue(), 1.);
						XT.set(wi.intValue(), reviewIndex, 1.);
					}
				}
				
//				beta = beta - (gamma * 2 / wordIndexMatrix.size()) * ( XT.times(X).times(beta) - yT.times(X));
				SparseMatrix temp1 = XT.times(X).times(beta);
				SparseMatrix temp2 = yT.times(X);

				for (int j = 0; j < wordMap.size(); j++) {
					beta.set(j, 1, beta.get(j, 1) - (temp1.get(j, 1) - temp2.get(1, j) * (gamma * 2 / wordIndexMatrix.size())));
				}
			}
			writeFile(beta.toString(), "beta");
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static ArrayList<Integer> getRating(String fileName) throws Exception {
		// TODO Auto-generated method stub
		File file;
		ArrayList<Integer> ret = new ArrayList<Integer>();
		for (int i = 0; i < 9; i++) {
			file = new File(fileName + Integer.valueOf(i).toString());
//		file = new File(fileName);
		if (file.isFile() == false) {
			throw new Exception();
		}
		FileInputStream f = new FileInputStream(file);
		ObjectInputStream s = new ObjectInputStream(f);
		ret.addAll((ArrayList<Integer>) s.readObject());
		System.out.println("size of ret =" + ret.size());
		s.close();
		f.close();
		}
		return ret;
	}

	private static HashMap<String, Integer> BuildWordArray(String fileName) {
		// TODO Auto-generated method stub
		File file = new File("wordMap");
		HashMap<String, Integer> wordMap = new HashMap<String, Integer>();
		
		if (file.isFile()) {
			try {
				FileInputStream f = new FileInputStream(file);
				ObjectInputStream s = new ObjectInputStream(f);
				wordMap = (HashMap<String, Integer>) s.readObject();
				s.close();
				f.close();
			}
			catch (Exception e) {
				e.printStackTrace();
			}
			return wordMap;
		}
		
		file = new File(fileName);
		
		// store stop words
		HashMap<String, Integer> stopWords = new HashMap<String, Integer>();
		for (String s : stopWordsString) {
			stopWords.put(s, 1);
		}
		
		try {
			FileReader fr = new FileReader(file);
			BufferedReader br = new BufferedReader(fr);
			String s;
			int index = 0;
			
			while ((s = br.readLine()) != null) {
				
				String[] token = s.split("\t");
//				for (String t: token)
//					System.out.print("[" + t + "]");
//				System.out.println("");

				if (token.length < 4 || token[3].startsWith("<") || stopWords.containsKey(token[3])) {
					continue;
				}
//				wordMap.put(Integer.valueOf(token[1]), token[3]);
				wordMap.put(token[3], index);
				index += 1;
				if (index >= 100000) {
//				if (index >= 10) {
					break;
				}
			}
//			JSONArray json = JSONArray.fromObject(wordMap);
			
			fr.close();
			br.close();
			
			writeFile(wordMap, "wordMap");
			
			int test = 0;
			Iterator<Entry<String, Integer>> it = wordMap.entrySet().iterator();
			while (it.hasNext()) {
				System.out.println(it.next());
				test += 1;
				if (test >= 10) {
					break;
				}
			}
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
		return wordMap;
	}

	private static void writeFile(Object obj, String fileName)
			throws FileNotFoundException, IOException {
		File file;
		file = new File(fileName);
		FileOutputStream fos = new FileOutputStream(file);
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(obj);
		oos.close();
		fos.close();
	}

	private static void BuildWordMatrix(String fileName, HashMap<String, Integer> wordMap) {
		// TODO Auto-generated method stub
		File reviewFile = new File(fileName);

		try {
			FileReader reviewFR = new FileReader(reviewFile);
			BufferedReader br = new BufferedReader(reviewFR);
			String s;
			
			int reviewCount = 0;
			int fileIndex = 0;
			ArrayList<Integer> rating = new ArrayList<Integer>();
			ArrayList<ArrayList<Integer>> wordIndexMatrix = new ArrayList<ArrayList<Integer>>();
			
			while ((s = br.readLine()) != null) {
				// work per review...
				if (s.indexOf("<review>") != -1) {
					ArrayList<Integer> wordIndex = new ArrayList<Integer>();
					reviewCount += 1;
					// find rating.....
					while ((s = br.readLine()).indexOf("<rating>") == -1) {
						if (s.indexOf("</review>") != -1) {
							System.out.println("tag order is wrong........");
							throw new Exception();
						}
						continue;
					}
					s = br.readLine();
					// build rating array
					rating.add(Double.valueOf(s).intValue());
//					System.out.println("rating = " + s);
					
					// find review text
					while ((s = br.readLine()).indexOf("<review_text>") == -1) {
						if (s.indexOf("</review>") != -1) {
							System.out.println("tag order is wrong........");
							throw new Exception();
						}
						continue;
					}
					while ((s = br.readLine()).indexOf("</review_text>") == -1) {
						// here review text...
						String[] token = s.split(" ");
						for (String t : token) {
//							System.out.print("t=[" + t);
							String tt = t.toLowerCase().replaceAll("^\\W+", "");
//							System.out.print("]  ||t=[" + tt);
							tt = tt.toLowerCase().replaceAll("\\W+$", "");
//							System.out.print("]  ||t=[" + tt);
							if (wordMap.containsKey(tt) == true) {
								if (wordIndex.contains(wordMap.get(tt)) == false) {
//									System.out.println("true[" + tt + "]");
									wordIndex.add(wordMap.get(tt));	
								}
							}
						}
					}
					wordIndexMatrix.add(wordIndex);
//					break;
				}
				if (reviewCount >= 10000) {
					writeFile(wordIndexMatrix, "wordIndexMatrix" + Integer.valueOf(fileIndex).toString());
//					writeFile(rating, "rating" + Integer.valueOf(fileIndex).toString());
					reviewCount = 0;
					wordIndexMatrix.removeAll(wordIndexMatrix);
//					rating.removeAll(rating);
					fileIndex += 1;
				}
			}
			writeFile(rating, "rating");
			
			System.out.println(wordIndexMatrix.size());
			System.out.println(rating.size());
			System.out.println(reviewCount);
			
			reviewFR.close();
			br.close();
		}
//		catch (FileNotFoundException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
	}

}
