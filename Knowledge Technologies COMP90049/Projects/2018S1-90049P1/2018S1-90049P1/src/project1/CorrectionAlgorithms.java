package project1;

import java.util.ArrayList;

import org.apache.commons.codec.EncoderException;
import org.apache.commons.codec.language.Soundex;

import info.debatty.java.stringsimilarity.LongestCommonSubsequence;

class CorrectionAlgorithms{
    
	private final int DELETECOST = 1;
	private final int INSERTCOST = 1;
	private final int MATCHCOST = 0;
	private final int REPLACECOST = 1;
	private final int EQUAL = 0;
	
    /** This function implements Global Edit Distance using Levenshtein
     * Distance Parameter, where the score rules are match = 0, insert = 1,
     * delete = 1 and replace = 1.
     * @param a The incorrect string
     * @param b The string in a dictionary
     * @return The global edit distance
     */
	int globalEditDistance(String a, String b){
    	// Totally match, return 0 directly
        if (a.equals(b)) {
            return EQUAL;
        }
        int lenA = a.length();
        int lenB = b.length();
        int[][] array = new int[lenA + 1][lenB + 1];
        
        // Initialize array
        for(int initA = 0; initA <= lenA; initA++){
            array[initA][0] = initA * INSERTCOST;
        }
        for(int initB = 0; initB <= lenB; initB++){
            array[0][initB] = initB * DELETECOST;
        }
        
        for(int row = 1; row <= lenA; row++){
            for(int col = 1; col <= lenB; col++){
            	/** First min: insert/delete/replace = 1
            	 *  Second min: match = 0 or not match (from first min) = 1.
            	 *  because this is using Levenshtein Distance Parameter,
            	 *  here we use math.min instead of math.max
            	 * */
                array[row][col] = Math.min(
                				Math.min(array[row-1][col] + INSERTCOST,
                                array[row][col-1] + DELETECOST),
                                array[row-1][col-1] + 
                                (a.charAt(row-1) == 
                                b.charAt(col-1)?MATCHCOST:REPLACECOST));
            }
        }
        return array[lenA][lenB];
    }
    
    /** The 2-gram distance from string a to b
     * @param a The first string
     * @param b The second string
     * @return 2-gram distance
     */
    int twoGramDistance(String a, String b){
    	ArrayList<String> splitA = this.nGramSplit(a, 2);
    	ArrayList<String> splitB = this.nGramSplit(b, 2);
    	int sizeA = splitA.size();
    	int sizeB = splitB.size();
		return sizeA + sizeB - 2 * inCommon(splitA, splitB);
    }
    
    /** Find how many strings are in common in two array list
     * @param a The first array list
     * @param b The second array list
     * @return Number of in commons
     */
    private int inCommon(ArrayList<String> a, ArrayList<String> b) {
    	int count = 0;
    	for (int index = 0; index < a.size(); index++) {
    		count = count + (b.contains(a.get(index))?1:0);
    	}
    	return count;
    }
    
    /** Split a String into n-gram
     * @param a The string need to be split
     * @param n Gram number
     * @return The splited array list
     */
    private ArrayList<String> nGramSplit(String a, int n){
    	ArrayList<String> list = new ArrayList<String>();
    	int lenOfStr = a.length();
    	/* The first item should be always the first char */
    	list.add(a.substring(0, 1));
    	for (int index = 0; index < lenOfStr - n + 1; index++) {
    		list.add(a.substring(index, index + n));
    	}
    	/* The last item should be always the last char */
    	list.add(a.substring(lenOfStr - 1, lenOfStr));
    	return list;
    }
    
    
    /** Find local edit distance of 2 strings
     * @param a The first string
     * @param b The second string
     * @return local edit distance
     */
    double localEditDistance(String a, String b) {
    	LongestCommonSubsequence lcs = new LongestCommonSubsequence();
    	double distance = 0;
		distance = lcs.distance(a, b);
    	return distance;
    }
    
    /** Find Soundex distance of 2 strings
     * @param a The first string
     * @param b The second string
     * @return Soundex distance
     */
    int soundexDistance(String a, String b) {
    	Soundex soundex = new Soundex();
    	int distance = 0;
		try {
			distance = soundex.difference(a, b);
		} catch (EncoderException e) {
			e.printStackTrace();
		}
    	return distance;
    }
    
    /** Calculate the accuracy of an algorithm
     * accuracy = first string correct count / total number of first string
     * @param raw The attempts list
     * @param correct The correct list
     * @return the accuracy
     */
    public float accuracy(ArrayList<ArrayList<String>> raw, 
    		ArrayList<String> correct) {
    	int count = 0;
    	for (int index = 0; index < raw.size(); index++) {
    		count = count + 
    				(raw.get(index).get(0).equals(correct.get(index))?1:0);
    	}
    	float accuracy = (float) count / raw.size();
    	return accuracy;
    }
    
    /** Calculate the recall of the word attempts
     * recall = correct / total number of string
     * @param raw List of lists containing all attempts
     * @param correct The correct words list
     * @return The recall
     */
    public float recall(ArrayList<ArrayList<String>> raw, 
    		ArrayList<String> correct) {
    	int count = 0;
    	for (ArrayList<String> tempRaw:raw) {
    		count = count + (isInCorrect(tempRaw, correct)?1:0);
    	} 
    	return (float) count / raw.size();
    }
    
    /** Calculate the precision of an algorithm
     * accuracy = correct count / predict count
     * @param raw The attempts list
     * @param correct The correct list
     * @return the precision
     */
    public float precision(ArrayList<ArrayList<String>> raw, 
    		ArrayList<String> correct) {
    	int predictCount = 0;
    	int correctCount = 0;
    	for (ArrayList<String> tempRaw:raw) {
    		correctCount = correctCount + (isInCorrect(tempRaw, correct)?1:0);
    		predictCount = predictCount + tempRaw.size();
    	} 
    	float precision = (float) correctCount / predictCount;
    	return precision;
    }
    
    
    /** To find whether each word in a list is correct
     * @param raw The list of words
     * @param correct The correct form
     * @return Return true if it is correct
     */
    private boolean isInCorrect(ArrayList<String> raw, 
    		ArrayList<String> correct) {
    	for (String tempRaw:raw) {
    		if (correct.contains(tempRaw)) {
    			return true;
    		}
    	}
    	return false;
    }
    
    /** Get global edit distance result of two lists
     * @param a The first list
     * @param b The second list
     * @return The list of lists containing all possible attempts 
     */
    public ArrayList<ArrayList<String>> 
    getGlobalEditDistanceResult(ArrayList<String> a, ArrayList<String> b){
    	
    	ArrayList<ArrayList<String>> results = 
    			new ArrayList<ArrayList<String>>();
    	for (int indexA = 0; indexA < a.size(); indexA ++) {
    		int bestScore = Integer.MAX_VALUE;
    		ArrayList<String> tempResult = new ArrayList<String>();
    		for (int indexB = 0; indexB < b.size(); indexB ++) {
    			int tempScore = 
    					globalEditDistance(a.get(indexA), b.get(indexB));
 			    if (tempScore < bestScore) {
 			    	bestScore = tempScore;
 			    	tempResult.clear();
 			    	tempResult.add(b.get(indexB));
	  			}
	  			if (tempScore == bestScore) {
	  				tempResult.add(b.get(indexB));
	  			}
    		}
    		results.add(tempResult);
    	}
    	return results;
    }

    /** Get local edit distance result of two lists
     * @param a The first list
     * @param b The second list
     * @return The list of lists containing all possible attempts 
     */
    public ArrayList<ArrayList<String>> 
    getLocalEditDistanceResult(ArrayList<String> a, ArrayList<String> b){
    	
    	ArrayList<ArrayList<String>> results = 
    			new ArrayList<ArrayList<String>>();
    	for (int indexA = 0; indexA < a.size(); indexA ++) {
    		double bestScore = Double.MAX_VALUE;
    		ArrayList<String> tempResult = new ArrayList<String>();
    		for (int indexB = 0; indexB < b.size(); indexB ++) {
    			double tempScore = 
    					localEditDistance(a.get(indexA), b.get(indexB));
 			    if (tempScore < bestScore) {
 			    	bestScore = tempScore;
 			    	tempResult.clear();
 			    	tempResult.add(b.get(indexB));
	  			}
	  			if (tempScore == bestScore) {
	  				tempResult.add(b.get(indexB));
	  			}
    		}
    		results.add(tempResult);
    	}
    	return results;
    }

    /** Get two-gram distance result of two lists
     * @param a The first list
     * @param b The second list
     * @return The list of lists containing all possible attempts 
     */
    public ArrayList<ArrayList<String>> getTwoGramResult(ArrayList<String> a, 
    		ArrayList<String> b){
    	ArrayList<ArrayList<String>> results = 
    			new ArrayList<ArrayList<String>>();
    	for (int indexA = 0; indexA < a.size(); indexA ++) {
    		int bestScore = Integer.MAX_VALUE;
    		ArrayList<String> tempResult = new ArrayList<String>();
    		for (int indexB = 0; indexB < b.size(); indexB ++) {
    			int tempScore = twoGramDistance(a.get(indexA), b.get(indexB));
 			    if (tempScore < bestScore) {
 			    	bestScore = tempScore;
 			    	tempResult.clear();
 			    	tempResult.add(b.get(indexB));
	  			}
	  			if (tempScore == bestScore) {
	  				tempResult.add(b.get(indexB));
	  			}
    		}
    		results.add(tempResult);
    	}
    	return results;
    }
    
    /** Get soundex distance result of two lists
     * @param a The first list
     * @param b The second list
     * @return The list of lists containing all possible attempts 
     */
    public ArrayList<ArrayList<String>> getSoundexResult(ArrayList<String> a,
    		ArrayList<String> b){
    	ArrayList<ArrayList<String>> results = 
    			new ArrayList<ArrayList<String>>();
    	for (int indexA = 0; indexA < a.size(); indexA ++) {
    		int bestScore = 0;
    		ArrayList<String> tempResult = new ArrayList<String>();
    		for (int indexB = 0; indexB < b.size(); indexB ++) {
    			int tempScore = soundexDistance(a.get(indexA), b.get(indexB));
 			    if (tempScore > bestScore) {
 			    	bestScore = tempScore;
 			    	tempResult.clear();
 			    	tempResult.add(b.get(indexB));
	  			}
	  			if (tempScore == bestScore) {
	  				tempResult.add(b.get(indexB));
	  			}
    		}
    		results.add(tempResult);
    	}
    	return results;
    }
    
    /** Get optimised result of two lists
     * @param misspell The misspelling list
     * @param fo The file operation class
     * @return The list of lists containing all possible attempts 
     */
    public ArrayList<ArrayList<String>> 
    getOptimisedResult(ArrayList<String> misspell, FileOperation fo){
    	ArrayList<String> newDict = fo.getNewDictionary();
    	ArrayList<ArrayList<String>> results = 
    			new ArrayList<ArrayList<String>>();
    	for (int indexA = 0; indexA < misspell.size(); indexA ++) {
    		int gedBest = Integer.MAX_VALUE;
    		double ledBest = Double.MAX_VALUE;
    		int soundexBest = 0;
    		ArrayList<String> gedTempResult = new ArrayList<String>();
    		ArrayList<String> ledTempResult = new ArrayList<String>();
    		ArrayList<String> soundexResult = new ArrayList<String>();
    		
    		/* Find a list using Soundex */
    		for (int indexB = 0; indexB < newDict.size(); indexB ++) {
    			int soundexTempScore = soundexDistance(misspell.get(indexA), 
    					newDict.get(indexB));
 			    if (soundexTempScore > soundexBest) {
 			    	soundexBest = soundexTempScore;
 			    	soundexResult.clear();
 			    	soundexResult.add(newDict.get(indexB));
	  			}
	  			if (soundexTempScore == soundexBest) {
	  				soundexResult.add(newDict.get(indexB));
	  			}
    		}
    		
    		/* Find best matching using both led and ged*/
    		for (int indexB = 0; indexB < soundexResult.size(); indexB ++) {
    			int gedTempScore = globalEditDistance(misspell.get(indexA),
    					soundexResult.get(indexB));
    			double ledTempScore = localEditDistance(misspell.get(indexA),
    					soundexResult.get(indexB));
    			if (gedTempScore > 0) {
    				if (gedTempScore < gedBest) {
     			    	gedBest = gedTempScore;
     			    	gedTempResult.clear();
     			    	gedTempResult.add(soundexResult.get(indexB));
    	  			}else if (gedTempScore == gedBest) {
    	  				gedTempResult.add(soundexResult.get(indexB));
    	  			}
    			}
 			    if (ledTempScore > 0) {
 			    	if (ledTempScore < ledBest) {
 	 			    	ledBest = ledTempScore;
 	 			    	ledTempResult.clear();
 	 			    	ledTempResult.add(soundexResult.get(indexB));
 		  			}else if (ledTempScore == ledBest) {
 		  				ledTempResult.add(soundexResult.get(indexB));
 		  			}
 			    }
    		}
    		
    		/* Select better one */
    		if (gedBest < ledBest) {
    			results.add(gedTempResult);
    		}else {
    			results.add(ledTempResult);
    		}
    		System.out.println(indexA+"/"+misspell.size());
    	}
    	return results;
    }

}