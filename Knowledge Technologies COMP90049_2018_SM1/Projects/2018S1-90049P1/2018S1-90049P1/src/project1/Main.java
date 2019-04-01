package project1;

import java.util.ArrayList;

class Main{
	
    static long startTime;
    
    
    /** Main function
     * @param args
     */
    public static void main(String args[]){
    	startTime=System.currentTimeMillis();
	   
    	FileOperation fo = new FileOperation();
    	ArrayList<String> misspellingList = fo.getMisspelling();
    	ArrayList<String> dictList = fo.getDictionary();
    	ArrayList<String> correctList = fo.getCorrect();
    	System.out.println("Words in Raw Dict: " + 
    			dictList.size()); 
    	printResult(misspellingList, correctList, 
    			dictList, startTime, fo);
    	long endTime = System.currentTimeMillis();
    	System.out.println("Total Running Time: " + 
    			((endTime-startTime)/1000)+"s");
    }
   
    /** Print all results
     * @param misspelling
     * @param correct
     * @param dict
     * @param startTime
     * @param fo
     */
    public static void printResult(ArrayList<String> misspelling, 
		   							ArrayList<String> correct, 
		   							ArrayList<String> dict, long startTime, 
		   							FileOperation fo) {
	   
    	CorrectionAlgorithms alg = new CorrectionAlgorithms();
    	ArrayList<ArrayList<String>> gedResult = 
    			alg.getGlobalEditDistanceResult(misspelling, dict);
    	resultOutput(gedResult, correct);
    	ArrayList<ArrayList<String>> ledResult = 
    			alg.getLocalEditDistanceResult(misspelling, dict);
    	resultOutput(ledResult, correct);
    	ArrayList<ArrayList<String>> tgResult = 
    			alg.getTwoGramResult(misspelling, dict);
    	resultOutput(tgResult, correct);
    	ArrayList<ArrayList<String>> soudexResult = 
    			alg.getSoundexResult(misspelling, dict);
    	resultOutput(soudexResult, correct);
    	ArrayList<ArrayList<String>> oaResult =
    			alg.getOptimisedResult(misspelling, fo);
	    resultOutput(oaResult, correct);
   
    }
   
    /** Print formated result
     * @param result
     * @param correct
     */
    public static void resultOutput(ArrayList<ArrayList<String>> result, 
    			ArrayList<String> correct) {
    	CorrectionAlgorithms alg = new CorrectionAlgorithms();
    	float precision = alg.precision(result, correct);
    	float acc = alg.accuracy(result, correct);
    	float recall = alg.recall(result, correct);
    	System.out.println("precision - " + precision * 100 + "% - accuracy - " +
    			acc * 100 + "% - recall - " + recall * 100 + "%");
    	System.out.println("Current Running Time: " + 
    			((System.currentTimeMillis() - startTime)/1000) + "s");
    	
    }
}