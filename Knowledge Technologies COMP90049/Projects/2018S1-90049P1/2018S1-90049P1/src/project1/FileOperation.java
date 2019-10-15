package project1;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

public class FileOperation {
	
	/* Location of dictionary and misspelling file */
	private final String DICTLOCATION = "data/dictionary.txt";
	private final String NEWDICTLOCATION = "data/newDict.txt";
	private final String MISSPELLLOCATION = "data/misspell.txt";
	private final String CORRECTLOCATION = "data/correct.txt";
	
	private ArrayList<String> dictionary;
	private ArrayList<String> newDict;
	private ArrayList<String> misspelling;
	private ArrayList<String> correct;
	
	/**
	 * Constructor
	 */
	public FileOperation() {
		this.dictionary = new ArrayList<String>();
		this.misspelling = new ArrayList<String>();
		this.correct = new ArrayList<String>();
		this.newDict = new ArrayList<String>();
		this.readCorrect();
		this.readMispelling();
		this.readDictionary();
		addWordIntoDict(this.correct, this.dictionary);
		this.readNewDictionary();
	}

	/**
	 * Read the dictionary file
	 */
	public void readDictionary() {
        this.dictionary = this.readFile(this.DICTLOCATION);
	}
	
	/**
	 * Read the misspell file
	 */
	public void readMispelling() {
        this.misspelling = this.readFile(this.MISSPELLLOCATION);
	}
	
	/**
	 * Read the correct file
	 */
	public void readCorrect() {
        this.correct = this.readFile(this.CORRECTLOCATION);
	}
	
	/**
	 * Read the new dictionary file
	 */
	public void readNewDictionary() {
        this.newDict = this.readFile(this.NEWDICTLOCATION);
	}
	
	/**
	 * @param fileName The file name to be read
	 * @return An array list that includes the content of the file
	 */
	private ArrayList<String> readFile(String fileName) {
		ArrayList<String> tempBuffer = new ArrayList<String>();
        try{
        	FileReader reader = new FileReader(fileName);
        	BufferedReader br = new BufferedReader(reader);
	        String line = null;
	        while((line = br.readLine()) != null) {
	              tempBuffer.add(line);
	        }
	        br.close();
	        reader.close();
        }catch(FileNotFoundException e) {
        	e.printStackTrace();
        }catch(IOException e) {
            e.printStackTrace();
        }
        return tempBuffer;
	}

	/**
	 * @return The dictionary array list
	 */
	public ArrayList<String> getDictionary() {
		return this.dictionary;
	}
	
	/**
	 * @return The new dictionary array list
	 */
	public ArrayList<String> getNewDictionary() {
		return this.newDict;
	}

	/**
	 * @return The misspelling array list
	 */
	public ArrayList<String> getMisspelling() {
		return this.misspelling;
	}
	
	/**
	 * @return The correct array list
	 */
	public ArrayList<String> getCorrect() {
		return this.correct;
	}
	
	/** Add missing words from raw into dictionary
	 * @param raw Word list
	 * @param dict Dictionary
	 */
	public void addWordIntoDict(ArrayList<String> raw, ArrayList<String> dict){
		ArrayList<String> tempDict = new ArrayList<String>();
		for (String eachWord:dict) {
			tempDict.add(eachWord);
		}
		for (String eachRaw:raw) {
			if (!dict.contains(eachRaw)) {
				tempDict.add(eachRaw);
			}
		}
		File file = new File(NEWDICTLOCATION);
		if (!file.exists()) {  
            try {  
                file.createNewFile();
            } catch (IOException e) {  
                // TODO Auto-generated catch block  
                e.printStackTrace();  
            } 
        }
        try {  
            FileWriter fileWriter = new FileWriter(file); 
            for (String eachWord:tempDict) {
            	fileWriter.write(eachWord + "\n");
            }
            fileWriter.close();
        } catch (IOException e) {  
            // TODO Auto-generated catch block  
            e.printStackTrace();  
        }
        System.out.println("New dictionary created.");
	}
}
