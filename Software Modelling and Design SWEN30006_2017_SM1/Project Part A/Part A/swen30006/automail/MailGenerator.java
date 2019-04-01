package automail;

import java.util.*;

/**
 * This class generates the mail
 */
public class MailGenerator {

    private final int MAIL_TO_CREATE;

    private int mailCreated;

    private final Random random;
    /** This seed is used to make the behavior deterministic */
    
    private boolean complete;
    private IMailPool mailPool;

    private HashMap<Integer,ArrayList<MailItem>> allMail;

    /**
     * Constructor for mail generation
     * @param mailToCreate  how many mail to create
     * @param floors the number of floors in the building
     */
    public MailGenerator(int mailToCreate, IMailPool mailPool, HashMap<Boolean,Integer> seed){
        MAIL_TO_CREATE = mailToCreate;
        mailCreated = 0;
        if(seed.containsKey(true)){
        	this.random = new Random((long) seed.get(true));
        }
        else{
        	this.random = new Random();	
        }
        complete = false;
        allMail = new HashMap<Integer,ArrayList<MailItem>>();
        this.mailPool = mailPool;
    }

    /**
     * Creates a mail item that needs to be delivered
     */
    public MailItem generateMail(){
        int dest_floor = generateDestinationFloor();
        int size = generateSize();
        String priority_level = generatePriorityLevel();
        int arrival_time = generateArrivalTime();
        return new MailItem(dest_floor,size,priority_level,arrival_time);
    }

    /**
     * Creates a destination floor between the ranges of GROUND_FLOOR to FLOOR
     */
    public int generateDestinationFloor(){
        return Building.LOWEST_FLOOR + this.random.nextInt(Building.FLOORS);
    }

    /**
     * Creates a random size selected from the POSSIBLE_SIZES array
     */
    public int generateSize(){
        return (int) getRandom(MailItem.POSSIBLE_SIZES);
    }

    /**
     * Creates a random priority level selected from the PRIORITY_LEVELS array
     */
    public String generatePriorityLevel(){
        return (String) getRandom(MailItem.PRIORITY_LEVELS);
    }

    public int generateArrivalTime(){
        return 1 + random.nextInt(Clock.LAST_DELIVERY_TIME);
    }

    /**
     * Returns a random element from an array
     * @param array of objects
     */
    private Object getRandom(Object[] array){
        return array[random.nextInt(array.length)];
    }

    /**
     * This class initializes all mail and sets their corresponding values,
     */
    public void generateAllMail(){
        while(!complete){
            MailItem newMail =  generateMail();
            int timeToDeliver = newMail.getArrivalTime();
            /** Check if key exists for this time **/
            if(allMail.containsKey(timeToDeliver)){
                /** Add to existing array */
                allMail.get(timeToDeliver).add(newMail);
            }
            else{
                /** If the key doesn't exist then set a new key along with the array of MailItems to add during
                 * that time step.
                 */
                ArrayList<MailItem> newMailList = new ArrayList<MailItem>();
                newMailList.add(newMail);
                allMail.put(timeToDeliver,newMailList);
            }
            /** Mark the mail as created */
            mailCreated++;

            /** Once we have satisfied the amount of mail to create, we're done!*/
            if(mailCreated == MAIL_TO_CREATE){
                complete = true;
            }
        }
    }
    
    /**
     * While there are steps left, create a new mail item to deliver
     */
    public void step(){
    	// Check if there are any mail to create
        if(this.allMail.containsKey(Clock.Time())){
            for(MailItem mailItem : allMail.get(Clock.Time())){
                mailPool.addToPool(mailItem);
            }
        }


    }

}
