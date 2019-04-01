package automail;

import java.util.UUID;

/**
 * Represents a mail item
 */
public class MailItem {
	
    public final static Integer[] POSSIBLE_SIZES = {1,2,4};
    public final static String[] PRIORITY_LEVELS = {"LOW","MEDIUM","HIGH"};

    /** Represents the destination floor the mail is intended to go to*/
    private final int DESTINATION_FLOOR;
    private final int UNIT_SIZE;
    private final String ID;
    private final String PRIORITY_LEVEL;
    private final int ARRIVAL_TIME;

    /**
     * Constructor for a MailItem
     * @param dest_floor the destination floor intended for this mail item
     * @param mailSize the size of the mail can be 1,2 or 4
     */
    public MailItem(int dest_floor, int mailSize, String priority_level, int arrival_time){
        this.DESTINATION_FLOOR = dest_floor;
        this.UNIT_SIZE = mailSize;
        this.ID = UUID.randomUUID().toString();
        this.PRIORITY_LEVEL = priority_level;
        this.ARRIVAL_TIME = arrival_time;
    }

    @Override
    public String toString(){
        return "Mail Item : { " +
                "ID: " + ID +
                ", Destination: "+ DESTINATION_FLOOR +
                ", Size: "+ UNIT_SIZE +
                ", Priority Level: "+ PRIORITY_LEVEL +
                ", Arrival Time: "+ ARRIVAL_TIME +
                "}";
    }

    /**
     *
     * @return the destination floor of the mail item
     */
    public int getDestFloor() {
        return DESTINATION_FLOOR;
    }

    /**
     *
     * @return the unit size for a mail item
     */
    public int getSize() {
        return UNIT_SIZE;
    }

    /**
     *
     * @return the ID of a mail item
     */
    public String getId() {
        return ID;
    }

    /**
     *
     * @return the priority level of a mail item
     */
    public String getPriorityLevel(){
        return PRIORITY_LEVEL;
    }

    /**
     *
     * @return the arrival time of a mail item
     */
    public int getArrivalTime(){
        return ARRIVAL_TIME;
    }

}
