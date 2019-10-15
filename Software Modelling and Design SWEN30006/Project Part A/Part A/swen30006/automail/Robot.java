package automail;

/**
 * The robot delivers mail!
 */
public class Robot {

	StorageTube tube;
    IMailSorter sorter;
    IMailDelivery delivery;
    /** Possible states the robot can be in */
    public enum RobotState { DELIVERING, WAITING, RETURNING};
    public static RobotState current_state;
    private int current_floor;
    private int destination_floor;
    
    private MailItem deliveryItem;

    /**
     * Initiates the robot's location at the start to be at the mailroom
     * also set it to be waiting for mail.
     */
    public Robot(IMailSorter sorter, IMailDelivery delivery){
        // current_state = RobotState.WAITING;
    	current_state = RobotState.RETURNING;
        current_floor = Building.MAILROOM_LOCATION;
        tube = new StorageTube();
        this.sorter = sorter;
        this.delivery = delivery;
    }

    /**
     * This is called on every time step
     */
    public void step(){
    	
    	boolean go = false;
    	
    	switch(current_state) {
    		/** This state is triggered when the robot is returning to the mailroom after a delivery */
    		case RETURNING:
    			/** If its current position is at the mailroom, then the robot should change state */
                if(current_floor == Building.MAILROOM_LOCATION){
                    current_state = RobotState.WAITING; //Drop through
                } else {
                	/** If the robot is not at the mailroom floor yet, then move towards it! */
                    moveTowards(Building.MAILROOM_LOCATION);
                	break;
                }
    		case WAITING:
    			/** Tell the sorter the robot is ready */
                go = sorter.fillStorageTube(tube);
                // System.out.println("Tube total size: "+tube.getTotalOfSizes());
                /** If the StorageTube is ready and the Robot is waiting in the mailroom then start the delivery */
                if(go){
                	current_state = RobotState.DELIVERING;
                	setRoute();
                }
                break;
    		case DELIVERING:
    			/** If the robot is not at the destination yet, move towards it! */
                if(current_floor != destination_floor){
                    moveTowards(destination_floor);
                }
                else{
                    /** Delivery complete, report this to the simulator! */
                    delivery.deliver(deliveryItem);
                    /** Check if there are more items in the tube*/
                    if(tube.isEmpty()){
                        current_state = RobotState.RETURNING;
                    }
                    else{
                        /** If there are more items, set the robot's route to the location to deliver the item */
                        setRoute();
                    }
                }
                break;
    	}
    }

    /**
     * Sets the route for the robot
     */
    public void setRoute(){
        /** Pop the item from the StorageUnit */
        deliveryItem = tube.pop();
        /** Set the destination floor */
        destination_floor = deliveryItem.getDestFloor();
    }

    /** Generic function that moves the robot towards the destination */
    public void moveTowards(int destination){
        if(current_floor < destination){
            current_floor++;
        }
        else{
            current_floor--;
        }
    }

}
