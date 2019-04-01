package automail;

// import exceptions.RobotNotInMailRoomException;
import exceptions.TubeFullException;

import java.util.Stack;

/**
 * The storage tube carried by the robot.
 */
public class StorageTube {

    public final int MAXIMUM_CAPACITY = 4;
    public Stack<MailItem> tube;

    /**
     * Constructor for the storage tube
     */
    public StorageTube(){
        this.tube = new Stack<MailItem>();
    }

    /**
     * Check if the storage tube is full
     */
    public boolean isFull(){
        return tube.capacity() == MAXIMUM_CAPACITY;
    }

    /**
     * Check if the storage tube is empty
     */
    public boolean isEmpty(){
        return tube.isEmpty();
    }

    /**
     * Add an item to the tube
     * @param item The item being added
     * @throws TubeFullException thrown if an item is added which exceeds the capacity
     */
    public void addItem(MailItem item) throws TubeFullException { //, RobotNotInMailRoomException {
    	/** Item can only be added when the robot is in the mailroom **/
    	/*
    			if(Robot.getState() != Robot.RobotState.WAITING){
    		throw new RobotNotInMailRoomException();
    	}
    	*/
        int current = getTotalOfSizes();
        if(current + item.getSize() <= MAXIMUM_CAPACITY){
        	tube.add(item);
        } else {
            throw new TubeFullException(current,item.getSize());
        }
    }

    /** Returns the size of the tube **/
    public int getTotalOfSizes(){
    	// Aggregate the total size of the items in the tube
    	int totalSize = 0;
    	for(MailItem mailItem : tube){
    		totalSize += mailItem.getSize();
    	}
    	return totalSize;
    }
    
    /** Pop the first item off the stack */
    public MailItem pop(){
        return tube.pop();
    }

}
