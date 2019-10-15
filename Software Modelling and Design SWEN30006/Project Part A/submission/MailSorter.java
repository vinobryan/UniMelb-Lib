package strategies;

import automail.Clock;
import automail.IMailSorter;
import automail.StorageTube;
import exceptions.TubeFullException;

/**
 * This class adds the mail to the tube
 */
public class MailSorter implements IMailSorter{
	MailPool mailPool;
	private final int ARRAY_START_POSITION = 0;
	
	public MailSorter(MailPool MailPool) {
		this.mailPool = MailPool;
	}
	
	/**
	 * Fill the tube with MAX CAPACITY each time, fetching
	 * items from mail pool
	 */
	public boolean fillStorageTube(StorageTube tube) {
        try{
        	for (int index=0; index<tube.MAXIMUM_CAPACITY; index++){
        		if (!mailPool.isEmptyPool()) {
        			if (tube.getTotalOfSizes() + 
        					mailPool.get(ARRAY_START_POSITION).getSize() <= tube.MAXIMUM_CAPACITY){
        				/** if not exceed the tube size */
        				tube.addItem(mailPool.get(ARRAY_START_POSITION));
                		mailPool.remove(ARRAY_START_POSITION);
        			}else{
        				/** Full tube exception */
        				return true;
        			}
        		}
        	}
        }
        catch(TubeFullException e){
        	/** Full tube exception */
        	return true;
        }      
        if(Clock.Time() > Clock.LAST_DELIVERY_TIME && mailPool.isEmptyPool() && !tube.isEmpty()){
            return true;
        }
        return false;
	}
}
