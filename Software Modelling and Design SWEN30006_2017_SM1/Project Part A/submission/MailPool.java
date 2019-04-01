package strategies;

import java.util.ArrayList;

import automail.IMailPool;
import automail.MailItem;

/**
 * This class stores the mails
 */
public class MailPool implements IMailPool{
	
	private ArrayList<MailItem> mailItemHighP;
	private ArrayList<MailItem> mailItemMidP;
	private ArrayList<MailItem> mailItemLowP;

	
    public MailPool(){
    	mailItemHighP = new ArrayList<MailItem>();
    	mailItemMidP = new ArrayList<MailItem>();
    	mailItemLowP = new ArrayList<MailItem>();
    }
    
    /**
     * Add mail to its own priority list
     */
    public void addToPool(MailItem mailItem){
    	switch(mailItem.getPriorityLevel()){
        case "LOW":
        	mailItemLowP = addToMailItems(mailItem, mailItemLowP);
            break;
        case "MEDIUM":
        	mailItemMidP = addToMailItems(mailItem, mailItemMidP);
            break;
        case "HIGH":
        	mailItemHighP = addToMailItems(mailItem, mailItemHighP);
            break;
    	}
    }
    
    /**
     * Return true if those 3 priority lists are all empty, or false
     * if one of these is not empty
     */
    public boolean isEmptyPool(){
		return mailItemHighP.isEmpty()&&mailItemMidP.isEmpty()&&mailItemLowP.isEmpty();
    }
    
    /**
     * Fetch a mail from the list, starting from higher priority list
     */
    public MailItem get(int itemNumber){
    	if (!mailItemHighP.isEmpty()){
    		return mailItemHighP.get(itemNumber);
    	}else if (!mailItemMidP.isEmpty()){
    		return mailItemMidP.get(itemNumber);
    	}else{
    		return mailItemLowP.get(itemNumber);
    	}
    }
    
    /**
     * Delete a mail from the list, starting from higher priority list
     */
    public void remove(int itemNumber){
    	if (!mailItemHighP.isEmpty()){
    		mailItemHighP.remove(itemNumber);
    	}else if (!mailItemMidP.isEmpty()){
    		mailItemMidP.remove(itemNumber);
    	}else if (!mailItemLowP.isEmpty()){
    		mailItemLowP.remove(itemNumber);
    	}

    }
    
    /**
     * Adding algorithm, here we are using item size and floor to sort items in
     * each list
     */
    private ArrayList<MailItem> addToMailItems(MailItem mailItem, ArrayList<MailItem> mailItems){
    	boolean isFinished = false;
    	if (mailItems.isEmpty()){
    		/** empty list, add item directly */
    		mailItems.add(mailItem);
    		return mailItems;
    	}
    	for (int index=0; index<mailItems.size(); index++){
    		/** normal circumstance */
    		/** Size consider secondly (first priority) */
    		if (mailItem.getSize()==mailItems.get(index).getSize()){
    			/** Floor consider thirdly */
    			if (mailItem.getDestFloor()<mailItems.get(index).getDestFloor()){
    				mailItems.add(index, mailItem);
    				isFinished = true;
    				break;
    			}
    		}
    		if (mailItem.getSize()<mailItems.get(index).getSize()){
    			mailItems.add(index, mailItem);
    			isFinished = true;
    			break;
    		}

    	}
    	if (!isFinished){
    		/** last position detected, add to the end */
    		mailItems.add(mailItem);
    	}
    	return mailItems;
    }
}
