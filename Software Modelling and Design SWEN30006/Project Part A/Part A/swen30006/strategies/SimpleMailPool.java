package strategies;
import java.util.Stack;

import automail.MailItem;
import automail.IMailPool;

/**
 * Sample of what a MailPool could look like.
 * This one tosses the incoming mail on a pile and takes the outgoing mail from the top.
 */
public class SimpleMailPool implements IMailPool {

    /** Stack of mailItems pending delivery*/
    private Stack<MailItem> mailItems;

    public SimpleMailPool(){
        mailItems = new Stack<MailItem>();
    }

    @Override
    public void addToPool(MailItem mailItem){
        mailItems.push(mailItem);
    }
    
    public boolean isEmptyPool(){
        return mailItems.isEmpty();
    }
    
    public MailItem get(){
    	return mailItems.peek();
    }
    
    public void remove(){
        mailItems.pop();
    }

}
