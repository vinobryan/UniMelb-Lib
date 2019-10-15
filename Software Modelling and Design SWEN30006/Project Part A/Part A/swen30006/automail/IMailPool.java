package automail;

/**
 * A MailPool is called from MailGenerator during a certain time step
 * i.e. when t = 3 if there is mail to be delivered, addToPool will be called
 * which adds a MailItem to the MailPool.
 * The data structure in the MailPool is determined by your implementation.
 */
public interface IMailPool {
	/**
     * Adds an item to the mail pool
     * @param mailItem the mail item being added.
     */
    void addToPool(MailItem mailItem);
}
