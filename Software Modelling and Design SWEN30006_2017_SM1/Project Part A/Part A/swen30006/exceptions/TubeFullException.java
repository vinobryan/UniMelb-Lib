package exceptions;

/**
 * This exception is thrown when a MailItem is added to a StorageTube which does not have the
 * capacity to hold said MailItem
 */
public class TubeFullException extends Exception {

    public TubeFullException(int toAdd, int currentCapacity){
        super("Not enough space in the tube! " +
                "Trying to add: "+toAdd+
                " but there is "+currentCapacity+" space left!");
    }
}
