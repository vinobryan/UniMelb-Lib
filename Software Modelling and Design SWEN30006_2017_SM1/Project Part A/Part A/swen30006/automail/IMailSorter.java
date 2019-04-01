package automail;

/**
 * A MailSorter sorts the mail and is called at every step by the simulation.
 */
public interface IMailSorter {
    /** Fills the robot's tube or backpack and tells robot whether or not to go (start delivering) */
    /** This method is called in Simulation.java on every time step */
    boolean fillStorageTube(StorageTube tube);
}
