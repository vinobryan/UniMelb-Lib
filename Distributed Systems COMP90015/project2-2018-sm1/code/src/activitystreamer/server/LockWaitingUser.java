package activitystreamer.server;

import activitystreamer.util.Settings;

public class LockWaitingUser extends User{

	private int allowedTimes;
	private int currentServerNum;
	private Connection con;
	private String type;
	
	/** Construct an user in lock request queue
	 * @param username
	 * @param secret
	 * @param con
	 * @param currentServerNum
	 * @param type
	 */
	public LockWaitingUser(String username, String secret, Connection con, int currentServerNum, String type) {
		super(username, secret);
		this.con = con;
		this.currentServerNum = currentServerNum;
		allowedTimes = 0;
		this.type = type;
	}
	
	
	/** Get user's connection
	 * @return
	 */
	public Connection getCon() {
		return con;
	}

	/** Whether the user is request register
	 * @return
	 */
	public boolean isRegister() {
		if (type.equals(Settings.REGISTER)) {
			return true;
		}
		return false;
	}

	/** Get current server number
	 * @return
	 */
	public int getCurrentServerNum() {
		return currentServerNum;
	}
	
	/** Check whether lock requests are allowed
	 * @return
	 */
	public boolean allAllowed() {
		if (allowedTimes>=currentServerNum) {
			return true;
		}
		return false;
	}
	public int getAllowedTimes() {
		return allowedTimes;
	}
	public void setAllowedTimes() {
		this.allowedTimes++;
	}
	public void setDenied() {
		this.allowedTimes = Settings.USERREGDENIED;
	}
	public boolean isDenied() {
		if (allowedTimes == Settings.USERREGDENIED) {
			return true;
		}
		return false;
	}
}
