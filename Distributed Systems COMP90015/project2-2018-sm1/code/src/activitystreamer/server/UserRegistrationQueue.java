package activitystreamer.server;

import java.util.ArrayList;

import activitystreamer.util.Settings;

public class UserRegistrationQueue {
	
	private ArrayList<LockWaitingUser> userRegStatus = null;
	private UIFrame frame = UIFrame.getInstance();
	
	public UserRegistrationQueue() {
		userRegStatus = new ArrayList<LockWaitingUser>();
	}

	/** Get user registration queue
	 * @param username
	 * @return
	 */
	public LockWaitingUser getUserReg(String username) {
		for (LockWaitingUser eachUser:userRegStatus) {
			if (eachUser.getUsername().equals(username)) {
				return eachUser;
			}
		}
		return null;
	}
	
	/** Add an user into user registration queue
	 * @param username
	 * @param secret
	 * @param con
	 * @param currentServerNum
	 * @param type
	 */
	public void addUserReg(String username, String secret, Connection con, int currentServerNum, String type) {
		LockWaitingUser su = new LockWaitingUser(username, secret, con, currentServerNum, type);
		userRegStatus.add(su);
	}
	
	/** Set an user into denied
	 * @param username
	 * @param con
	 */
	public void deniedUserReg(String username, Connection con) {
		for (LockWaitingUser eachUser:userRegStatus) {
			if (eachUser.getUsername().equals(username)) {
				eachUser.setDenied();
			}
		}
	}
	
	/** Add a success on an user
	 * @param username
	 */
	public void addOneUserRegSuccess(String username) {
		for (LockWaitingUser eachUser:userRegStatus) {
			if (eachUser.getUsername().equals(username)) {
				eachUser.setAllowedTimes();
			}
		}
	}
	
	/** Regularly check the status of user registration queue
	 * Remove success or denied users
	 * 
	 */
	public void userRegStatusCheck() {
		frame.setCurrentRegQueue(userRegStatus.size());
		for (int i=0; i<userRegStatus.size(); i++) {
			LockWaitingUser eachUser = userRegStatus.get(i);
			String username = eachUser.getUsername();
			String secret = eachUser.getSecret();
			Connection con = eachUser.getCon();
			if(eachUser.getAllowedTimes() == Settings.USERREGDENIED) {
				Response tempResponse;
				if (eachUser.isRegister()) {
					tempResponse = new Response(ServerMessage.ctLock(Settings.REGISTERFAIL, username, secret));
				}else {
					tempResponse = new Response(ServerMessage.ctLock(Settings.LOCKDENIED, username, secret));
				}
				
				con.writeMsg(tempResponse.getResponse());
				userRegStatus.remove(eachUser);
			}else if(eachUser.allAllowed()) {
				ClientActions cAction = new ClientActions();
				cAction.directRegister(eachUser.getUsername(), eachUser.getSecret());
				Response tempResponse;
				if (eachUser.isRegister()) {
					tempResponse = new Response(ServerMessage.ctLock(Settings.REGISTERSUCCESS, username, secret));
				}else {
					tempResponse = new Response(ServerMessage.ctLock(Settings.LOCKALLOWED, username, secret));
				}
				con.writeMsg(tempResponse.getResponse());
				userRegStatus.remove(eachUser);
			}
		}
	}
}
