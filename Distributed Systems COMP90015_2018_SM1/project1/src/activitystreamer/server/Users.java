package activitystreamer.server;

import java.util.ArrayList;

import activitystreamer.util.Settings;

public class Users {
	private ArrayList<SingleUser> users;
	
	/** Construct an user list, storing all users details
	 * 
	 */
	public Users() {
		users = new ArrayList<SingleUser>();
		addAnonymous();
	}

	/** Add an anonymous user into list
	 * 
	 */
	private void addAnonymous() {
		addUser(Settings.ANONYMOUS, "NO SECRET");
	}
	 
	/** Add an user into list
	 * @param username
	 * @param secret
	 */
	public void addUser(String username, String secret) {
		UIFrame.setUser(username, secret);
		users.add(new SingleUser(username, secret));
	}
	
	/** Whether an user exists in the list
	 * @param username
	 * @return
	 */
	public boolean usernameExist(String username) {
		if (users == null) {
			return false;
		}
		for (SingleUser eachUser:users) {
			if (eachUser.getUsername().equals(username)) {
				return true;
			}
		}
		return false;
	}
	
	/** Whether an user matches user name and secret in list
	 * @param username
	 * @param secret
	 * @return
	 */
	public boolean userMatch(String username, String secret) {
		for (SingleUser eachUser:users) {
			if (eachUser.getUsername().equals(username)) {
				if (eachUser.getSecret().equals(secret)) {
					return true;
				}
				return false;
			}
		}
		return false;
	}
	
	/** Get an user's secret
	 * @param username
	 * @return
	 */
	public String getSecret(String username) {
		for (SingleUser eachUser:users) {
			if (eachUser.getUsername().equals(username)) {
				return eachUser.getSecret();
			}
		}
		return null;
	}
	
	public ArrayList<SingleUser> getAllUsers(){
		return users;
	}
	
}
