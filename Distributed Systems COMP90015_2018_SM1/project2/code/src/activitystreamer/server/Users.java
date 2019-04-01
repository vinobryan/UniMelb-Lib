package activitystreamer.server;

import java.util.ArrayList;

public class Users {
	private ArrayList<User> users;
	
	/** Construct an user list, storing all users details
	 * 
	 */
	public Users() {
		users = new ArrayList<User>();
//		addAnonymous();
	}

//	/** Add an anonymous user into list
//	 * 
//	 */
//	private void addAnonymous() {
//		addUser(Settings.ANONYMOUS, "NO SECRET");
//	}
	 
	/** Add an user into list
	 * @param username
	 * @param secret
	 */
	public void addUser(String username, String secret) {
		User newUser = new User(username, secret);
		for (User user:users) {
			if (newUser.isSame(user)) {
				return;
			}
		}
		UIFrame.setUser(username, secret);
		users.add(newUser);
	}
	
	/** Whether an user exists in the list
	 * @param username
	 * @return
	 */
	public boolean usernameExist(String username) {
		if (users == null) {
			return false;
		}
		for (User eachUser:users) {
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
		for (User eachUser:users) {
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
		for (User eachUser:users) {
			if (eachUser.getUsername().equals(username)) {
				return eachUser.getSecret();
			}
		}
		return null;
	}
	
	public ArrayList<User> getAllUsers(){
		return users;
	}

	public boolean contains(User user) {
		if (users.contains(user)) { return true;}
		return false;
	}
	
}
