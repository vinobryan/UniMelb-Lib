package activitystreamer.server;

import java.util.ArrayList;

import activitystreamer.util.Settings;

public 	class SingleUser{
	private String username;
	private String secret;
	private ArrayList<String> messages;
	private boolean isLogin = false;

	public SingleUser(String username, String secret) {
		this.username = username;
		this.secret = secret;
		this.messages = new ArrayList<String>();
	}
	
	public boolean isLogedin() {
		return isLogin;
	}
	
	public void login() {
		isLogin = true;
	}
	
	public void logout() {
		isLogin = false;
	}
	
	public ArrayList<String> getMessageList(){
		return messages;
	}
	
	public boolean isMessage() {
		return messages.isEmpty();
	}
	
	public void addMessage(String msg) {
		messages.add(msg);
	}
	
	public void removeMessage(int index) {
		messages.remove(index);
	}

	/** Get user's user name
	 * @return
	 */
	public String getUsername() {
		return username;
	}
	
	/** Set user's user name
	 * @param username
	 */
	public void setUsername(String username) {
		this.username = username;
	}
	
	/** Get user's secret
	 * @return
	 */
	public String getSecret() {
		return secret;
	}
	
	/** Set user's secret
	 * @param secret
	 */
	public void setSecret(String secret) {
		this.secret = secret;
	}
	
	/** Whether this user is anonymous
	 * @return
	 */
	public boolean isAnonymous() {
		return this.username.equals(Settings.ANONYMOUS);
	}
	
	/** Whether 2 users are identical
	 * @param u
	 * @return
	 */
	public boolean isSame(SingleUser u) {
		if (u.getUsername().equals(this.username) && u.getSecret().equals(this.secret)) {
			return true;
		}else {
			return false;
		}
	}
}
