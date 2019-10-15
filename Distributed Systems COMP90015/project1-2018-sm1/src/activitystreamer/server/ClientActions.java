package activitystreamer.server;

import java.util.ArrayList;

import org.json.simple.JSONObject;

import activitystreamer.util.Settings;

public class ClientActions {
	
	private JSONObject content;
	private MessageValidator mValid;
	
	public ClientActions(JSONObject content) {
		this.content = content;
		this.mValid = new MessageValidator();
	}
	
	public ClientActions() {
		
	}

	/** 3 actions of login
	 * @return
	 */
	public Response login(Connection con) {
		SingleUser sUser;
		if (!mValid.login(content)) {
			return invalidMessage("message is not valid");
		}
		if (content.get(Settings.USERNAME).equals(Settings.ANONYMOUS)) {
			sUser = new SingleUser(((String) content.get(Settings.USERNAME)), null);
		}else {
			sUser = new SingleUser(((String) content.get(Settings.USERNAME)), (String) content.get(Settings.SECRET));
		}
		if (Control.getUsers().usernameExist(sUser.getUsername())){
			if (Control.getUsers().userMatch(sUser.getUsername(), sUser.getSecret()) || sUser.isAnonymous()) {
				con.setClient();
				con.setUser(sUser);
				return loginSuccess();
			}else {
				return new Response(loginFailed(), true);
			}
		}else {
			return new Response(userNotExist(), true);
		}
	}

	/** Action if login was success
	 * @return
	 */
	private Response loginSuccess() {
		String firstMsg = ServerMessage.ctInfo(Settings.LOGINSUCCESS, "logged in as user " + ((String) content.get(Settings.USERNAME)));
		Response tempResponse;
		if (isAvailableServer()>=0) {
			String secondMsg = redirect();
			tempResponse = new Response(firstMsg, secondMsg, true);
		}else {
			tempResponse = new Response(firstMsg);
		}
		return tempResponse;
	}
	
	/** Action if login was failed
	 * @return
	 */
	private String loginFailed() {
		Response tempResponse = new Response(ServerMessage.ctInfo(Settings.LOGINFAIL, "attempt to login with wrong secret"), true);
		return tempResponse.getResponse();
	}
	
	/** Action if user name is not exist in database
	 * @return
	 */
	private String userNotExist() {
		Response tempResponse = new Response(ServerMessage.ctInfo(Settings.LOGINFAIL, ((String) content.get(Settings.USERNAME)) + " not found"), true);
		return tempResponse.getResponse();
	}
	
	/** Redirect to another server
	 * It firstly finds the best server and get the details of
	 * that server, then returns to client.
	 * After the client receives the response, the connection
	 * will be closed.
	 * @return Host name and port of the new server
	 */
	private String redirect() {
		ArrayList<ServerDetails> sd = Control.getInstance().getSd();
		int serverId = isAvailableServer();
		ServerDetails bestServer = null;
		if (serverId>=0) {
			bestServer = sd.get(serverId);
		}
		return ServerMessage.ctRedirect(bestServer.getHost(), bestServer.getPort());
	}
	
	/** Find position in array list of an available server
	 * which has 2 less clients than current server
	 * @return
	 */
	private int isAvailableServer() {
		ArrayList<ServerDetails> sd = Control.getInstance().getSd();
		Long best = (long) Control.getInstance().getClients().size() - 2;
		for (int i=0; i<sd.size(); i++) {
			Long currentLoad = sd.get(i).getLoad();
			if (currentLoad<best) {
				return i;
			}
		}
		return -1;
	}
	
	/** Construct logout message
	 * @return
	 */
	public Response logout() {
		return new Response(true);
	}
	
	public Response invalidMessage(String msg) {
		return new Response(ServerMessage.ctInfo(Settings.INVALIDMESSAGE, msg), true);
	}
	
	/** Register a new user
	 * @return
	 */
	public Response register(Connection con) {
		String username = (String) content.get(Settings.USERNAME);
		String secret = (String) content.get(Settings.SECRET);
		Response failMsg = new Response(ServerMessage.ctInfo(Settings.REGISTERFAIL, username + " is already registered with the system"), true);
		if (Control.getUsers().usernameExist((String) content.get(Settings.USERNAME))) {
			return failMsg;
		} else {
			ServerMessage.sendLockRequest(username, secret, con, Settings.REGISTER);
			return new Response(false);
		}
	}
	
	public Response directRegister(String username, String secret) {
		Response failMsg = new Response(ServerMessage.ctInfo(Settings.REGISTERFAIL, username + " is already registered with the system"), true);
		Response successMsg = new Response(ServerMessage.ctInfo(Settings.REGISTERSUCCESS, "register success for " + username), false);
		if (Control.getUsers().usernameExist(username)) {
			return failMsg;
		} else {
			Control.getUsers().addUser(username, secret);
			return successMsg;
		}
	}
}
