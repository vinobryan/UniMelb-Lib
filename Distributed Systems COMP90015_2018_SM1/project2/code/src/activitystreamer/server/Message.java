package activitystreamer.server;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;


import activitystreamer.util.Settings;

public class Message {
	private JSONObject content;
	private JSONParser parser = new JSONParser();
	private MessageValidator mValid= new MessageValidator();
	private UIFrame frame = UIFrame.getInstance();
	

	/** Transfer raw message from client into JSON format
	 * @param msg The raw message from client
	 * @return True if it constructs successfully
	 */
	public Boolean construct(String msg) {
		try {
			content = (JSONObject) parser.parse(msg);
			return true;
		} catch (ParseException e) {
			return false;
		} catch (ClassCastException exc) {
			return false;
		}
	}

	/** Redirect to specific task
	 * @return The corresponding response
	 * @throws ParseException 
	 */
	public Response directToTask(Connection con) {
		if (content.containsKey(Settings.COMMAND)) {
			ClientActions cAction = new ClientActions(content);
			ServerActions sAction = new ServerActions(content);
			if (con.getType().equals(Settings.UNDEFINED)) {
				switch ((String) content.get(Settings.COMMAND)) {
					case Settings.AUTHENTICATE: // only for server
						return sAction.authenticate(con);
					case Settings.LOGIN: // only for client
						return cAction.login(con);
					case Settings.REGISTER: // only for client
						return cAction.register(con);
					case Settings.ACTIVITYMESSAGE:
						return unauthenticatedActivity();
					default:
						return invalidMessage("command cannot be recognized");
				}
			}else if (con.getType().equals(Settings.SERVER)){
				switch ((String) content.get(Settings.COMMAND)) {
					case Settings.AUTHENTICATESUCCESS: // only for server
						return authenticateSuccess(con);
					case Settings.LOCKREQUEST: // only for server
						return sAction.receiveLockRequest(con);
					case Settings.SERVERANNOUNCE:
						return sAction.serverAnnounce(con);
					case Settings.ACTIVITYBROADCAST:
						return sAction.activityBroadcast(con);
					case Settings.LOCKALLOWED:
						return lockAllowReceived();
					case Settings.LOCKDENIED:
						return lockDeniedReceived(con);
					case Settings.REBALANCE:
						return sAction.redirect();
					default:
						return invalidMessage("command cannot be recognized");
				}
			}else{
				switch ((String) content.get(Settings.COMMAND)) {
					case Settings.LOGOUT: // only for client
						return cAction.logout();
//					case Settings.REDIRECT: // only for client
//						return cAction.redirect();
					case Settings.ACTIVITYMESSAGE:
						return sAction.activityMessage(con);
					default:
						return invalidMessage("command cannot be recognized");
				}
			}
		}else {
			return invalidMessage("the received message did not contain a command");
		}
	}
	
	public Response invalidMessage(String msg) {
		return new Response(ServerMessage.ctInfo(Settings.INVALIDMESSAGE, msg), true);
	}
	
	/** Send an INVALID_MESSAGE if anything is incorrect about the message or 
	 * if it receives a LOCK_ALLOWED from an unauthenticated server (i.e. the
	 * sender has not * authenticated with the * server secret). The connection
	 *  is closed.
	 * @return
	 */
	private Response lockAllowReceived() {
		if (mValid.lock(content)) {
			frame.setAction("receive lock allowed");
			String username = (String) content.get(Settings.USERNAME);
			if (Control.getUq() != null) {
				if(!Control.getUq().getUserReg(username).isDenied()) {
					Control.getUq().addOneUserRegSuccess(username);
				}
			}
			return new Response(false);
		}else {
			return invalidMessage("command cannot be recognized");
		}
	}
	
	/** Send an INVALID_MESSAGE if anything is incorrect about the 
	 * message or if it receives a LOCK_DENIED from an unauthenticated 
	 * server (i.e. the sender has not authenticated with the server secret).
	 * The connection is closed.
	 * @param con
	 * @return
	 */
	private Response lockDeniedReceived(Connection con) {
		if (mValid.lock(content)) {
			Control.getUq().deniedUserReg((content.get(Settings.USERNAME).toString()), con);
			return new Response(false);
		}else {
			return invalidMessage("command cannot be recognized");
		}
	}
	
	/** Construct unauthenticated activity message
	 * @return
	 */
	private Response unauthenticatedActivity() {
		return new Response(ServerMessage.ctInfo(Settings.AUTHENTICATEFAIL, "the user has not logged in yet"), true);
	}
	
	/** Construct unauthenticated activity message
	 * @return
	 */
	private Response authenticateSuccess(Connection con) {
		JSONObject users = (JSONObject) content.get(Settings.USERINFO);
		JSONObject servers = (JSONObject) content.get(Settings.SERVERINFO);
		for (Object key:users.keySet()) {
			String username = key.toString();
			String secret = users.get(username).toString();
			Control.getInstance().getUsers().addUser(username, secret);
		}
		for (Object key:servers.keySet()) {
			String id = key.toString();
			JSONObject serverInfo = (JSONObject) servers.get(id);
			String hostName = (String) serverInfo.get(Settings.HOSTNAME);
			String port = (String) serverInfo.get(Settings.PORT);
			Control.getInstance().addParentServers(new ServerDetails(hostName, Long.parseLong(port), con));
		}
		return new Response(false);
	}

}
