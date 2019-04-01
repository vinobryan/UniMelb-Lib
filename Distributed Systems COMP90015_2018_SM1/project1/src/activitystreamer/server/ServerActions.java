package activitystreamer.server;


import java.util.ArrayList;

import org.json.simple.JSONObject;

import activitystreamer.util.Settings;

public class ServerActions {
	
	private JSONObject content;
	private MessageValidator mValid;
	private UIFrame frame = UIFrame.getInstance();
	
	public ServerActions(JSONObject content) {
		this.content = content;
		this.mValid = new MessageValidator();
	}
	
	/** Construct authenticate message
	 * @param con
	 * @return
	 */
	public Response authenticate(Connection con) {
		if (!mValid.authenticate(content)) {
			String msg = "the supplied secret is incorrect: " + (String) content.get(Settings.SECRET);
			return new Response(ServerMessage.ctInfo(Settings.AUTHENTICATEFAIL, msg), true);
		}
		con.setServer();
		return new Response(false);
	}
	
	/** Construct activity message
	 * @param con
	 * @return
	 */
	public Response activityMessage(Connection con) {
		SingleUser sUser;
		if(!mValid.activity(content)) {
			return invalidMessage("activity message is not valid");
		}
		if (content.get(Settings.USERNAME).equals(Settings.ANONYMOUS)) {
			sUser = new SingleUser(((String) content.get(Settings.USERNAME)), null);
		}else {
			sUser = new SingleUser(((String) content.get(Settings.USERNAME)), (String) content.get(Settings.SECRET));
		}
		String activityMsg = ServerMessage.activityBroadcastMsg((JSONObject) content.get(Settings.ACTIVITY), sUser.getUsername());
		if (sUser.isAnonymous()) {
			Broadcaster.getInstance().broadcastToServer(activityMsg);
			Broadcaster.getInstance().forwardToClient(activityMsg, con);
			return new Response(false);
		} else {
			if (con.getUser().isSame(sUser)) {
				Broadcaster.getInstance().broadcastToServer(activityMsg);
				Broadcaster.getInstance().forwardToClient(activityMsg, con);
				return new Response(false);
			} else {
				String msg = "verify authentication failed";
				return new Response(ServerMessage.ctInfo(Settings.AUTHENTICATEFAIL, msg), true);
			}
		}
	}
	
	/** Action for received a lock request from another server
	 * @return
	 */
	public Response receiveLockRequest(Connection con) {
		if (mValid.lock(content)) {
			frame.setAction("receive lock request from " + Settings.socketAddress(con.getSocket()));
			String username = (String) content.get(Settings.USERNAME);
			if (Control.getUsers().usernameExist(username)) {
				return lockDenied();
			}else {
				return lockAllowed(con);
			}
		}else {
			return invalidMessage("invalid lock request message");
		}
	}
	
	
	
	/** Broadcast from a server to all other servers if the server 
	 * received a LOCK_REQUEST and the user name was not already 
	 * known to the server in its local storage.
	 * @return Null response
	 */
	public Response lockAllowed(Connection con) {
		
		String username = (String)content.get(Settings.USERNAME);
		String secret = (String)content.get(Settings.SECRET);
		
		Response tempResponse = new Response(ServerMessage.ctLock(Settings.LOCKREQUEST, username, secret));
		Broadcaster.getInstance().broadcastLock(tempResponse.getResponse(), username, secret, con, Settings.LOCKREQUEST);
		return new Response(false);

	}
	
	/** Broadcast from a server to all other servers (between 
	 * servers only), if the server received a LOCK_REQUEST and 
	 * to indicate that an user name is already known, and the 
	 * user name and secret pair should not be registered.
	 * @return Null response
	 */
	public Response lockDenied() {
		String username = (String)content.get(Settings.USERNAME);
		String secret = (String)content.get(Settings.SECRET);
		Response tempResponse = new Response(ServerMessage.ctLock(Settings.LOCKDENIED, username, secret));
//		Broadcaster.getInstance().broadcastToServer(tempResponse.getResponse());
		return tempResponse;
	}
	
	/** Construct announce message
	 * @return
	 */
	public Response serverAnnounce() {
		if(!mValid.announce(content)) {
			return invalidMessage("incorrect SERVER_ANNOUNCE message");
		}
		updateServerDetails(content);
		return new Response(false);
	}
	
	
	/** Construct activity broadcast message
	 * @param receiveFrom
	 * @return
	 */
	public Response activityBroadcast(Connection receiveFrom) {
		if(!mValid.activityBroadcast(content)) {
			return invalidMessage("incorrect ACTIVITY_BROADCAST message");
		}
		Broadcaster.getInstance().broadcastToClient(content.toJSONString());
		Broadcaster.getInstance().forwardToServer(content.toJSONString(), receiveFrom);
		return new Response(false);
	}
	
	/** Update server details in local storage
	 * @param content
	 */
	private void updateServerDetails(JSONObject content) {
		boolean found = false;
		String id = (String) content.get(Settings.ID);
		String host = (String) content.get(Settings.HOSTNAME);
		Long load = (Long) content.get(Settings.LOAD);
		Long port = (Long) content.get(Settings.PORT);
		ArrayList<ServerDetails> sd = Control.getInstance().getSd();
		for (int i=0; i<sd.size(); i++) {
			if (sd.get(i).getId().equals(id)) {
				sd.get(i).setLoad(load);
				found = true;
				break;
			}
		}
		if (!found) {
			ServerDetails singleSd = new ServerDetails(id, host, port, load);
			sd.add(singleSd);
		}
	}
	
	/** Construct invalid message
	 * @param msg
	 * @return
	 */
	public Response invalidMessage(String msg) {
		return new Response(ServerMessage.ctInfo(Settings.INVALIDMESSAGE, msg), true);
	}
}
