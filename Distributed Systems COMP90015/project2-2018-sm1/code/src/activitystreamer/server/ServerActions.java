package activitystreamer.server;


import java.util.ArrayList;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import activitystreamer.util.Settings;

public class ServerActions {
	
	private JSONObject content;
	private JSONParser parser = new JSONParser();
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
		return new Response(ServerMessage.authenticateSuccess());
	}
	
	/** Construct activity message
	 * @param con
	 * @return
	 * @throws ParseException 
	 */
	@SuppressWarnings("unchecked")
	public Response activityMessage(Connection con) {
		User sUser;
		Users users = Control.getInstance().getUsers();  // an ArrayList of users that currently connect to servers
		if(!mValid.activity(content)) {
			return invalidMessage("activity message is invalid");
		}
		if (content.get(Settings.USERNAME).equals(Settings.ANONYMOUS)) {
			sUser = new User(((String) content.get(Settings.USERNAME)), null);
		}else {
			sUser = new User(((String) content.get(Settings.USERNAME)), (String) content.get(Settings.SECRET));
		}
		String activityMsg = ServerMessage.activityBroadcastMsg((JSONObject) content.get(Settings.ACTIVITY), sUser.getUsername());
		if (sUser.isAnonymous()) {
			Broadcaster.getInstance().broadcastToServer(activityMsg);
			Broadcaster.getInstance().forwardToClient(activityMsg, con);
			return new Response(false);
		} else {
			if (con.getUser().isSame(sUser)) {
				Broadcaster.getInstance().forwardToCurrentClient(activityMsg, users, con);
				
				JSONObject objActivity = new JSONObject();;
				try {
					objActivity = (JSONObject)parser.parse(activityMsg);
				} catch (ParseException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				ArrayList<String> allusers = new ArrayList<String>();
				for (User eachUser:users.getAllUsers()) {
					allusers.add(eachUser.getUsername());
				}
				objActivity.put(Settings.USERINFO, allusers);
				Broadcaster.getInstance().broadcastToServer(objActivity.toJSONString());
				frame.setAction(objActivity.toJSONString());
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
			if (Control.getInstance().getUsers().usernameExist(username)) {
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
	public Response serverAnnounce(Connection con) {
		if(!mValid.announce(content)) {
			return invalidMessage("incorrect SERVER_ANNOUNCE message");
		}
		updateServerDetails(content, con);
		rebalance();
		return new Response(false);
	}
	
	
	/** Construct activity broadcast message
	 * @param receiveFrom
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Response activityBroadcast(Connection receiveFrom) {
		if(!mValid.activityBroadcast(content)) {
			return invalidMessage("incorrect ACTIVITY_BROADCAST message");
		}
		Broadcaster.getInstance().forwardToServer(content.toJSONString(), receiveFrom);
		
		ArrayList<String> users = (ArrayList<String>) content.get(Settings.USERINFO);
		content.remove(Settings.USERINFO);
		String activitiMsg = content.toJSONString();
		
		Broadcaster.getInstance().broadcastToCurrentClient(activitiMsg, users);
		
		return new Response(false);
	}
	
	/** Update server details in local storage
	 * @param content
	 */
	private void updateServerDetails(JSONObject content, Connection con) {
		boolean found = false;
		String id = (String) content.get(Settings.ID);
		String host = (String) content.get(Settings.HOSTNAME);
		Long load = (Long) content.get(Settings.LOAD);
		Long port = (Long) content.get(Settings.PORT);
		frame.setAction("port"+port+" ---id"+id);
		ArrayList<ServerDetails> sd = Control.getInstance().getSd();
		for (int i=0; i<sd.size(); i++) {
			if (sd.get(i).getId().equals(id)) {
				sd.get(i).setLoad(load);
				found = true;
				break;
			}
		}
		if (!found) {
			Control.getInstance().setSd(new ServerDetails(id, host, port, load, con));
		}
	}
	
	/** Redirect to another server
	 * It firstly finds the best server and get the details of
	 * that server, then returns to client.
	 * After the client receives the response, the connection
	 * will be closed.
	 * @return Host name and port of the new server
	 */
	public Response redirect() {
		String hostname = (String) content.get(Settings.HOSTNAME);
		Long port = (Long) content.get(Settings.PORT);
		ArrayList<Connection> clients = Control.getInstance().getClients();
		if (clients.isEmpty()) {
			return new Response(false);
		}
		clients.get(0).writeMsg(ServerMessage.ctRedirect(hostname, port));
		rebalance();
		return new Response(false);
	}
	
	public Response rebalance() {
    	ArrayList<ServerDetails> sds = Control.getInstance().getSd();
    	if (sds.size() == 0) {
    		return new Response(false);
    	}
    	int largestIdx = -1, smallestIdx = -1;
//    	frame.setAction("sds.size="+sds.size()+"--"+sds.get(0).getPort());
    	Long largest = (long) - 1, smallest = (long) Control.getInstance().getClients().size() + 2;
    	for (int i=0; i<sds.size(); i++) {
			Long currentLoad = sds.get(i).getLoad();
			if (currentLoad < smallest) {
				smallest = currentLoad;
				smallestIdx = i;
			}
			if (currentLoad > largest) {
				largest = currentLoad;
				largestIdx = i;
			}
		}
    	if(largest == 0 ) {
    		return new Response(false);
    	}
    	
    	Long selfLoad = (long) Control.getInstance().getClients().size();
    	
    	if (selfLoad > largest && selfLoad - smallest > 1) {
    		ServerDetails redirectTo = sds.get(smallestIdx);
    		Connection clientConnection = Control.getInstance().getClients().get(0);
    		clientConnection.writeMsg(ServerMessage.ctRedirect(redirectTo.getHost(), redirectTo.getPort()));
    	}
    	else if (selfLoad < smallest && largest - selfLoad > 1) {
    		ServerDetails redirectFrom = sds.get(largestIdx);
    		Connection serverConnection = redirectFrom.getConnection();
    		serverConnection.writeMsg(ServerMessage.rebalanceMsg(Settings.getLocalHostname(), Settings.getLocalPort(), 1));
    	}
    	else if (largest - smallest > 1) {
    		ServerDetails redirectTo = sds.get(smallestIdx);
    		ServerDetails redirectFrom = sds.get(largestIdx);
    		Connection serverConnection = redirectFrom.getConnection();
    		serverConnection.writeMsg(ServerMessage.rebalanceMsg(redirectTo.getHost(), redirectTo.getPort(), 1));
    	}
    	return new Response(false);
    }
	
	
	/** Construct invalid message
	 * @param msg
	 * @return
	 */
	public Response invalidMessage(String msg) {
		return new Response(ServerMessage.ctInfo(Settings.INVALIDMESSAGE, msg), true);
	}
}
