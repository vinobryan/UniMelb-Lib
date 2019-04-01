package activitystreamer.server;

import java.util.ArrayList;

public class Broadcaster {
	
	private static Broadcaster instance;
	
	public static Broadcaster getInstance() {
		if(instance == null) {
			instance = new Broadcaster();
		}
		return instance;
	}
	
	/** Broadcast message to all servers
	 * @param msg
	 */
	public void broadcastToServer(String msg) {
		for (Connection eachServer:Control.getInstance().getServers()) {
			eachServer.writeMsg(msg);
		}
	}
	
	
	/** Broadcast message to current connected servers
	 * @param msg
	 */
	public void activityMessageOnServer(String msg) {
		ArrayList<Connection> servers = new ArrayList<>(Control.getInstance().getServers());
		ArrayList<Connection> clients = new ArrayList<>(Control.getInstance().getClients());
		for (Connection eachServer:servers) {
			eachServer.writeMsg(msg);
		}
		for (Connection eachClient:clients) {
			eachClient.writeMsg(msg);
		}
	}
	
	/** Broadcast lock message to all servers
	 * @param msg
	 * @param username
	 * @param secret
	 * @param con
	 * @param type
	 */
	public void broadcastLock(String msg, String username, String secret, Connection con, String type) {
		ArrayList<Connection> servers = Control.getServersExcept(con);
		int serverNum = servers.size();
		for (Connection eachServer:servers) {
			eachServer.writeMsg(msg);
		}
		Control.getUq().addUserReg(username, secret, con, serverNum, type);
	}
	
	/** Forward a message to all servers except the one whom 
	 * we received from
	 * @param msg
	 * @param receiveFrom
	 */
	public void forwardToServer(String msg, Connection receiveFrom) {
		for (Connection eachServer:Control.getInstance().getServers()) {
			if(eachServer != receiveFrom ) {
				eachServer.writeMsg(msg);
			}
		}
	}
	
	/** Forward a message to all clients except the one whom 
	 * we received from
	 * @param msg
	 * @param receiveFrom
	 */
	public void forwardToClient(String msg, Connection receiveFrom) {
		for (Connection eachClient:Control.getInstance().getClients()) {
			if(eachClient != receiveFrom ) {
				eachClient.writeMsg(msg);
			}
		}
	}
	
	/** Broadcast message to all clients
	 * @param msg
	 */
	public void broadcastToClient(String msg) {
		for (Connection client:Control.getInstance().getClients()) {
			client.writeMsg(msg);
		}
	}
	
	/** Broadcast announce message to all servers
	 * @param id
	 * @param load
	 * @param host
	 * @param port
	 */
	public void broadcastAnnounce(String id, int load, String host, int port) {
		UIFrame frame = UIFrame.getInstance();
		frame.setAnnounce(ServerMessage.serverAnnounceMsg(id, load, host, port));
		broadcastToServer(ServerMessage.serverAnnounceMsg(id, load, host, port));
	}
	
	public void broadcastToCurrentClient(String msg, ArrayList<String> users) {
		for (Connection eachClient:Control.getInstance().getClients()) {
			if(users.contains((eachClient.getUser().getUsername()))) {
				eachClient.writeMsg(msg);
			}
		}
	}
	
	public void forwardToCurrentClient(String msg, Users users, Connection receiveFrom) {
		for (Connection eachClient:Control.getInstance().getClients()) {
			if(users.contains(eachClient.getUser()) && eachClient != receiveFrom) {
				eachClient.writeMsg(msg);
			}
		}
	}
}
