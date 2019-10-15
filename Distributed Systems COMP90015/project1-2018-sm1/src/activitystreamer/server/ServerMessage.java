package activitystreamer.server;

import org.json.simple.JSONObject;

import activitystreamer.util.Settings;

public class ServerMessage {
	private static UIFrame frame = UIFrame.getInstance();
	
	/** Send authenticate message
	 * @param con
	 */
	public static void sendAuthenticate(Connection con) {
		con.writeMsg(ctAuth());
	}
	
	/** Construct authenticate message
	 * @return
	 */
	@SuppressWarnings("unchecked")
	private static String ctAuth() {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.AUTHENTICATE);
		obj.put(Settings.SECRET, Settings.getSecret());
		return obj.toJSONString();
	}
	
	/** Construct basic INFO message
	 * @param command Command name
	 * @param msg Message content
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static String ctInfo(String command, String msg) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, command);
		obj.put(Settings.INFO, msg);
		return obj.toJSONString();
	}
	
	/** Construct basic redirect message
	 * @param hostname Host name
	 * @param port Host port
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static String ctRedirect(Object hostname, Object port) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.REDIRECT);
		obj.put(Settings.HOSTNAME, hostname);
		obj.put(Settings.PORT, port);
		return obj.toJSONString();
	}
	
	/** Construct basic lock message
	 * @param command Command name
	 * @param username User name
	 * @param secret Secret
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static String ctLock(String command, String username, String secret) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, command);
		obj.put(Settings.USERNAME, username);
		obj.put(Settings.SECRET, secret);
		return obj.toJSONString();
	}
	
	/** Construct server announce message
	 * @param id
	 * @param load
	 * @param host
	 * @param port
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static String serverAnnounceMsg(String id, int load, String host, int port) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.SERVERANNOUNCE);
		obj.put(Settings.ID, id);
		obj.put(Settings.LOAD, load);
		obj.put(Settings.HOSTNAME, host);
		obj.put(Settings.PORT, port);
		return obj.toJSONString();
	}
	
	/** Construct activity broadcast message
	 * @param activity
	 * @param username
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static String activityBroadcastMsg(JSONObject activity, String username) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.ACTIVITYBROADCAST);
		obj.put(Settings.ACTIVITY, activity);
		activity.put(Settings.AUTHENTICATEDUSER, username);
		return obj.toJSONString();
	}
	
	/** Send lock request message to all servers
	 * @param username
	 * @param secret
	 * @param con
	 * @param type
	 */
	public static void sendLockRequest(String username, String secret, Connection con, String type) {
		frame.setAction("lock request sent");
		Broadcaster.getInstance().broadcastLock(ServerMessage.ctLock(Settings.LOCKREQUEST, username, secret), username, secret, con, type);
	}
}
