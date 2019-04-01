package activitystreamer.client;


import java.io.IOException;
import java.net.Socket;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import activitystreamer.util.Settings;

public class ClientSkeleton extends Thread {
	private static ClientSkeleton clientSolution;
	private TextFrame textFrame;
	private ClientConnection con;
	private JSONParser parser = new JSONParser();
	private static final Logger log = LogManager.getLogger();
	
	public static ClientSkeleton getInstance(){
		if(clientSolution==null){
			clientSolution = new ClientSkeleton();
		}
		return clientSolution;
	}
	
	public ClientSkeleton(){
		connect();
		textFrame = new TextFrame();
		start();
	}
	
	/** Connect to a server
	 * @return
	 */
	public boolean connect() {
		try {
			con = new ClientConnection(new Socket(Settings.getRemoteHostname(), Settings.getRemotePort()));
			return true;
		} catch (IOException e) {
			return false;
		}
	}
	
	public boolean process(String data) {
		textFrame.setOutputText(data);
		JSONObject results = construct(data);
		if (results!=null) {
			String command = (String) results.get(Settings.COMMAND);
			if (command.equals(Settings.REGISTERSUCCESS)) {
				login();
			}else if(command.equals(Settings.REGISTERFAIL)) {
				log.warn("Register failed");
				logout();
				System.exit(-1);
			}else if(command.equals(Settings.REDIRECT)) {
				log.warn("Redirect");
				Settings.setRemoteHostname((String) results.get(Settings.HOSTNAME));
				Settings.setRemotePort(((Long) results.get(Settings.PORT)).intValue());
				disconnect();
				connect();
				login();
			}else if(command.equals(Settings.LOGINFAIL)) {
				log.warn("Login failed");
				logout();
				System.exit(-1);
			}else if(command.equals(Settings.REGISTERSUCCESS)) {
				log.info(Settings.getSecret());
				textFrame.setOutputText(Settings.getSecret());
			}
			log.info(data);
		}
		return false;
	}
	
	/** Send activity message
	 * @param activityObj
	 */
	public void sendActivityObject(JSONObject activityObj){
		MessageConstructor mc = new MessageConstructor();
		String acMsg = mc.activity(activityObj);
		con.writeMsg(acMsg);
	}
	
	/** Send login message
	 * 
	 */
	public void login() {
		MessageConstructor mc = new MessageConstructor();
		String loginMsg = mc.login(Settings.getUsername(), Settings.getSecret());
		con.writeMsg(loginMsg);
	}
	
	/** Send logout message
	 * 
	 */
	public void logout() {
		MessageConstructor mc = new MessageConstructor();
		String logoutMsg = mc.logOut();
		con.writeMsg(logoutMsg);
	}
	
	/** Send anonymous login message 
	 * 
	 */
	public void anonymusLogin() {
		MessageConstructor mc = new MessageConstructor();
		String loginMsg = mc.anonymusLogin();
		con.writeMsg(loginMsg);
	}
	
	/** Send disconnect message
	 * 
	 */
	public void disconnect(){
		MessageConstructor mc = new MessageConstructor();
		String logoutMsg = mc.logOut();
		con.writeMsg(logoutMsg);
		con.closeCon();
	}
	
	/** Send register message
	 * 
	 */
	public void register() {
		MessageConstructor mc = new MessageConstructor();
		String register = mc.register(Settings.getUsername(), Settings.getSecret());
		con.writeMsg(register);
	}
	
	public void run(){

	}

	/** Construct a JSON message
	 * @param msg
	 * @return
	 */
	public JSONObject construct(String msg) {
		JSONObject content;
		try {
			content = (JSONObject) parser.parse(msg);
			return content;
		} catch (ParseException e) {
			return null;
		} catch (ClassCastException exc) {
			return null;
		}
	}
}
