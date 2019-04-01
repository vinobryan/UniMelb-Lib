package activitystreamer.util;

import java.math.BigInteger;
import java.net.Socket;
import java.security.SecureRandom;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Settings {
	private static final Logger log = LogManager.getLogger();
	private static SecureRandom random = new SecureRandom();
	private static int localPort = 3780;
	private static String localHostname = "localhost";
	private static String remoteHostname = null; //"sunrise.cis.unimelb.edu.au";
	private static int remotePort = 3780;
	private static int activityInterval = 5000; // milliseconds
	private static String secret = "gen1p85md2qnq0d59qll3fbcoa";
	private static String username = "spicy-hot-pot";
	public static final String ANONYMOUS = "anonymous";
	public static final String SERVER = "S";
	public static final String CLIENT = "C";
	public static final String UNDEFINED = "U";
	public static final String COMMAND = "command";
	public static final String INFO = "info";
	public static final String SECRET = "secret";
	public static final String USERNAME = "username";
	public static final String HOSTNAME = "hostname";
	public static final String PORT = "port";
	public static final String LOAD = "load";
	public static final String ID = "id";
	public static final String ACTIVITY = "activity";
	public static final String REBALANCENUMBER = "number";
	public static final String AUTHENTICATEDUSER = "authenticated_user";
	public static final String CONNECTIONRESET = "Connection reset";
	public static final String serverId = String.valueOf(random.nextInt());
	
	public static final String USERINFO = "users";
	public static final String SERVERINFO = "servers";
	public static final String AUTHENTICATE = "AUTHENTICATE";
	public static final String REBALANCE = "REBALANCE";
	
	public static final String AUTHENTICATESUCCESS = "AUTHENTICATE_SUCCESS";
	public static final String AUTHENTICATEFAIL = "AUTHENTICATE_FAIL";
	public static final String INVALIDMESSAGE = "INVALID_MESSAGE";
	public static final String LOGIN = "LOGIN";
	public static final String LOGINFAIL = "LOGIN_FAILED";
	public static final String LOGINSUCCESS = "LOGIN_SUCCESS";
	public static final String LOGOUT = "LOGOUT";
	public static final String REDIRECT = "REDIRECT";
	public static final String REGISTER = "REGISTER";
	public static final String REGISTERFAIL = "REGISTER_FAIL";
	public static final String REGISTERSUCCESS = "REGISTER_SUCCESS";
	public static final String LOCKREQUEST = "LOCK_REQUEST";
	public static final String LOCKALLOWED = "LOCK_ALLOWED";
	public static final String LOCKDENIED = "LOCK_DENIED";
	public static final String ACTIVITYMESSAGE = "ACTIVITY_MESSAGE";
	public static final String SERVERANNOUNCE = "SERVER_ANNOUNCE";
	public static final String ACTIVITYBROADCAST = "ACTIVITY_BROADCAST";
	
	public static final int USERREGDENIED = -1;

	public static int getLocalPort() {
		return localPort;
	}

	public static void setLocalPort(int localPort) {
		if(localPort<0 || localPort>65535){
			log.error("supplied port "+localPort+" is out of range, using "+getLocalPort());
		} else {
			Settings.localPort = localPort;
		}
	}
	
	public static int getRemotePort() {
		return remotePort;
	}

	public static void setRemotePort(int remotePort) {
		if(remotePort<0 || remotePort>65535){
			log.error("supplied port "+remotePort+" is out of range, using "+getRemotePort());
		} else {
			Settings.remotePort = remotePort;
		}
	}
	
	public static String getRemoteHostname() {
		return remoteHostname;
	}

	public static void setRemoteHostname(String remoteHostname) {
		Settings.remoteHostname = remoteHostname;
	}
	
	public static int getActivityInterval() {
		return activityInterval;
	}

	public static void setActivityInterval(int activityInterval) {
		Settings.activityInterval = activityInterval;
	}
	
	public static String getSecret() {
		return secret;
	}

	public static void setSecret(String s) {
		secret = s;
	}
	
	public static String getUsername() {
		return username;
	}

	public static void setUsername(String username) {
		Settings.username = username;
	}
	
	public static String getLocalHostname() {
		return localHostname;
	}

	public static void setLocalHostname(String localHostname) {
		Settings.localHostname = localHostname;
	}

	
	/*
	 * some general helper functions
	 */
	
	public static String socketAddress(Socket socket){
		return socket.getInetAddress()+":"+socket.getPort();
	}

	public static String nextSecret() {
	    return new BigInteger(130, random).toString(32);
	 }



	
}
