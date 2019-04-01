/**
 * COMP90015 Distributed Systems
 * 2018 Semester 1 - Project 2
 * Team Spicy Hot Pot
 * Duer Wang           824325  minghaow1@student.unimelb.edu.au
 * Ivan Ken Weng Chee  736901  ichee@student.unimelb.edu.au
 * Yue Yang            920577  yuey16@student.unimelb.edu.au
 * Ziren Xiao          675485  zirenx@student.unimelb.edu.au
 */

package activitystreamer.util;

import java.math.BigInteger;
import java.net.Socket;
import java.security.SecureRandom;
import java.util.HashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * settings associated with a server
 */
public class Settings {

	private static final Logger log = LogManager.getLogger();

	private static SecureRandom random = new SecureRandom();

	private static String localHostname = "localhost";
	private static String remoteHostname = null;
	private static String secret = null;
	private static String username = null;
	private static String id = null;

	private static int localPort = 8000;
	private static int remotePort = 3781;
	private static int activityInterval = 5000;

  public static String getId() {
    return id;
  }

  public static void setId(String idNew) {
    id = idNew;
  }

	public static int getLocalPort() {
		return localPort;
	}

	public static void setLocalPort(int localPort) {
		if (localPort < 0 || localPort > 65535) {
			log.error("supplied port " + localPort + " is out of range, using " + getLocalPort());
		} else {
			Settings.localPort = localPort;
		}
	}

	public static int getRemotePort() {
		return remotePort;
	}

	public static void setRemotePort(int remotePort) {
		if (remotePort < 0 || remotePort > 65535){
			log.error("supplied port " + remotePort + " is out of range, using " + getRemotePort());
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
	 * general helper functions
	 */

	public static String socketAddress(Socket socket) {
		return socket.getInetAddress() + ":" + socket.getPort();
	}

	public static String nextSecret() {
	    return new BigInteger(130, random).toString(32);
	 }

}
