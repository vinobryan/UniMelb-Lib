package activitystreamer.server;


import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import activitystreamer.util.Settings;


public class Connection extends Thread {
	private static final Logger log = LogManager.getLogger();
	private DataInputStream in;
	private DataOutputStream out;
	private BufferedReader inreader;
	private PrintWriter outwriter;
	private boolean open = false;
	private Socket socket;
	private boolean term=false;
	private UIFrame frame = UIFrame.getInstance();
	private String type = Settings.UNDEFINED;
	private SingleUser user = null;
	
	public Connection(Socket socket) throws IOException{
		in = new DataInputStream(socket.getInputStream());
	    out = new DataOutputStream(socket.getOutputStream());
	    inreader = new BufferedReader( new InputStreamReader(in));
	    outwriter = new PrintWriter(out, true);
	    this.socket = socket;
	    open = true;
	    start();
	}
	
	public SingleUser getUser() {
		return user;
	}

	public void setUser(SingleUser user) {
		this.user = user;
	}

	/**
	 * returns true if the message was written, otherwise false
	 */
	public boolean writeMsg(String msg) {
		if(open){
			outwriter.println(msg);
			outwriter.flush();
			frame.setSentMsg(msg);
			return true;	
		}
		return false;
	}
	
	public boolean isServer() {
		if (type.equals(Settings.SERVER)) {
			return true;
		}
		return false;
	}
	
	public void setServer() {
		UIFrame.addServer(Settings.socketAddress(this.socket));
		type = Settings.SERVER;
	}
	
	public void setClient() {
		type = Settings.CLIENT;
	}
	
	public String getType() {
		return type;
	}
	
	public void closeCon(){
		if(open){
			frame.setAction("closing connection "+Settings.socketAddress(socket));
			log.info("closing connection "+Settings.socketAddress(socket));
			try {
				term=true;
				inreader.close();
				out.close();
			} catch (IOException e) {
				// already closed?
				frame.setWarn("received exception closing the connection "+Settings.socketAddress(socket)+": "+e);
				log.error("received exception closing the connection "+Settings.socketAddress(socket)+": "+e);
			}
		}
	}
	
	
	public void run(){
		try {
			String data;
			while(!term && (data = inreader.readLine())!=null){
				term=Control.getInstance().process(this,data);
			}
			frame.setAction("connection closed to "+Settings.socketAddress(socket));
			log.debug("connection closed to "+Settings.socketAddress(socket));
			Control.getInstance().connectionClosed(this);
			in.close();
		} catch (IOException e) {
			frame.setAction("connection "+Settings.socketAddress(socket)+" closed with exception: "+e);
			log.error("connection "+Settings.socketAddress(socket)+" closed with exception: "+e);
			Control.getInstance().connectionClosed(this);
		}
		open=false;
	}
	
	public Socket getSocket() {
		return socket;
	}
	
	public boolean isOpen() {
		return open;
	}
}
