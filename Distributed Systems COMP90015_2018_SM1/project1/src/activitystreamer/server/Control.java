package activitystreamer.server;

import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import activitystreamer.util.Settings;

public class Control extends Thread {
	private static final Logger log = LogManager.getLogger();
	private static ArrayList<Connection> connections;
	private static Users users;
	private static UserRegistrationQueue uq;
	private ArrayList<ServerDetails> sd = new ArrayList<ServerDetails>();
	

	private static boolean term=false;
	private static Listener listener;
	private static UIFrame frame = UIFrame.getInstance();

	protected static Control control = null;

	public static Control getInstance() {
		if(control == null){
			control = new Control();
			users = new Users();
			uq = new UserRegistrationQueue();
		}
		return control;
	}

	public Control() {
		// initialize the connections array
		connections = new ArrayList<Connection>();
		
		// start a listener
		try {
			listener = new Listener();
		} catch (IOException e1) {
			frame.setWarn("failed to startup a listening thread: "+e1);
			log.fatal("failed to startup a listening thread: "+e1);
			System.exit(-1);
		}
		// start the control loop making server announce periodically
		start();
	}

	public void initiateConnection(){
		// make a connection to another server if remote hostname is supplied
		if(Settings.getRemoteHostname()!=null){
			try {
				Connection c = outgoingConnection(new Socket(Settings.getRemoteHostname(),Settings.getRemotePort()));
				ServerMessage.sendAuthenticate(c);
				c.setServer();
				connections.add(c);
				frame.setAction("connected to " + Settings.getRemoteHostname() + " at port " + Settings.getRemotePort());
			} catch (IOException e) {
				frame.setWarn("failed to make connection to "+Settings.getRemoteHostname()+":"+Settings.getRemotePort()+" :"+e);
				log.error("failed to make connection to "+Settings.getRemoteHostname()+":"+Settings.getRemotePort()+" :"+e);
				System.exit(-1);
			}
		}
	}

	/*
	 * Processing incoming messages from the connection.
	 * Return true if the connection should close.
	 */
	public synchronized boolean process(Connection con,String msg){
		Message message = new Message();
		if (message.construct(msg)) {
			frame.setMsg(msg, con);
			Response response = message.directToTask(con);
			if (response.getResponse() != null) {
				con.writeMsg(response.getResponse());
			}
			if (response.getSecResponse() != null) {
				con.writeMsg(response.getSecResponse());
			}
			uq.userRegStatusCheck();
			return response.getDisconnect();
		}else {
			con.writeMsg(message.invalidMessage("JSON parse error while parsing message").getResponse());
			frame.setReport("[warn] invalid JSON object received from " + con.getSocket().getRemoteSocketAddress());
			log.warn("[warn] invalid JSON object received from " + con.getSocket().getRemoteSocketAddress());
			return true;
		}
	}

	/*
	 * The connection has been closed by the other party.
	 */
	public synchronized void connectionClosed(Connection con){
		if (con.isServer()) {
			UIFrame.delServer(Settings.socketAddress(con.getSocket()));
		}
		if(!term) connections.remove(con);
	}

	/*
	 * A new incoming connection has been established, and a reference is returned to it
	 */
	public synchronized Connection incomingConnection(Socket s) throws IOException{
		frame.setAction("incomming connection: "+Settings.socketAddress(s));
		log.debug("incomming connection: "+Settings.socketAddress(s));
		Connection c = new Connection(s);
		connections.add(c);
		return c;
	}

	/*
	 * A new outgoing connection has been established, and a reference is returned to it
	 */
	public synchronized Connection outgoingConnection(Socket s) throws IOException{
		frame.setAction("outgoing connection: "+Settings.socketAddress(s));
		log.debug("outgoing connection: "+Settings.socketAddress(s));
		Connection c = new Connection(s);
		return c;
	}

	@Override
	public void run(){
		frame.setAction("using activity interval of "+Settings.getActivityInterval()+" milliseconds");
		log.info("using activity interval of "+Settings.getActivityInterval()+" milliseconds");
		while(!term){
			// do something with 5 second intervals in between
			try {
				Thread.sleep(Settings.getActivityInterval());
			} catch (InterruptedException e) {
				log.info("received an interrupt, system is shutting down");
				break;
			}
			if(!term){
				log.debug("doing activity");
				term=doActivity();
			}

		}
		frame.setAction("closing "+connections.size()+" connections");
		log.info("closing "+connections.size()+" connections");
		// clean up
		for(Connection connection : connections){
			connection.closeCon();
		}
		listener.setTerm(true);
	}

	public boolean doActivity(){
		if (frame.doActivitySelected()) {
			Broadcaster.getInstance().broadcastAnnounce(Settings.getSecret(), getClients().size(), Settings.getLocalHostname(), Settings.getLocalPort());
			frame.setLoad(getClients().size());
		}
		
		uq.userRegStatusCheck();
		return false;
	}

	public final void setTerm(boolean t){
		term=t;
	}

	/** Get all current connections
	 * @return
	 */
	public final ArrayList<Connection> getConnections() {
		return connections;
	}
	
	/** Get all servers
	 * @return
	 */
	public ArrayList<Connection> getServers() {
		ArrayList<Connection> servers = new ArrayList<Connection>();
		for (Connection eachCon:connections) {
			if (eachCon.getType().equals(Settings.SERVER)) {
				servers.add(eachCon);
			}
		}
		return servers;
	}
	
	/** Get all servers except one provided
	 * @param con
	 * @return
	 */
	public static ArrayList<Connection> getServersExcept(Connection con) {
		ArrayList<Connection> servers = new ArrayList<Connection>();

		for (Connection eachCon:connections) {

			if (eachCon.getType().equals(Settings.SERVER)) {
				if (!con.equals(eachCon)) {
					servers.add(eachCon);
				}
			}
		}

		return servers;
	}
	
	/** Get all current clients
	 * @return
	 */
	public ArrayList<Connection> getClients(){
		ArrayList<Connection> clients = new ArrayList<Connection>();
		for(Connection con : connections) {
			if (con.getType().equals(Settings.CLIENT)) {
				clients.add(con);
			}
		}
		return clients;
	}
	
	/** Get all current users
	 * @return
	 */
	public static Users getUsers() {
		return users;
	}
	
	/** Get user registration queue
	 * @return
	 */
	public static UserRegistrationQueue getUq() {
		return uq;
	}
	
	/** Get all server details
	 * @return
	 */
	public ArrayList<ServerDetails> getSd() {
		return sd;
	}
}

