/**
 * COMP90015 Distributed Systems
 * 2018 Semester 1 - Project 2
 * Team Spicy Hot Pot
 * Duer Wang           824325  minghaow1@student.unimelb.edu.au
 * Ivan Ken Weng Chee  736901  ichee@student.unimelb.edu.au
 * Yue Yang            920577  yuey16@student.unimelb.edu.au
 * Ziren Xiao          675485  zirenx@student.unimelb.edu.au
 */

package activitystreamer.server;

import activitystreamer.util.ActivityStream;
import activitystreamer.util.Settings;
import activitystreamer.util.User;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Timer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * server functionality delegator
 */
public class Control implements Runnable {

  private static final Logger log = LogManager.getLogger();

  private static Listener listener;
  private static Thread listenerWorker;
  private static Boolean terminate = false;
  private static Boolean recovered = false;

  private static HashMap<Connection, Thread> connections;
  private static HashMap<User, Connection> users;
  private static Queue<ActivityStream> messages;
  private static Queue<User> registrations;

  private static String idAnterior = null;
  private static String idPosterior = null;
  private static Connection anterior = null; // Server in front of me
  private static Connection posterior = null; // Server behind me

  private Gson gson;
  private GsonBuilder builder;
  private ActivityStream orbit = null;

  protected static Control control = null;

  public static Control getInstance() {
    if (control == null) {
      control = new Control();
    }
    return control;
  }

  public Control() {
    super();
    connections = new HashMap<Connection, Thread>();
    users = new HashMap<User, Connection>();
    messages = new LinkedList<>();
    registrations = new LinkedList<>();
    builder = new GsonBuilder();
    gson = builder.create();

    // initialise orbit message
    orbit = new ActivityStream();
    orbit.command = "ORBIT";
    orbit.id = Settings.getId();
    ActivityStream server = new ActivityStream();
    server.id = Settings.getId();
    server.load = getLoad();
    server.hostname = Settings.getLocalHostname();
    server.port = Settings.getLocalPort();
    orbit.insertServer(server, null, null);

    try {
      listener = new Listener();
      listenerWorker = new Thread(listener);
      listenerWorker.start();
    } catch (IOException e) {
      log.fatal("failed to start a listening thread: " + e);
      System.exit(-1);
    }
  }

  // make a connection to another server if remote hostname is supplied
  public void initiateConnection() {
    if (Settings.getRemoteHostname() != null) {
      // not first server
      try {
        anterior = outgoingConnection(new Socket(Settings.getRemoteHostname(), Settings.getRemotePort()));
        ActivityStream authenticate = new ActivityStream();
        authenticate.command = "AUTHENTICATE";
        authenticate.secret = Settings.getSecret();
        authenticate.hostname = Settings.getLocalHostname();
        authenticate.port = Settings.getLocalPort();
        authenticate.id = Settings.getId();
        anterior.writeMsg(gson.toJson(authenticate));
      } catch (IOException e) {
        log.error("failed to make connection to " + Settings.getRemoteHostname() + ":" + Settings.getRemotePort() + ": " + e);
        System.exit(-1);
      }
    }
  }

  // the connection has been closed by the other party
  public synchronized void connectionClosed(Connection connection) {
    if (!terminate) {
      Thread worker = connections.get(connection);
      if (worker != null) worker.interrupt();
      connections.remove(connection);
      if (connection == anterior) {
        log.debug("anterior closed");
        idAnterior = anterior.getId();
        orbit.servers.remove(orbit.getServer(anterior.getId()));
        String newAnteriorHostname = orbit.getNextAnterior(Settings.getId()).hostname;
        Integer newAnteriorPort = orbit.getNextAnterior(Settings.getId()).port;
        if (!(newAnteriorHostname.equals(Settings.getLocalHostname()) && newAnteriorPort.equals(Settings.getLocalPort()))) {
          try {
            anterior = outgoingConnection(new Socket(newAnteriorHostname, newAnteriorPort));
            ActivityStream authenticate = new ActivityStream();
            authenticate.command = "AUTHENTICATE";
            authenticate.secret = Settings.getSecret();
            authenticate.hostname = Settings.getLocalHostname();
            authenticate.port = Settings.getLocalPort();
            authenticate.id = Settings.getId();
            anterior.writeMsg(gson.toJson(authenticate));
            recovered = true;
          } catch (IOException e) {
            log.error("failed to make connection to " + Settings.getRemoteHostname() + ":" + Settings.getRemotePort() + ": " + e);
            System.exit(-1);
          }
        }
      }
      if (connection == posterior) {
        idPosterior = posterior.getId();
        log.debug("posterior closed");
        posterior = null;
      }
    }
  }

  // a new incoming connection has been established, and a reference is returned to it
  public synchronized Connection incomingConnection(Socket socket) throws IOException {
    log.debug("incoming connection: " + Settings.socketAddress(socket));
    Connection connection = new Connection(socket);
    Thread worker = new Thread(connection);
    worker.start();
    connections.put(connection, worker);
    return connection;
  }

  // a new outgoing connection has been established, and a reference is returned to it
  public synchronized Connection outgoingConnection(Socket socket) throws IOException {
    log.debug("outgoing connection: " + Settings.socketAddress(socket));
    Connection connection = new Connection(socket);
    Thread worker = new Thread(connection);
    worker.start();
    connections.put(connection, worker);
    return connection;
  }

  // processes incoming messages from the connection
  // returns true if the conneciton should close
  public synchronized Boolean process(Connection connection, String received) {
    if (received == null) {
      ActivityStream invalidMessage = new ActivityStream();
      invalidMessage.command = "INVALID_MESSAGE";
      invalidMessage.info = "JSON parse error while parsing message";
			connection.writeMsg(gson.toJson(invalidMessage));
			connection.closeCon();
			if (connections.containsKey(connection)) connections.remove(connection);
		} else {
      ActivityStream as = gson.fromJson(received, ActivityStream.class);
      log.info(as);
      switch (as.command) {
        case "ACTIVITY_MESSAGE":
          as.activity.source = Settings.getId();
          messages.add(as.activity);
          break;
        case "AUTHENTICATE":
          if (as.secret.equals(Settings.getSecret())) {
            // correct secret
            connection.setHostname(as.hostname);
            connection.setPort(as.port);
            connection.setId(as.id);

            if (posterior != null) {
              // nth server
              ActivityStream serverRedirect = new ActivityStream();
              serverRedirect.command = "SERVER_REDIRECT";
              serverRedirect.hostname = connection.getHostname();
              serverRedirect.port = connection.getPort();
              serverRedirect.id = connection.getId();

              // tell posterior to join up with new server
              posterior.writeMsg(gson.toJson(serverRedirect));

              // set new server as posterior
              ActivityStream authenticationSuccess = new ActivityStream();
              authenticationSuccess.command = "AUTHENTICATION_SUCCESS";
              authenticationSuccess.id = Settings.getId();
              connection.writeMsg(gson.toJson(authenticationSuccess));
              connection.setServer();
              posterior = connection;
              posterior.setId(as.id);

              // sync data
              ActivityStream dataSync = new ActivityStream();
              dataSync.command = "DATA_SYNC";
              for (User user: users.keySet()) {
                dataSync.users.add(user);
              }
              posterior.writeMsg(gson.toJson(dataSync));

            } else {
              // first or second server
              connection.setServer();
              posterior = connection;
              posterior.setId(as.id);
              ActivityStream authenticationSuccess = new ActivityStream();
              authenticationSuccess.command = "AUTHENTICATION_SUCCESS";
              authenticationSuccess.id = Settings.getId();
              posterior.writeMsg(gson.toJson(authenticationSuccess));
              if (!(Settings.getRemoteHostname() != null)) {
                // first server
                anterior = connection;
                anterior.setId(as.id);
                log.debug("initiating orbit");
                anterior.writeMsg(gson.toJson(orbit));
              }
            }
          } else {
            // wrong secret
            ActivityStream authenticationFail = new ActivityStream();
            authenticationFail.command = "AUTHENTICATION_FAIL";
            authenticationFail.info = "the supplied secret is incorrect: " + as.secret;
            connection.writeMsg(gson.toJson(authenticationFail));
      			connection.closeCon();
      			if (connections.containsKey(connection)) connections.remove(connection);
          }
          break;
        case "AUTHENTICATION_FAIL":
          break;
        case "AUTHENTICATION_SUCCESS":
          anterior = connection;
          anterior.setId(as.id);
          if (recovered) {
            anterior.writeMsg(gson.toJson(orbit));
            recovered = false;
          }
          break;
        case "DATA_SYNC":
          for (User user: as.users) {
            users.put(user, null);
          }
          break;
        case "LOCK_DENIED":
          if (as.user.getSource().equals(Settings.getId())) {
            // my client
            ActivityStream registerFail = new ActivityStream();
            registerFail.command = "REGISTER_FAIL";
            registerFail.info = as.user.getUsername() + " is already registered with the system";
            if (users.containsKey(as.user)) {
              users.get(as.user).writeMsg(gson.toJson(registerFail));
              users.get(as.user).closeCon();
              if (connections.containsKey(connection)) connections.remove(connection);
              users.remove(as.user);
            }
          } else {
            // continue backpropagation
            if (users.containsKey(as.user)) {
              users.remove(as.user);
            }
            posterior.writeMsg(gson.toJson(as));
          }
          break;
        case "LOGIN":
          User user1 = new User(as.username, as.secret);
          if (users.containsKey(user1)) {
            if (users.get(user1) != null) {
            } else {
              users.put(user1, connection);
            }
            users.get(user1).setClient();
            ActivityStream loginSuccess = new ActivityStream();
            loginSuccess.command = "LOGIN_SUCCESS";
            loginSuccess.info = "logged in as user " + as.username;
            connection.writeMsg(gson.toJson(loginSuccess));
            connection.setClient();

            // client redirect
            for (ActivityStream server: orbit.servers) {
              if ((getLoad() - server.load) > 1) {
                ActivityStream redirect = new ActivityStream();
                redirect.command = "REDIRECT";
                redirect.hostname = server.hostname;
                redirect.port = server.port;
                connection.writeMsg(gson.toJson(redirect));
                connection.closeCon();
                if (connections.containsKey(connection)) connections.remove(connection);
                break;
              }
            }
          } else {
            ActivityStream loginFailed = new ActivityStream();
            loginFailed.command = "LOGIN_FAILED";
            loginFailed.info = "attempt to login with wrong secret";
            connection.writeMsg(gson.toJson(loginFailed));
            users.get(user1).closeCon();
            if (connections.containsKey(connection)) connections.remove(connection);
          }
          break;
        case "LOGOUT":
          connection.closeCon();
          if (connections.containsKey(connection)) connections.remove(connection);
          break;
        case "ORBIT":
          // for second server only
          if (posterior == null) {
            posterior = connection;
            posterior.setId(as.id);
          }

          // uncomment this block for full performance
          try {
            Thread.sleep(Settings.getActivityInterval());
          } catch (InterruptedException e) {
            log.info("received an interrupt, system is shutting down");
            break;
          }

          // change id to my id
          as.id = Settings.getId();

          // add myself to the server list if not present, else update my entry
          ActivityStream me = as.getServer(Settings.getId());
          if (me != null) {
            as.servers.get(as.servers.indexOf(me)).load = getLoad();
          } else {
            me = new ActivityStream();
            me.id = Settings.getId();
            me.load = getLoad();
            me.hostname = Settings.getLocalHostname();
            me.port = Settings.getLocalPort();
            as.insertServer(me, anterior.getId(), posterior.getId());
          }

          // redirect a client to posterior if it has less load than I do
          ActivityStream pos = as.getServer(posterior.getId());
          if (pos != null) {
            if ((getLoad() - pos.load) > 0) {
              ActivityStream redirect = new ActivityStream();
              redirect.command = "REDIRECT";
              redirect.hostname = pos.hostname;
              redirect.port = pos.port;
              for (Connection c: connections.keySet()) {
                c.writeMsg(gson.toJson(redirect));
                c.closeCon();
                if (connections.containsKey(c)) connections.remove(c);
                break;
              }
            }
          }

          // remove messages added by me during the last orbit
          List<ActivityStream> broadcasted = new ArrayList<ActivityStream>();
          for (ActivityStream message: as.messages) {
            if (message.source.equals(Settings.getId())) {
              broadcasted.add(message);
            } else {
              broadcast(message);
            }
          }
          as.messages.removeAll(broadcasted);

          // add new messages i have to queue
          as.messages.addAll(messages);
          messages.clear();

          // remove lock requests added by me during the last orbit
          List<User> registered = new ArrayList<User>();
          for (User user: as.registrations) {
            if (user.getSource().equals(Settings.getId())) {
              ActivityStream registerSuccess = new ActivityStream();
              registerSuccess.command = "REGISTER_SUCCESS";
              registerSuccess.info = "register success for " + user.getUsername();
              users.get(user).writeMsg(gson.toJson(registerSuccess));
              registered.add(user);
            } else {
              if (users.containsKey(user)) {
                // i already know of this user
                registered.add(user);
                ActivityStream response = new ActivityStream();
                response.command = "LOCK_DENIED";
                response.user = user;
                posterior.writeMsg(gson.toJson(response));
              } else {
                users.put(user, null);
              }
            }
          }
          as.registrations.removeAll(registered);

          // add new lock requests i have to queue
          as.registrations.addAll(registrations);
          registrations.clear();

          // forward orbit message
          if (anterior != null) {
            anterior.writeMsg(gson.toJson(as));
            orbit = as;
          }
          break;
        case "REGISTER":
          User user = new User(as.username, as.secret);
          if (users.containsKey(user)) {
            // User exists
            ActivityStream registerFailed = new ActivityStream();
            registerFailed.command = "REGISTER_FAILED";
            registerFailed.info = user.getUsername() + " is already registered with the system";
            connection.writeMsg(gson.toJson(registerFailed));
            connection.closeCon();
            if (connections.containsKey(connection)) connections.remove(connection);
          } else {
            // User does not exist
            user.setSource(Settings.getId());
            users.put(user, connection);
            registrations.add(user);
          }
          break;
        case "SERVER_REDIRECT":
          try {
            anterior = outgoingConnection(new Socket(as.hostname, as.port));
            ActivityStream authenticate = new ActivityStream();
            authenticate.command = "AUTHENTICATE";
            authenticate.secret = Settings.getSecret();
            authenticate.hostname = Settings.getLocalHostname();
            authenticate.port = Settings.getLocalPort();
            anterior.writeMsg(gson.toJson(authenticate));
          } catch (IOException e) {
            log.error("failed to make connection to " + Settings.getRemoteHostname() + ":" + Settings.getRemotePort() + ": " + e);
            System.exit(-1);
          }
          break;
      }
    }
    return false;
  }

  private synchronized void broadcast(ActivityStream message) {
    ActivityStream msg = new ActivityStream();
    msg.command = "ACTIVITY_BROADCAST";
    msg.info = message.info;
    for (Connection connection: connections.keySet()) {
      if (connection.getType().equals("CLIENT")) {
        connection.writeMsg(gson.toJson(msg));
      }
    }
  }

  private Integer getLoad() {
    Integer load = 0;
    for (Connection connection: connections.keySet()) {
      if (connection.getType().equals("CLIENT")) {
        load += 1;
      }
    }
    return load;
  }

  public void setTerminate(Boolean t) {
    terminate = t;
    listener.setTerminate(t);
  }

  @Override
  public void run() {
    log.info("using activity interval of " + Settings.getActivityInterval() + " milliseconds");
    while (!terminate) {

    }
    log.info("closing " + connections.size() + " connections");
    for (Connection connection: connections.keySet()) {
      connection.closeCon();
    }
    listenerWorker.interrupt();
  }

}
