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

import activitystreamer.util.Settings;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * listens for incomming and outgoing connections
 */
public class Listener implements Runnable {

  private static final Logger log = LogManager.getLogger();

  private ServerSocket serverSocket = null;
  private Boolean terminate = false;
  private int port;

  public Listener() throws IOException {
    super();
    port = Settings.getLocalPort();
    serverSocket = new ServerSocket(port);
  }

  @Override
  public void run() {
    log.info("listening for new connections on " + port);
    while (!terminate) {
      Socket clientSocket;
      try {
        clientSocket = serverSocket.accept();
        Control.getInstance().incomingConnection(clientSocket);
      } catch (IOException e) {
        log.info("received exception, shutting down");
        terminate = true;
      }
    }
  }

  public void setTerminate(Boolean terminate) {
    this.terminate = terminate;
  }

}
