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

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * TCP socket connection
 */
public class Connection implements Runnable {

  private static final Logger log = LogManager.getLogger();
  private Socket socket;
  private DataInputStream in;
  private DataOutputStream out;
  private BufferedReader inreader;
  private PrintWriter outwriter;
  private Boolean open = false;
  private Boolean terminate = false;

  private String type = "UNDEFINED";
  private String identity = null;
  private String hostname = null;
  private Integer port = 0;

  private String username = null;
  private String secret = null;
  private String id = null;

  public Connection(Socket socket) throws IOException {
    in = new DataInputStream(socket.getInputStream());
    out = new DataOutputStream(socket.getOutputStream());
    inreader = new BufferedReader(new InputStreamReader(in));
    outwriter = new PrintWriter(out, true);
    this.socket = socket;
    open = true;
  }

  // returns true if the message was writter, otherwise false
  public boolean writeMsg(String msg) {
    if (open) {
      outwriter.println(msg);
      outwriter.flush();
      return true;
    }
    return false;
  }

  public void closeCon() {
    if (open) {
      log.info("closing connection " + Settings.socketAddress(socket));
      try {
        terminate = true;
        inreader.close();
        out.close();
      } catch (IOException e) {
        log.error("received exception closing the connection " + Settings.socketAddress(socket) + ": " + e);
      }
    }
  }

  public String getType() {
    return this.type;
  }

  public void setClient() {
    this.type = "CLIENT";
  }

  public void setServer() {
    this.type = "SERVER";
  }

  public String getHostname() {
    return this.hostname;
  }
  public void setHostname(String hostname) {
    this.hostname = hostname;
  }

  public Integer getPort() {
    return this.port;
  }
  public void setPort(Integer port) {
    this.port = port;
  }

  public String getId() {
    return this.id;
  }
  public void setId(String id) {
    this.id = id;
  }

  public void setTerminate(Boolean terminate) {
    this.terminate = terminate;
  }

  @Override
  public void run() {
    try {
      String data;
      while (!terminate && (data = inreader.readLine()) != null) {
        terminate = Control.getInstance().process(this, data);
      }
      log.debug("connection closed to " + Settings.socketAddress(socket));
      Control.getInstance().connectionClosed(this);
      in.close();
    } catch (IOException e) {
      log.error("connection " + Settings.socketAddress(socket) + " closed with exception: " + e);
      Control.getInstance().connectionClosed(this);
    }
    open = false;
  }

}
