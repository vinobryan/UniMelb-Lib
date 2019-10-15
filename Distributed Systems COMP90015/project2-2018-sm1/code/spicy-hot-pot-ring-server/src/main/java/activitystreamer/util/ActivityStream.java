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

import java.util.ArrayList;
import java.util.List;

/**
 * ActivityStream message
 */
public class ActivityStream {

  public List<ActivityStream> servers;
  public List<ActivityStream> messages;
  public List<User> registrations;
  public List<User> users;

  public ActivityStream activity;
  public User user;

  public String authenticated_user;
  public String command;
  public String hostname;
  public String id;
  public String info;
  public String secret;
  public String source;
  public String username;

  public Integer load;
  public Integer port;

  public ActivityStream() {
    servers = new ArrayList<ActivityStream>();
    messages = new ArrayList<ActivityStream>();
    registrations = new ArrayList<User>();
    users = new ArrayList<User>();
  }

  public ActivityStream getServer(String id) {
    for (ActivityStream server: servers) {
      if (server.id.equals(id)) {
        return server;
      }
    }
    return null;
  }

  public void insertServer(ActivityStream server, String idAnterior, String idPosterior) {
    ActivityStream anterior = getServer(idAnterior);
    ActivityStream posterior = getServer(idPosterior);
    if (anterior != null && posterior != null) {
      // both exist
      this.servers.add(this.servers.indexOf(anterior) + 1, server);
    } else if (anterior != null) {
      // no posterior
      this.servers.add(this.servers.indexOf(anterior) + 1, server);
    } else if (posterior != null) {
      // no anterior
      this.servers.add(this.servers.indexOf(posterior), server);
    } else {
      // first server
      this.servers.add(server);
    }
  }

  public ActivityStream getNextAnterior(String id) {
    Integer index = this.servers.indexOf(getServer(id));
    if (index > 0) {
      return this.servers.get(index - 1);
    } else {
      return this.servers.get(this.servers.size() - 1);
    }
  }

  @Override
  public String toString() {
    return("{"
      + ((activity != null) ? "activity: " + activity.toString() : "")
      + ((authenticated_user != null) ? "| authenticated_user: " + authenticated_user : "")
      + ((command != null) ? "| command: " + command : "")
      + ((hostname != null) ? "| hostname: " + hostname : "")
      + ((id != null) ? "| id: " + id : "")
      + ((info != null) ? "| info: " + info : "")
      + ((load != null) ? "| load: " + Integer.toString(load) : "")
      + ((messages.size() > 0) ? "| messages: [" + toStringList(messages) + "]" : "")
      + ((port != null) ? "| port: " + Integer.toString(port) : "")
      + ((registrations.size() > 0) ? "| registrations: [" + registrations.toString() + "]" : "")
      + ((secret != null) ? "| secret: " + secret : "")
      + ((source != null) ? "| source: " + source : "")
      + ((servers.size() > 0) ? "| servers: [" + toStringList(servers) + "]" : "")
      + ((user != null) ? "| user: " + user : "")
      + ((username != null) ? "| username: " + username : "")
      + "}");
  }

  private String toStringList(List<ActivityStream> list) {
    String str = "";
    for (ActivityStream as: list) {
      str += as.toString();
    }
    return str;
  }

}
