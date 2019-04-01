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

import java.util.Objects;

/**
 * user data stored in servers
 */
public class User {

  private String username;
  private String secret;
  private String source;

  public User(String username, String secret) {
    this.username = username;
    this.secret = secret;
  }

  public String getUsername() {
    return this.username;
  }

  public String getSecret() {
    return this.secret;
  }

  public String getSource() {
    return this.source;
  }
  public void setSource(String source) {
    this.source = source;
  }

  @Override
  public boolean equals(Object o) {
    if (o == this) return true;
    if (!(o instanceof User)) {
      return false;
    }
    User user = (User) o;
    return(
      Objects.equals(username, user.username)
      && Objects.equals(secret, user.secret)
    );
  }

  @Override
  public int hashCode() {
    return Objects.hash(username, secret);
  }

}
