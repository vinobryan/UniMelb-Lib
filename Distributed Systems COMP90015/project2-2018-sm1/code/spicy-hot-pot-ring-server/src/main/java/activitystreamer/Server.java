/**
 * COMP90015 Distributed Systems
 * 2018 Semester 1 - Project 2
 * Team Spicy Hot Pot
 * Duer Wang           824325  minghaow1@student.unimelb.edu.au
 * Ivan Ken Weng Chee  736901  ichee@student.unimelb.edu.au
 * Yue Yang            920577  yuey16@student.unimelb.edu.au
 * Ziren Xiao          675485  zirenx@student.unimelb.edu.au
 */

package activitystreamer;

import activitystreamer.server.Control;
import activitystreamer.util.Settings;

import java.net.InetAddress;
import java.net.UnknownHostException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * main server class
 */
public class Server {

  private static final Logger log = LogManager.getLogger();
  private static Options options;

  public static void main(String[] args) {
    log.info("reading command line options");
    initialise(args);
    log.info("starting server");

    final Control control = Control.getInstance();
    control.initiateConnection();
    control.run();

    Runtime.getRuntime().addShutdownHook(new Thread() {
      public void run() {
        control.setTerminate(true);
        interrupt();
      }
    });
  }

  private static void initialise(String[] args) {
    // set server id
    Settings.setId(Settings.nextSecret());

    // construct options
    options = new Options();
    options.addOption("lp", true, "local port number");
    options.addOption("rp", true, "remote port number");
    options.addOption("lh", true, "local hostname");
    options.addOption("rh", true, "remote hostname");
    options.addOption("a", true, "activity interval in milliseconds");
    options.addOption("s", true, "secret for the server to use");

    // build parser
    CommandLineParser parser = new DefaultParser();
    CommandLine cmd = null;
    try {
      cmd = parser.parse(options, args);
    } catch (ParseException e) {
      help(options);
    }

    // local port number
    if (cmd.hasOption("lp")) {
      try {
        int port = Integer.parseInt(cmd.getOptionValue("lp"));
        Settings.setLocalPort(port);
      } catch (NumberFormatException e) {
        log.info("-lp requires a port number, parsed: " + cmd.getOptionValue("lp"));
        help(options);
      }
    }

    // remote port number
    if (cmd.hasOption("rp")) {
      try {
        int port = Integer.parseInt(cmd.getOptionValue("rp"));
        Settings.setRemotePort(port);
      } catch (NumberFormatException e) {
        log.info("-rp requires a port number, parsed: " + cmd.getOptionValue("rp"));
        help(options);
      }
    }

    // local hostname
    try {
      log.info(InetAddress.getLocalHost().getHostAddress());
      Settings.setLocalHostname(InetAddress.getLocalHost().getHostAddress());
    } catch (UnknownHostException e) {
      log.warn("failed to get localhost IP address");
    }
    if (cmd.hasOption("lh")) {
      Settings.setLocalHostname(cmd.getOptionValue("lh"));
    }

    // remote hostname
    if (cmd.hasOption("rh")) {
      if (!cmd.getOptionValue("rh").equals("null")) {
        Settings.setRemoteHostname(cmd.getOptionValue("rh"));
      }
    }

    // activity interval
    if (cmd.hasOption("a")) {
      try {
        int a = Integer.parseInt(cmd.getOptionValue("a"));
        Settings.setActivityInterval(a);
      } catch (NumberFormatException e) {
        log.error("-a requires a number in milliseconds, parsed: " + cmd.getOptionValue("a"));
      }
    }

    // secret
    if (cmd.hasOption("s")) {
      Settings.setSecret(cmd.getOptionValue("s"));
    }
  }

  private static void help(Options options) {
    String header = "distributed ring ActivityStream server\n";
    String footer = "contact ichee@student.unimelb.edu.au for issues";
    HelpFormatter formatter = new HelpFormatter();
    formatter.printHelp("ActivityStreamer.Server", header, options, footer, true);
    System.exit(-1);
  }

}
