package activitystreamer;

import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import activitystreamer.client.ClientSkeleton;
import activitystreamer.util.Settings;

public class Client {
	
	private static final Logger log = LogManager.getLogger();
	
	private static void help(Options options){
		String header = "An ActivityStream Client for Unimelb COMP90015\n\n";
		String footer = "\ncontact aharwood@unimelb.edu.au for issues.";
		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("ActivityStreamer.Client", header, options, footer, true);
		System.exit(-1);
	}
	
	public static void main(String[] args) {
		
		log.info("reading command line options");
		
		Options options = new Options();
		options.addOption("u",true,"username");
		options.addOption("rp",true,"remote port number");
		options.addOption("rh",true,"remote hostname");
		options.addOption("s",true,"secret for username");
		
		
		// build the parser
		CommandLineParser parser = new DefaultParser();
		
		CommandLine cmd = null;
		try {
			cmd = parser.parse( options, args);
		} catch (ParseException e1) {
			help(options);
		}
		
		if(cmd.hasOption("rh")){
			Settings.setRemoteHostname(cmd.getOptionValue("rh"));
		}
		
		if(cmd.hasOption("rp")){
			try{
				int port = Integer.parseInt(cmd.getOptionValue("rp"));
				Settings.setRemotePort(port);
			} catch (NumberFormatException e){
				log.error("-rp requires a port number, parsed: "+cmd.getOptionValue("rp"));
				help(options);
			}
		}
		
		log.info("starting client");

		ClientSkeleton c = ClientSkeleton.getInstance(); 
		
		if(cmd.hasOption("u")){
			Settings.setUsername(cmd.getOptionValue("u"));
			if(cmd.hasOption("s")){
				Settings.setSecret(cmd.getOptionValue("s"));
				if (!c.connect()) {
					log.warn("connection failed");
					System.exit(-1);
				}
				c.login();
			}else {
				int randomNum = ThreadLocalRandom.current().nextInt(1000, 9000);
				Settings.setSecret(String.valueOf(randomNum));
				log.info("a new secret has been generated "+randomNum);
				c.register();
			}
		}else {
			try {
				Settings.setUsername(Settings.ANONYMOUS);
				Settings.setSecret("NO SECRET");
				c.anonymusLogin();
			}catch(NullPointerException e) {
				log.warn("null pointer");
				System.exit(-1);
			}
			
		}
		
	}

	
}
