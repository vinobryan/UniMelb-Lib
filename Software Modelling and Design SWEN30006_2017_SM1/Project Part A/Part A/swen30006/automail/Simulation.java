package automail;

import exceptions.MailAlreadyDeliveredException;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * This class simulates the behavior of AutoMail
 */
public class Simulation {

    /** Constant for the mail generator */
    private static final int MAIL_TO_CREATE = 50;

    private static ArrayList<MailItem> MAIL_DELIVERED;
    private static double total_score = 0;

    public static void main(String[] args){

        MAIL_DELIVERED = new ArrayList<MailItem>();
                
        /** Used to see whether a seed is initialized or not */
        HashMap<Boolean, Integer> seedMap = new HashMap<>();
        
        /** Read the first argument and save it as a seed if it exists */
        if(args.length != 0){
        	int seed = Integer.parseInt(args[0]);
        	seedMap.put(true, seed);
        } else{
        	seedMap.put(false, 0);
        }
        Automail automail = new Automail(new ReportDelivery());
        MailGenerator generator = new MailGenerator(MAIL_TO_CREATE, automail.mailPool, seedMap);
        
        /** Initiate all the mail */
        generator.generateAllMail();

        while(MAIL_DELIVERED.size() != MAIL_TO_CREATE) {
        	// System.out.println("-- Step: "+Clock.Time());
            generator.step();
            automail.robot.step();
            Clock.Tick();
        }
        printResults();
    }
    
    static class ReportDelivery implements IMailDelivery {
    	
    /** Confirm the delivery and calculate the total score */
    public void deliver(MailItem deliveryItem){
        if(!MAIL_DELIVERED.contains(deliveryItem)){
            MAIL_DELIVERED.add(deliveryItem);
            // Calculate delivery score
            total_score += calculateDeliveryScore(deliveryItem);
        }
        else{
            try {
                throw new MailAlreadyDeliveredException();
            } catch (MailAlreadyDeliveredException e) {
                e.printStackTrace();
            }
        }
    }

    }

    private static double calculateDeliveryScore(MailItem deliveryItem) {
    	// Penalty for longer delivery times
    	final double penalty = 1.1;
        // Take (delivery time - arrivalTime)**penalty * priority_weight
        double priority_weight = 0;
        
        // Determine the priority_weight
        switch(deliveryItem.getPriorityLevel()){
            case "LOW":
                priority_weight = 1;
                break;
            case "MEDIUM":
                priority_weight = 1.5;
                break;
            case "HIGH":
                priority_weight = 2;
                break;
        }
        return Math.pow(Clock.Time() - deliveryItem.getArrivalTime(),penalty)*priority_weight;
    }

    public static void printResults(){
        System.out.println("Simulation complete!");
        System.out.println("Final Delivery time: "+Clock.Time());
        System.out.println("Final Score: "+total_score);
    }
}
