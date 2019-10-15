package controller;

import java.util.HashMap;

import com.badlogic.gdx.math.Vector2;

import tiles.MapTile;
import utilities.Coordinate;
import utilities.PeekTuple;
import world.Car;
import world.WorldSpatial;

public abstract class CarController {
	
	private Car car;
	
	/**
	 * Instantiates the car
	 * @param car
	 */
	public CarController(Car car){
		this.car = car;
	}
	
	/**
	 * Slows the car down
	 */
	public void applyBrake(){
		this.car.brake();
	}
	
	/**
	 * Speeds the car up in the forward direction
	 */
	public void applyForwardAcceleration(){
		this.car.applyForwardAcceleration();
	}
	
	/**
	 * Speeds the car up in the backwards direction
	 */
	public void applyReverseAcceleration(){
		this.car.applyReverseAcceleration();
	}
	
	/**
	 * Turns the car left
	 * @param delta refers to the value passed in by the update call
	 */
	public void turnLeft(float delta){
		this.car.turnLeft(delta);
	}
	
	/**
	 * Turns the car right
	 * @param delta refers to the value passed in by the update call
	 */
	public void turnRight(float delta){
		this.car.turnRight(delta);
	}
	
	/**
	 * Retrieves the car's current position
	 */
	public String getPosition(){
		return this.car.getPosition();
	}
	
	/**
	 * Returns the car's current velocity.
	 */
	public float getVelocity(){
		return this.car.getVelocity();
	}
	
	public Vector2 getRawVelocity(){
		return this.car.getRawVelocity();
	}
	
	/**
	 * Returns the car's current angle
	 */
	public float getAngle(){
		return this.car.getAngle();
	}
	
	/**
	 * Returns the car's current health
	 */
	public int getHealth(){
		return this.car.getHealth();
	}
	
	/**
	 * Predict which square you will be on when you turn to a certain degree.
	 * Prediction gets more accurate as you near the actual point.
	 * This projection assumes that the car's velocity is constant and the delta value is
	 * also constant (in reality, the delta value will vary)
	 * 
	 * @param velocity refers to the car's velocity
	 * @param targetDegree refers to what degree you want to be in
	 * @param turnDirection refers to which direction you are turning
	 * @param delta refers to the delta value passed by update.
	 */
	public PeekTuple peek(Vector2 velocity, float targetDegree, WorldSpatial.RelativeDirection turnDirection, float delta){
		return car.peek(velocity, targetDegree, turnDirection, delta);
	}
	
	/**
	 * Returns the view around your car (this is a 3x3 area)
	 */
	public HashMap<Coordinate,MapTile> getView(){
		return car.getView();
	}
	
	/**
	 * Get the distance the car can see
	 */
	public int getViewSquare(){
		return Car.VIEW_SQUARE;
	}
	
	/**
	 * Get the current car orientation (North, West, East or South)
	 */
	public WorldSpatial.Direction getOrientation(){
		return car.getOrientation();
	}
	
	/**
	 * This is the required update step for a vehicle.
	 */
	public abstract void update(float delta);
}
