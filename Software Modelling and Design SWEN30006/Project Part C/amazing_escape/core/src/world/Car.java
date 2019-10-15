package world;

import java.util.ArrayList;
import java.util.HashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.g2d.Sprite;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer;
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer.Cell;
import com.badlogic.gdx.math.Vector2;
import com.swen30006.driving.Simulation;

import exceptions.NoStartLocationException;
import tiles.GrassTrap;
import tiles.MapTile;
import tiles.MudTrap;
import tiles.TrapTile;
import tiles.UtilityTile;
import utilities.Coordinate;
import utilities.PeekTuple;
import world.WorldSpatial.Direction;

/**
 * This class provides functionality for use within the simulation system. It is NOT intended to be
 * read or understood for SWEN30006 Part C. Comments have been intentionally removed to reinforce
 * this. We take no responsibility if you use all your time trying to understand this code.
 *
 */
public class Car extends Sprite{


	// Logger
	private static Logger logger = LogManager.getLogger();

	private Direction currentOrientation;


	private Vector2 velocity;
	private float angle = 0;
	private float rotation = 0;
	private boolean reversing = false;
	private boolean accelerating = false;
	private boolean handBrake = false;
	private boolean wasReversing = false;



	private static final float MAX_SPEED = 5f;
	private static final float MAX_REVERSE_SPEED = 2.5f;
	private static final float ROTATING_FACTOR = 150f;
	private static final float STEERING_ADJUST_FACTOR = 0.05f;
	private static final float EPSILON = 0.01f;
	private static final float TURN_EPSILON = 0.05f;
	private static final float BRAKING_FORCE = 2f;
	private static final float ACCELERATION = 2f;
	private static final float MAX_DEGREES = 360;
	private static final float FRICTION_FORCE = 0.5f;
	private static final int SNAP_THRESHOLD = 5;
	public static final int VIEW_SQUARE = 3;



	private static enum State { FORWARD, REVERSE };
	private static State carDirection = State.FORWARD;


	private static int CAR_WIDTH;
	private static int CAR_HEIGHT;

	private int health;
	private static final int INITIAL_HEALTH = 100;

	ArrayList<Integer> startLocation;

	public Car(Sprite sprite){
		super(sprite);

		health = INITIAL_HEALTH;
		velocity = new Vector2();

		startLocation = getStartPosition();

		if(startLocation.size() > 0){
			setX(startLocation.get(0));
			setY(startLocation.get(1));
		}
		else{
			try {
				throw new NoStartLocationException();
			} catch (NoStartLocationException e) {
				e.printStackTrace();
			}
		}

		CAR_WIDTH = (int) sprite.getWidth();
		CAR_HEIGHT = (int) sprite.getHeight();

		this.currentOrientation = WorldSpatial.Direction.EAST;

	}


	public void update(float delta) {
		if(Simulation.DEBUG_MODE){
			printDebug();
		}
		checkHealth();

		// Get the current tile
		MapTile currentTile = World.lookUp(getX(), getY());
		if(currentTile.getName().equals("Utility")){

			if(((UtilityTile) currentTile).isExit()){

				Simulation.winGame();
			}
		}
		if((this.velocity.len() < EPSILON) && (World.lookUp(getX(), getY()) instanceof MudTrap)){
			System.out.println("STUCK IN MUD. GAME OVER. ESCAPE FAILED!!");
			System.out.println("Time elapsed: " + ((System.currentTimeMillis() - Simulation.startTime) / 1000+" seconds!"));
			Gdx.app.exit();
		}
		// First calculate the force created by the engine, that is either accelerating or reversing
		float drivingForce = 0;
		if(accelerating || reversing){
			drivingForce = ACCELERATION;
		}

		// Calculate the braking force, if not braking apply a small amount of friction so we slow down over
		// time, given this is negligible compared to braking we do one or, not both
		float frictionForce = 0;
		if(handBrake && (this.velocity.len() > EPSILON)){
			frictionForce = BRAKING_FORCE;
		} else if (this.velocity.len() > 0){
			frictionForce = FRICTION_FORCE;
		}

		// Check if you are standing on a trap!
		checkTrap(currentTile,delta);
		applySteering();

		// Calculate acceleration
		Vector2 netAcceleration = calculateAcceleration(drivingForce, frictionForce);

		// Apply the acceleration to velocity
		applyAcceleration(netAcceleration, delta);

		setPosition(velocity, delta);

		setRotation(rotation);

		resetControls();
	}

	private void checkHealth() {
		if(health <= 0){
			System.out.println("NO HEALTH. GAME OVER. ESCAPE FAILED!!");
			System.out.println("Time elapsed: " + ((System.currentTimeMillis() - Simulation.startTime) / 1000+" seconds!"));
			Gdx.app.exit();
		}
	}

	public void reduceHealth(float damage) {
		health -= damage;
	}

	public void applyForwardAcceleration(){
		// Can't accelerate if you are on mud!
		if(!(World.lookUp(getX(), getY()) instanceof MudTrap)){
			accelerating = true;
		}

	}

	public void applyReverseAcceleration(){
		// Can't reverse if you are on mud!
		if(!(World.lookUp(getX(), getY()) instanceof MudTrap)){
			reversing = true;
			wasReversing = true;
		}

	}

	public void brake(){
		handBrake = true;
	}


	/** Snap to an orientation if you get close to it! **/
	public void turnLeft(float delta){

		angle += ROTATING_FACTOR * delta;
		if(reversing){
			angle *= -1;
		}
		snapTo(reversing, currentOrientation, WorldSpatial.RelativeDirection.LEFT);

	}

	private void snapTo(boolean reversing, WorldSpatial.Direction currentOrientation, WorldSpatial.RelativeDirection turnDirection) {
		float angleDifference = SNAP_THRESHOLD;
		if((!reversing && turnDirection.equals(WorldSpatial.RelativeDirection.LEFT)) || (reversing && turnDirection.equals(WorldSpatial.RelativeDirection.RIGHT))){

			switch(currentOrientation){
			case EAST:
				angleDifference = WorldSpatial.NORTH_DEGREE - getAngle();
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = angleDifference;
					this.currentOrientation = WorldSpatial.Direction.NORTH;
				}
				break;
			case NORTH:
				angleDifference = WorldSpatial.WEST_DEGREE - getAngle();
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = angleDifference;
					this.currentOrientation = WorldSpatial.Direction.WEST;
				}
				break;
			case SOUTH:
				angleDifference = WorldSpatial.EAST_DEGREE_MAX - getAngle();
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = angleDifference;
					this.currentOrientation = WorldSpatial.Direction.EAST;
				}
				break;
			case WEST:
				angleDifference = WorldSpatial.SOUTH_DEGREE - getAngle();
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = angleDifference;
					this.currentOrientation = WorldSpatial.Direction.SOUTH;
				}
				break;
			default:
				break;

			}

		}
		else if((!reversing && turnDirection.equals(WorldSpatial.RelativeDirection.RIGHT)) || (reversing && turnDirection.equals(WorldSpatial.RelativeDirection.LEFT))){
			switch(currentOrientation){
			case EAST:
				angleDifference = getAngle() - WorldSpatial.SOUTH_DEGREE;
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = -angleDifference;
					this.currentOrientation = WorldSpatial.Direction.SOUTH;
				}
				break;
			case NORTH:
				angleDifference = getAngle();
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = -angleDifference;
					this.currentOrientation = WorldSpatial.Direction.EAST;
				}
				break;
			case SOUTH:
				angleDifference = getAngle() - WorldSpatial.WEST_DEGREE;
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = -angleDifference;
					this.currentOrientation = WorldSpatial.Direction.WEST;
				}
				break;
			case WEST:
				angleDifference = getAngle() - WorldSpatial.NORTH_DEGREE;
				if(angleDifference >= 0 && angleDifference < SNAP_THRESHOLD){
					angle = -angleDifference;
					this.currentOrientation = WorldSpatial.Direction.NORTH;
				}
				break;
			default:
				break;

			}
		}

	}


	public void turnRight(float delta) {
		angle -= ROTATING_FACTOR * delta;

		if(reversing){
			angle *= -1;
		}
		snapTo(reversing,currentOrientation,WorldSpatial.RelativeDirection.RIGHT);

	}

	private void applySteering(){


		if(velocity.len() > TURN_EPSILON && !handBrake && !(World.lookUp(getX(), getY()) instanceof GrassTrap)) {

			rotation += angle;

			if(angle > TURN_EPSILON || angle < TURN_EPSILON ){
				// Slowly update turnings
				int sign = angle > 0 ? 1 : -1;
				float magnitude = Math.abs(angle) - Math.abs(angle)*STEERING_ADJUST_FACTOR;
				angle = magnitude * sign;
			} else {
				angle = 0;
			}
		}
	}

	private Vector2 calculateAcceleration(float drivingForce, float frictionForce){

		Vector2 acceleration = new Vector2(1,0);
		acceleration.rotate(rotation);
		acceleration.scl(drivingForce);


		Vector2 friction = new Vector2(1,0);
		if(acceleration.len() > 0){

			friction.rotate(acceleration.angle() - MAX_DEGREES/2);
		} else {

			friction.rotate((rotation - MAX_DEGREES/2) % MAX_DEGREES);
		}
		friction.scl(frictionForce);



		Vector2 netAcceleration = acceleration.add(friction);
		return netAcceleration;
	}

	private void applyAcceleration(Vector2 acceleration, float delta){


		this.velocity.setAngle(rotation);

		if((carDirection.equals(State.REVERSE) && accelerating) || (carDirection.equals(State.FORWARD) && reversing)){

			this.velocity.x -= acceleration.x * delta;
			this.velocity.y -= acceleration.y * delta;
		}
		else{
			this.velocity.x += acceleration.x * delta;
			this.velocity.y += acceleration.y * delta;
		}



		if(this.velocity.len() > MAX_SPEED && !reversing) {
			float scalar = this.velocity.len() / MAX_SPEED;
			this.velocity.scl(1/scalar);
		}
		else if(this.velocity.len() > MAX_REVERSE_SPEED && reversing){
			float scalar = this.velocity.len() / MAX_REVERSE_SPEED;
			this.velocity.scl(1/scalar);
		}
		else if (this.velocity.len() < EPSILON){
			this.velocity.x = 0;
			this.velocity.y = 0;
			if(carDirection.equals(State.FORWARD)){
				carDirection = State.REVERSE;
			}
			else{
				carDirection = State.FORWARD;
			}
			if(wasReversing){
				wasReversing = false;
			}
		}
		if(carDirection.equals(State.REVERSE)){

			velocity.rotate(180);
		}
	}

	private void setPosition(Vector2 velocity, float delta){
		double xOffset = ((CAR_WIDTH / 2)*1.0/World.MAP_PIXEL_SIZE);
		double yOffset = ((CAR_HEIGHT/2)*1.0/World.MAP_PIXEL_SIZE);

		double futureX = getX() + velocity.x * delta;
		double futureY = getY() + velocity.y * delta;
		if(velocity.x > 0){
			futureX += xOffset;
		}
		if(velocity.x < 0){
			futureX -= xOffset;
		}

		if(velocity.y > 0){
			futureY += yOffset;
		}
		else{
			futureY -= xOffset;
		}
		MapTile tile = World.lookUp(futureX, futureY);

		if(!tile.getName().equals("Empty") && !tile.getName().equals("Wall")){

			setX(getX() + velocity.x * delta);
			setY(getY() + velocity.y * delta);
		}
		else{
			velocity.x = 0;
			velocity.y = 0;
			reduceHealth(5 * delta);
		}

	}

	private void resetControls(){
		angle = 0;
		reversing = false;
		accelerating = false;
		handBrake = false;
	}

	public void draw(SpriteBatch spriteBatch){
		update(Gdx.graphics.getDeltaTime());
	}

	private ArrayList<Integer> getStartPosition(){

		ArrayList<Integer> startLocation = new ArrayList<Integer>();

		TiledMapTileLayer utilityLayer = (TiledMapTileLayer) World.getMap().getLayers().get("Utility");


		for(int x = 0; x < utilityLayer.getWidth(); x++){
			for(int y = 0; y < utilityLayer.getHeight(); y++){
				Cell utilityCell = utilityLayer.getCell(x, y);
				if(utilityCell != null && utilityCell.getTile().getProperties().get("startLocation") != null){
					if((Boolean) utilityCell.getTile().getProperties().get("startLocation")){
						startLocation.add(x);
						startLocation.add(y);
						break;
					}
				}

			}
		}
		return startLocation;
	}


	public void checkTrap(MapTile currentTile, float delta){

		if(currentTile.getName().equals("Trap")){
			TrapTile trapTile = (TrapTile) currentTile;
			trapTile.applyTo(this, delta);
		}
	}

	public void setVelocity(float x, float y) { /* Better if this wasn't public but needed in traps */
		velocity.x = x;
		velocity.y = y;
	}


	public void setVelocity(Vector2 scl) {
		this.velocity = scl;

	}
	
	public float normalizeAngle(float angle){
		float calculatedAngle = angle % 360;
		calculatedAngle = (calculatedAngle + 360) % 360;
		if(calculatedAngle > 180){
			calculatedAngle -= 360;
		}
		
		return Math.abs(calculatedAngle);
	}

	/** ACCESSIBLE METHODS **/
	public float getVelocity(){
		return velocity.len();
	}

	public Vector2 getRawVelocity(){
		return velocity;
	}




	// Given a velocity and the degree I want to end up on,
	// where will I be? Note: This method is a composition of other methods used above, would be best
	// if physics calculations were abstracted into their own class.
	public PeekTuple peek(Vector2 velocity, float degree, WorldSpatial.RelativeDirection turnDirection, float delta){


		float currentAngle = angle;
		float currentRotation = rotation;
		boolean currentlyAccelerating = this.accelerating;
		boolean currentlyReversing = this.reversing;
		float currentX = getX();
		float currentY = getY();
		Vector2 currentVelocity = new Vector2(velocity.x,velocity.y);
		State currentCarDirection = carDirection;


		boolean reachable = true;



		float timeDifference = -1;
		float normalizedRotation = normalizeAngle(currentRotation);
		float normalizedDegree = normalizeAngle(degree);
		
		
		timeDifference = ( Math.abs(normalizedRotation-normalizedDegree) / (ROTATING_FACTOR * delta));

				

		for(int i = 0; i < (int) Math.round(timeDifference); i++){
			if((currentRotation < degree && turnDirection.equals(WorldSpatial.RelativeDirection.LEFT) || (currentRotation > degree && turnDirection.equals(WorldSpatial.RelativeDirection.RIGHT)))){
				// Calculate Right turns
				if(turnDirection.equals(WorldSpatial.RelativeDirection.RIGHT)){
					currentAngle -= ROTATING_FACTOR * delta;
					if(currentlyReversing){
						currentAngle *= -1;
					}
				}
				// Calculate Left turns
				else{
					currentAngle += ROTATING_FACTOR * delta;
					if(currentlyReversing){
						currentAngle *= -1;
					}
				}
			}





			if(currentVelocity.len() > EPSILON) {
				// Update our rotation
				currentRotation += currentAngle;
				// Slowly return our rotation to 0 if not turning
				if(currentAngle > TURN_EPSILON || currentAngle < TURN_EPSILON ){
					// Slowly update turnings
					int sign = currentAngle > 0 ? 1 : -1;
					float magnitude = Math.abs(currentAngle) - Math.abs(currentAngle)*STEERING_ADJUST_FACTOR;
					currentAngle = magnitude * sign;
				} else {
					currentAngle = 0;
				}
			}

			// Create an acceleration vector by rotating a unit vector
			// and scaling with the appropriate force
			Vector2 acceleration = new Vector2(1,0);
			acceleration.rotate(currentRotation);
			acceleration.scl(ACCELERATION);

			// Create a friction vector
			Vector2 friction = new Vector2(1,0);
			if(acceleration.len() > 0){
				// Rotate to face the other direction
				friction.rotate(acceleration.angle() - MAX_DEGREES/2);
			} else {
				// Apply friction in the opposite direction that we are facing
				friction.rotate((currentRotation - MAX_DEGREES/2) % MAX_DEGREES);
			}
			friction.scl(FRICTION_FORCE);

			// Calculate net change

			Vector2 netAcceleration = acceleration.add(friction);



			// Calculating the movement
			// Rotate our velocity (highly simplified effect of rotating the car) and update with acceleration
			currentVelocity.setAngle(currentRotation);
			if((currentCarDirection.equals(State.REVERSE) && currentlyAccelerating) || (currentCarDirection.equals(State.FORWARD) && currentlyReversing)){
				currentVelocity.x -= netAcceleration.x * delta;
				currentVelocity.y -= netAcceleration.y * delta;
			}
			else{
				currentVelocity.x += netAcceleration.x * delta;
				currentVelocity.y += netAcceleration.y * delta;
			}

			// If we get greater than max velocity then limit us to that, if we're smaller than epsilon stop
			if(currentVelocity.len() > MAX_SPEED ) {
				float scalar = currentVelocity.len() / MAX_SPEED;
				currentVelocity.scl(1/scalar);
			}
			else if (currentVelocity.len() < EPSILON){
				currentVelocity.x = 0;
				currentVelocity.y = 0;
				if(currentCarDirection.equals(State.FORWARD)){
					currentCarDirection = State.REVERSE;
				}
				else{
					currentCarDirection = State.FORWARD;
				}
			}

			if(currentCarDirection.equals(State.REVERSE)){

				currentVelocity.rotate(180);
			}

			currentX += currentVelocity.x * delta;
			currentY += currentVelocity.y * delta;

			// Check if you will hit a wall
			if(World.lookUp(currentX, currentY).getName().equals("Wall")){
				reachable = false;
			}
			currentAngle = 0;
		}



		return new PeekTuple(new Coordinate(Math.round(currentX), Math.round(currentY)),reachable);

	}

	// Debug mode for the car
	public void printDebug(){
		logger.info(
				"\nCurrent Speed: "+getVelocity()+"\n"+
				"Current Angle: "+getAngle()+"\n"+
				"Current Position: "+getPosition()+"\n"+
				"Current Tile: "+World.lookUp(getX(), getY()).getName()+"\n\n"
				);
	}

	public float getAngle(){
		return (rotation % 360 + 360) % 360;
	}

	public HashMap<Coordinate,MapTile> getView(){
		int currentX = Math.round(getX());
		int currentY = Math.round(getY());

		HashMap<Coordinate,MapTile> subMap = new HashMap<Coordinate,MapTile>();
		for(int x = currentX - VIEW_SQUARE; x <= currentX+VIEW_SQUARE; x++){
			for(int y = currentY - VIEW_SQUARE; y <= currentY+VIEW_SQUARE; y++){
				MapTile tile = World.lookUp(x,y);
				subMap.put(new Coordinate(x,y),tile);

			}
		}

		return subMap;
	}

	public String getPosition(){
		return Math.round(this.getX())+","+Math.round(this.getY());
	}
	public int getHealth(){
		return this.health;
	}

	public WorldSpatial.Direction getOrientation(){
		return this.currentOrientation;
	}







}
