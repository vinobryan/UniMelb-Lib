package controller;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import world.Car;

// Manual Controls for the car
public class ManualController extends CarController{
	
	public ManualController(Car car){
		super(car);
	}
	
	public void update(float delta){

        if (Gdx.input.isKeyPressed(Input.Keys.B)) {
            applyBrake();
        }
        
        if (Gdx.input.isKeyPressed(Input.Keys.UP)) {
        	applyForwardAcceleration();
        }
        if (Gdx.input.isKeyPressed(Input.Keys.DOWN)) {
        	applyReverseAcceleration();
        }
        if (Gdx.input.isKeyPressed(Input.Keys.LEFT)){
        	turnLeft(delta);
        }
        if (Gdx.input.isKeyPressed(Input.Keys.RIGHT)){
        	turnRight(delta);
        }
        

	}
}
