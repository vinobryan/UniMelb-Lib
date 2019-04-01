package tiles;

import com.badlogic.gdx.math.Vector2;

import world.Car;

public class MudTrap extends TrapTile {
	
	private float SLOWDOWN_FACTOR = 0.6f;
	public void applyTo(Car car, float delta) {
		Vector2 currentSpeed = car.getRawVelocity();
		float xReduction = currentSpeed.x*SLOWDOWN_FACTOR*delta;
		float yReduction = currentSpeed.y*SLOWDOWN_FACTOR*delta;
		car.setVelocity(currentSpeed.x-xReduction,currentSpeed.y-yReduction);
	}

}
