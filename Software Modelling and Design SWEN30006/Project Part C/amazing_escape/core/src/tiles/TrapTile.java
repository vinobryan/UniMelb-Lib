package tiles;

import world.Car;

public abstract class TrapTile extends MapTile{

private static final String layerName = "Trap";

	public TrapTile() {
		super(layerName);
	}
	public abstract void applyTo(Car car, float delta);
}
