package tiles;

/**
 * Represents a Utility tile
 * There are two types of Utility Tiles, in the map, green representing the
 * start location while red representing the exit location.
 *
 */
public class UtilityTile extends MapTile{
	
	public enum Type {START, EXIT}
	private final Type type;
	
	public UtilityTile(String layerName, Type type) {
		super(layerName);
		this.type = type;
	}
	
	public boolean isExit(){
		return type == Type.EXIT;
	}
	
	public boolean isStart(){
		return type == Type.START;
	}

}
