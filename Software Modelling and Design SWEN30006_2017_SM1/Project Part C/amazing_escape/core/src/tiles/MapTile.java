package tiles;



/**
 * Represents a single MapTile
 * 
 *
 */
public class MapTile {
	
	public static final String tileNameSpace = "tiles.";

	protected String name;
	
	public MapTile(String layerName) {
		this.name = layerName;
	}

	public String getName() {
		return name;
	}
}
