import org.newdawn.slick.SlickException;
import org.newdawn.slick.tiled.TiledMap;

public class Map {
	
	private TiledMap map;
	// read map from a specific layer
	private static final int DEFAULTLAYER = 0;
	// render map at x/y coordinate
	private static final int DEFAULTINITX = 0; 
	private static final int DEFAULTINITY = 0;
	private static final String PROPERTYSOLID = "solid";
	private static final String PROPERTYTRUE = "true";
	private static final String PROPERTYFALSE = "false";
	// map file location
	private static final String MAPFILE = "assets/main.tmx";
	
	private float mapWidth;
	private float mapHeight;

	/** Construct a map
	 * 
	 */
	public Map() {
		try {
			map = new TiledMap(MAPFILE);
		} catch (SlickException e) {
			e.printStackTrace();
		}
		this.mapHeight = map.getHeight() * map.getTileHeight();
		this.mapWidth = map.getWidth() * map.getTileWidth();
	}
	
	/** Check if the coordinate is on solid
	 * @param x X pixel coordinate
	 * @param y Y pixel coordinate
	 * @return
	 */
	public boolean getSolid(float x, float y) {
		return this.getSolid(map.getTileId(Math.round(x / map.getTileWidth()),
				Math.round(y / map.getTileHeight()), DEFAULTLAYER));
	}
	
	/** Check if the id is solid
	 * @param id Tile id
	 * @return
	 */
	public boolean getSolid(int id) {
		String solid;
		// default return false
		solid = map.getTileProperty(id, PROPERTYSOLID, PROPERTYFALSE);
		if (solid.equals(PROPERTYTRUE)) {
			return true;
		}
		return false;
	}
	
	/** Render the map
	 * 
	 */
	public void render() {
		map.render(DEFAULTINITX, DEFAULTINITY);
	}

	/** Get map width
	 * @return
	 */
	public float getMapWidth() {
		return mapWidth;
	}

	/** Get map height
	 * @return
	 */
	public float getMapHeight() {
		return mapHeight;
	}
}
