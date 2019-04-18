import org.newdawn.slick.Graphics;

/**
 * This class should be used to restrict the game's view to a subset of the entire world.
 * 
 * You are free to make ANY modifications you see fit.
 * These classes are provided simply as a starting point. You are not strictly required to use them.
 */
public class Camera {
	
	// current coordinates of camera
	private float camX;
	private float camY;
	
	// offsets of a camera
	private float offsetMaxX;
	private float offsetMaxY;
	private float offsetMinX;
	private float offsetMinY;
	
	/** Construct a camera
	 * @param mapWidth Width of the map
	 * @param mapHeight Height of the map
	 */
	public Camera(float mapWidth, float mapHeight) {
		offsetMaxX = mapWidth - App.WINDOW_WIDTH;
		offsetMaxY = mapHeight - App.WINDOW_HEIGHT;
		offsetMinX = 0;
		offsetMinY = 0;
	}
	
	/** Update camera's position
	 * @param player
	 */
	public void update(Player player) {
		camX = player.getX() - App.WINDOW_WIDTH / 2;
		camY = player.getY() - App.WINDOW_HEIGHT / 2;
		
		// prevent display dark area
		if (camX > offsetMaxX){
			camX = offsetMaxX;
		}else if (camX < offsetMinX) {
			camX = offsetMinX;
		}
		if (camY > offsetMaxY) {
			camY = offsetMaxY;
		}else if (camY < offsetMinY) {
			camY = offsetMinY;
		}
		    
	}
	
	/** Render a camera to follow the player
	 * @param g
	 */
	public void render(Graphics g) {
		g.translate(-camX, -camY);
	}

	/** Get the left X coordinate of the camera
	 * @return
	 */
	public float getLeft() {
		// You probably want to change this.
		return camX;
	}
	
	/** Get the top Y coordinate of the camera
	 * @return
	 */
	public float getTop() {
		// You probably want to change this.
		return camY;
	}
	
	/** Get the right X coordinate of the camera
	 * @return
	 */
	public float getRight() {
		// You probably want to change this.
		return camX + App.WINDOW_WIDTH;
	}
	
	/** Get the bottom Y coordinate of the camera
	 * @return
	 */
	public float getBottom() {
		// You probably want to change this.
		return camY + App.WINDOW_HEIGHT;
	}

}
