import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;

public class Player {
	
	private static final float SPEED = 0.25f;
	private static final float DESTINATIONTHRESHOLD = 0.25f;
	private static final String FILE = "assets/scout.png";
	
	// initial position of the player
	private static final float PLAYERINITX = 55;
	private static final float PLAYERINITY = 99;
	
	private Image image;
	private float x;
	private float y;
	private float destX = 0;
	private float destY = 0;
	private boolean move = false;

	/** Construct a player with an initial point
	 * @param x Initial x
	 * @param y Initial y
	 */
	public Player() {
		// TODO Auto-generated constructor stub
		try {
			image = new Image(FILE);
		} catch (SlickException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		this.x = PLAYERINITX;
		this.y = PLAYERINITY;
	}
	
	/** Render the player
	 * (Draw image)
	 */
	public void render() {
		image.drawCentered(x, y);
	}
	
	/** Update player's x/y coordinates
	 * @param input
	 * @param delta
	 * @param map
	 * @param camera
	 */
	public void update(int delta, Map map) {	
		if (this.move) {
			// calculate angle and x/y move
			float diffX = this.destX - this.x;
			float diffY = this.destY - this.y;
			float rawX = this.x;
			float rawY = this.y;
			float angle = (float) Math.atan(Math.abs(diffY)/Math.abs(diffX));
			float r = delta * SPEED;
			float angleX = (float) (Math.cos(angle)*r);
			float angleY = (float) (Math.sin(angle)*r);

			// check move direction
			boolean moveRight = diffX >= 0;
			boolean moveDown = diffY >= 0;
			if (moveRight) {
				this.x = angleX + this.x;
			}
			if (!moveRight) {
				this.x = -angleX + this.x;
			}
			if (moveDown) {
				this.y = angleY + this.y;
			}
			if (!moveDown) {
				this.y = -angleY + this.y;
			}
			
			// if arrived destination, stop moving
			if (this.isDestination(diffX, diffY)) {
				this.setMove(false);
			}
			
			// if next point is solid, move back and stop moving
			if (map.getSolid(this.x, this.y)) {
				this.setMove(false);
				this.x = rawX;
				this.y = rawY;
			}
		}
		
	}
	
	/** Check if arrived destination
	 * @param diffX Difference of X
	 * @param diffY Difference of Y
	 * @return
	 */
	private boolean isDestination(float diffX, float diffY) {
		return diffX <= DESTINATIONTHRESHOLD && diffY <= DESTINATIONTHRESHOLD
				&& diffX >=0 && diffY >=0;
	}

	/** Get player's X coordinate
	 * @return
	 */
	public float getX() {
		return x;
	}

	/** Get player's Y coordinate
	 * @return
	 */
	public float getY() {
		return y;
	}

	/** Set destination coordinate
	 * @param destX
	 */
	public void setDestX(float destX, float destY) {
		this.destX = destX;
		this.destY = destY;
	}

	/** Set if player should move
	 * @param move
	 */
	public void setMove(boolean move) {
		this.move = move;
	}
	

}
