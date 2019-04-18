import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;

/**
 * This class should be used to contain all the different objects in your game world, and schedule their interactions.
 * 
 * You are free to make ANY modifications you see fit.
 * These classes are provided simply as a starting point. You are not strictly required to use them.
 */
public class World {
	
	private Map map;
	private Player player;
	private Camera camera;
	private static final int RIGHTCLICK = 1;
	
	/** Construct a world, initialising map, player
	 * 
	 */
	public World() {
		map = new Map();
		player = new Player();
		camera = new Camera(map.getMapWidth(), map.getMapHeight());
	}
	
	/** Update player and camera
	 * @param input
	 * @param delta
	 */
	public void update(Input input, int delta) {
		if (input.isMouseButtonDown(RIGHTCLICK)) {
			float destX = input.getMouseX() + camera.getLeft();
			float destY = input.getMouseY() + camera.getTop();
			player.setDestX(destX, destY);
			player.setMove(true);
		}
		
		player.update(delta, this.map);
		camera.update(player);
	}
	
	/** Render graphs
	 * @param g
	 */
	public void render(Graphics g) {
		camera.render(g);
		map.render();
		player.render();
	}
}
