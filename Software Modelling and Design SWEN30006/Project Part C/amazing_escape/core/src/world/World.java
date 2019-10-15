package world;

import java.util.HashMap;

import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.g2d.Sprite;
import com.badlogic.gdx.maps.tiled.TiledMap;
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer;
import com.badlogic.gdx.maps.tiled.TiledMapTileLayer.Cell;

import controller.AIController;
import controller.CarController;
import controller.ManualController;
import mycontroller.MyAIController;
import tiles.MapTile;
import tiles.TrapTile;
import tiles.UtilityTile;
import utilities.Coordinate;
/**
 * This class provides functionality for use within the simulation system. It is NOT intended to be
 * read or understood for SWEN30006 Part C. Comments have been intentionally removed to reinforce
 * this. We take no responsibility if you use all your time trying to understand this code.
 *
 */

/**
 * GO TO LINE 67 TO ALTER THE CONTROLLER
 *
 */
public class World {
	
	private Car car;
	
	// Car's controller
	private static CarController controller;
	
	private static TiledMap map;
	

	
	public static int MAP_PIXEL_SIZE = 32;
	public static int MAP_HEIGHT;
	public static int MAP_WIDTH;
	
	private static String[] LAYER_NAME = {"Road","Utility","Trap","Wall"};
	
	private static HashMap<Coordinate,MapTile> mapTiles = new HashMap<Coordinate,MapTile>();
	
	public World(TiledMap map){
		World.map = map;
		
		TiledMapTileLayer roadLayer = (TiledMapTileLayer) getMap().getLayers().get("Road");
		MAP_HEIGHT = roadLayer.getHeight();
		MAP_WIDTH = roadLayer.getWidth();
		
		initializeMap(map);
		

		
		car = new Car(new Sprite(new Texture("sprites/car2.png")));
		// Set car size relative to the map scaling.
		car.setSize(car.getWidth()*(1/32f), car.getHeight()*(1/32f));
		car.setOriginCenter();
		System.out.println(map.getLayers().get("Wall"));
		
		// Add the car controller
		// controller = new ManualController(car);
		controller = new AIController(car);
		// controller = new MyAIController(car);
	}
	
	private void initializeMap(TiledMap map2) {
		// Iterate through all layer names
		for(String layerName : LAYER_NAME){
			// Set the layer
			TiledMapTileLayer layer = (TiledMapTileLayer) getMap().getLayers().get(layerName);
			
			// Iterate through the layers and input them into the hashtable
			System.out.println(layerName+" width: "+layer.getWidth()+" height: "+layer.getHeight());
			for(int x = 0; x < layer.getWidth(); x++){
				for(int y = 0; y < layer.getHeight(); y++){
					Cell cell = layer.getCell(x, y);
					if(cell != null){
						
						int reverseYAxis = layer.getHeight() -y;
						MapTile newTile = null; // Only stays null if exception/exit
						
						if(layerName.equals("Trap")){
							String className = MapTile.tileNameSpace + (String) cell.getTile().getProperties().get("type");
							try {
								newTile = (TrapTile) Class.forName( className ).newInstance();
							} catch (Exception e) {
								e.printStackTrace();
								System.exit(1);
							}
						}
						else if(layerName.equals("Utility")){
							UtilityTile.Type type = UtilityTile.Type.START;
							if(cell.getTile().getProperties().get("exit") != null){
								type = UtilityTile.Type.EXIT;
							}
							newTile = new UtilityTile(layerName,type);
						}
						else{
							newTile = new MapTile(layerName);
						}
						mapTiles.put(new Coordinate(x, reverseYAxis), newTile);
					}
				}
			}
		}
	}

	public void update(float delta){
		controller.update(delta);
        
        // Update the car
        car.update(delta);
	}
	
	public void render(Batch batch){
		car.draw(batch);
	}
	
	public static TiledMap getMap(){
		return map;
	}
	
	public static MapTile lookUp(double futureX, double futureY){
		
		
		
		// Convert Y coordinate
		int x = (int) Math.round(futureX);
		int y =  MAP_HEIGHT - (int) Math.round(futureY);
		
		if(mapTiles.containsKey(new Coordinate(x,y))){
			return mapTiles.get(new Coordinate(x,y));
		}
		else{
			return new MapTile("Empty");
		}
		
	}
	
	
	public Car getCar(){
		return this.car;
	}
}
