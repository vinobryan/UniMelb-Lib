package utilities;

import java.util.Objects;

public class Coordinate {
	public int x;
	public int y;
	
	private static final int X_POS = 0;
	private static final int Y_POS = 1;

	/**
	 * Constructs a coordinate object
	 * @param coordinate: "1,0" is an example of a coordinate string that will
	 * be deconstructed into a coordinate object.
	 */
	public Coordinate(String coordinate){
		// Split up coordinate
		try{
			String[] splitCoordinate = coordinate.split(",");
			this.x = Integer.parseInt(splitCoordinate[X_POS]);
			this.y = Integer.parseInt(splitCoordinate[Y_POS]);
		}
		catch(Exception e){
			e.printStackTrace();
		}
		
	}
	
	public Coordinate(int x, int y){
		this.x = x;
		this.y = y;
	}
	
	public String toString(){
		return x+","+y;
	}
	
	
	/**
	 * Defined in order to use it as keys in a hashmap
	 */
	public boolean equals(Object c){
		if( c == this){
			return true;
		}
		if(!(c instanceof Coordinate)){
			return false;
		}
		
		Coordinate coordinate = (Coordinate) c;
		
		return (coordinate.x == this.x) && (coordinate.y == this.y);
	}
	
	public int hashCode(){
		return Objects.hash(x,y);
	}
}
