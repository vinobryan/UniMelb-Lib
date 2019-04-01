package utilities;

public class PeekTuple{
	private final Coordinate coordinate;
	private final boolean  reachable;
	
	public PeekTuple(Coordinate coordinate, boolean reachable){
		this.coordinate = coordinate;
		this.reachable = reachable;
	}
	
	public Coordinate getCoordinate(){
		return this.coordinate;
	}
	
	public boolean getReachable(){
		return this.reachable;
	}
}
