package exceptions;

public class NoStartLocationException extends Exception {
	public NoStartLocationException(){
		System.out.println("Start location not defined! Invalid map.tmx!");
	}
}
