package activitystreamer.server;

public class ServerDetails {
	private String id; 
	private String host; 
	private Long load; 
	private Long port;
	public ServerDetails(String id, String host, Long port, Long load) {
		super();
		this.id = id;
		this.load = load;
		this.host = host;
		this.port = port;
	}
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getHost() {
		return host;
	}
	public void setHost(String host) {
		this.host = host;
	}
	public Long getLoad() {
		return load;
	}
	public void setLoad(Long load) {
		this.load = load;
	}
	public Long getPort() {
		return port;
	}
	public void setPort(Long port) {
		this.port = port;
	}

}
