package activitystreamer.server;


public class Response {
	
	private String response = null;
	private boolean disconnect = false;
	private String secResponse = null;
	
	/** Single response
	 * @param response
	 * @param disconnect
	 */
	public Response(String response, boolean disconnect) {
		this.response = response;
		this.disconnect = disconnect;
	}
	
	/** Two responses
	 * @param response
	 * @param secResponse
	 * @param disconnect
	 */
	public Response(String response, String secResponse, boolean disconnect) {
		this.response = response;
		this.secResponse = secResponse;
		this.disconnect = disconnect;
	}
	
	/** No response
	 * @param disconnect
	 */
	public Response(boolean disconnect) {
		this.disconnect = disconnect;
	}
	
	/** Single response, keep connection
	 * @param response
	 */
	public Response(String response) {
		this.response = response;
		this.disconnect = false;
	}

	/** Get current (first) response
	 * @return
	 */
	public String getResponse() {
		return response;
	}
	
	/** Get second response
	 * @return
	 */
	public String getSecResponse() {
		return secResponse;
	}

	/** Set second response
	 * @return
	 */
	public void setResponse(String response) {
		this.response = response;
	}

	/** Get whether the response requires disconnection
	 * @return
	 */
	public Boolean getDisconnect() {
		return disconnect;
	}

	/** Set whether the response requires disconnection
	 * @return
	 */
	public void setDisconnect(boolean disconnect) {
		this.disconnect = disconnect;
	}
    

}
