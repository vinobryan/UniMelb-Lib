package activitystreamer.server;

import org.json.simple.JSONObject;

import activitystreamer.util.Settings;

public class MessageValidator {
	/** Validate lock message
	 * @param content
	 * @return True if it is correct
	 */
	public boolean lock(JSONObject content) {
		if (content.containsKey(Settings.USERNAME) && content.containsKey(Settings.SECRET)) {
			return true;
		}
		return false;
	}
	
	/** Validate authenticate message
	 * @param content
	 * @return True if it is correct
	 */
	public boolean authenticate(JSONObject content) {
		if (Settings.getSecret().equals((String) content.get(Settings.SECRET))) {
			return true;
		}else {
			return false;
		}
	}
	
	/** Validate activity message 
	 * @param content
	 * @return True if it is correct
	 */
	public boolean activity(JSONObject content) {
		if (content.containsKey(Settings.ACTIVITY) && content.containsKey(Settings.USERNAME)) {
			if (content.get(Settings.USERNAME).equals(Settings.ANONYMOUS) || content.containsKey(Settings.SECRET)) {
				try {
					@SuppressWarnings("unused")
					JSONObject json = (JSONObject) content.get(Settings.ACTIVITY);
					return true;
				}catch (ClassCastException e){
					return false;
				}
			}
			return false;
		}else {
			return false;
		}
	}
	
	/** Validate activity broadcast message
	 * @param content
	 * @return True if it is correct
	 */
	public boolean activityBroadcast(JSONObject content) {
		if (content.containsKey(Settings.ACTIVITY)) {
			return true;
		}else {
			return false;
		}
	}
	
	/** Validate announce message
	 * @param content
	 * @return True if it is correct
	 */
	public boolean announce(JSONObject content) {
		if (content.get(Settings.LOAD) == null | content.get(Settings.PORT) == null | content.get(Settings.ID) == null) {
			return false;
		}else {
			return true;
		}
	}
	
	/** Validate login message
	 * @param content
	 * @return True if it is correct
	 */
	public boolean login(JSONObject content) {
		if (content.containsKey(Settings.USERNAME)) {
			if (content.containsKey(Settings.SECRET) || content.get(Settings.USERNAME).equals(Settings.ANONYMOUS)) {
				return true;
			}
		}
		return false;
	}
	

}
