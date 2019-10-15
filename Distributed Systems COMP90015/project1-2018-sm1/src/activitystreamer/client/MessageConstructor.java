package activitystreamer.client;

import org.json.simple.JSONObject;

import activitystreamer.util.Settings;

public class MessageConstructor {
	public String activity(JSONObject activity) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.ACTIVITYMESSAGE);
		obj.put(Settings.USERNAME, Settings.getUsername());
		obj.put(Settings.SECRET, Settings.getSecret());
		obj.put(Settings.ACTIVITY, activity);
		return obj.toJSONString();
	}
	public String anonymusLogin() {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.LOGIN);
		obj.put(Settings.USERNAME, Settings.ANONYMOUS);
		return obj.toJSONString();
	}
	public String login(String username, String secret) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.LOGIN);
		obj.put(Settings.USERNAME, username);
		obj.put(Settings.SECRET, secret);
		return obj.toJSONString();
	}
	public String logOut() {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.LOGOUT);
		return obj.toJSONString();
	}
	public String register(String username, String secret) {
		JSONObject obj = new JSONObject();
		obj.put(Settings.COMMAND, Settings.REGISTER);
		obj.put(Settings.USERNAME, username);
		obj.put(Settings.SECRET, secret);
		return obj.toJSONString();
	}
}
