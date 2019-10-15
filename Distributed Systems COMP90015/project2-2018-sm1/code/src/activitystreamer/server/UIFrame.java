package activitystreamer.server;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.border.Border;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.DefaultCaret;

import activitystreamer.util.Settings;

@SuppressWarnings("serial")
public class UIFrame extends JFrame implements ActionListener {
	private JTextArea overallOutput = new JTextArea();
	private JTextArea receiveFromClient = new JTextArea("Client\n");
	private JTextArea receiveFromServer = new JTextArea("Server\n");
	private JTable userOutput;
	private JTable serverOutput;
	private JScrollPane scrollPane;
	private JButton refreshButton;
	private JLabel localPortNumber = new JLabel("Local Port:" + Settings.getLocalPort());
	private JLabel remotePortNumber = new JLabel("Remote Port:" + Settings.getRemotePort());
	private JLabel currentRegQueue = new JLabel("Reg Queue Num: 0");
	private JLabel currentLoad = new JLabel("Current Load: 0");
	private JCheckBox doActivity = new JCheckBox("Do Activity");
	private static DefaultTableModel dtmUser;
	private static DefaultTableModel dtmServer;
	
	private static UIFrame frame = null;
	
	public static UIFrame getInstance() {
		if(frame == null){
			frame = new UIFrame();
		}
		return frame;
	}

	public UIFrame(){
		setTitle("ActivityStreamer Text Output");
		((DefaultCaret) overallOutput.getCaret()).setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
		
		// main frame
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(2,2));
		
		// overall output frame
		JPanel outputPanel = setNewFrame("Report Output");
		overallOutput.setEditable(false);
		scrollPane = new JScrollPane(overallOutput);
		outputPanel.add(scrollPane, BorderLayout.CENTER);
		
		// message receive frame
		JPanel messagePanel = setNewFrame("Message Panel");
		ctMessage(messagePanel);
		
		// user frame
		
		JPanel userPanel = setNewFrame("User Registrations & Server Connections");
		ctUser(userPanel);
		
		// setting frame
		JPanel settingPanel = setNewFrame("Settings");
		ctSettings(settingPanel);
		
		// add frames into main
		mainPanel.add(outputPanel);
		mainPanel.add(messagePanel);
		mainPanel.add(userPanel);
		mainPanel.add(settingPanel);
		
		// add main and set parameters
		add(mainPanel);
		setLocationRelativeTo(null); 
		setSize(1280,768);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
	}
	
	private void ctSettings(JPanel settingPanel) {
		JPanel buttonGroup = new JPanel();
		refreshButton = new JButton("Refresh");
		refreshButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				
			}
		});
		doActivity.setSelected(true);
		buttonGroup.add(localPortNumber);
		buttonGroup.add(remotePortNumber);
		buttonGroup.add(currentLoad);
		buttonGroup.add(currentRegQueue);
		buttonGroup.add(doActivity);
		buttonGroup.add(refreshButton);
		settingPanel.add(buttonGroup,BorderLayout.CENTER);
		
	}
	
	private void ctUser(JPanel userPanel) {
		dtmUser = new DefaultTableModel(new String[] {"Username","Secret"}, 0);
		dtmServer = new DefaultTableModel(new String[] {"Server Connections"}, 0);
		userPanel.setLayout(new GridLayout(2,1));
		userOutput = new JTable(dtmUser);
		serverOutput = new JTable(dtmServer);
		scrollPane = new JScrollPane(userOutput);
		userPanel.add(scrollPane,BorderLayout.CENTER);
		scrollPane = new JScrollPane(serverOutput);
		userPanel.add(scrollPane,BorderLayout.CENTER);
	}
	
	private void ctMessage(JPanel messagePanel) {
		messagePanel.setLayout(new GridLayout(2,2));
		receiveFromClient.setEditable(false);
		receiveFromServer.setEditable(false);
		scrollPane = new JScrollPane(receiveFromClient);
		messagePanel.add(scrollPane,BorderLayout.CENTER);
		scrollPane = new JScrollPane(receiveFromServer);
		messagePanel.add(scrollPane,BorderLayout.CENTER);
	}
	
	public boolean doActivitySelected() {
		return doActivity.isSelected();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		
	}
	
	public void setReport(String msg) {
		overallOutput.append(msg + "\n");
	}
	
	public void setWarn(String msg) {
		overallOutput.append("[Warn] " + msg + "\n");
	}
	
	public void setAction(String msg) {
		overallOutput.append("[Action] " + msg + "\n");
	}
	
	public void setLoad(int num) {
		currentLoad.setText("Current Load: " + num);
	}
	
	public void setCurrentRegQueue(int num) {
		currentRegQueue.setText("Reg Queue Num: " + num);
	}
	
	public void setMsg(String msg, Connection con) {
		if (con.getType().equals(Settings.SERVER)) {
			setSvrMsg(msg, con);
		}else {
			receiveFromClient.append("[MSG] [" + con.getSocket().getRemoteSocketAddress() + "]: " + msg + "\n");
		}
		
	}
	
	public void setSentMsg(String msg) {
		receiveFromClient.append("[MSG Sent]: " + msg + "\n");
	}
	
	public void setSvrMsg(String msg, Connection con) {
		receiveFromServer.append("[From Server] [" + con.getSocket().getRemoteSocketAddress() + "]: " + msg + "\n");
	}
	
	public void setAnnounce(String str) {
		receiveFromServer.append("[Announce Sent] " + str + "\n");
	}
	
	public static void setUser(String username, String secret) {
		dtmUser.addRow(new String[] {username, secret});
	}
	
	public static void addServer(String address) {
		dtmServer.addRow(new String[] {address});
	}
	
	public static void delServer(String address) {
		for (int i=0; i<dtmServer.getRowCount(); i++) {
			if (dtmServer.getValueAt(i, 0).equals(address)) {
				dtmServer.removeRow(i);
				break;
			}
		}
	}
	
	private JPanel setNewFrame(String title) {
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		Border lineBorder = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.lightGray), title);
		panel.setBorder(lineBorder);
		return panel;
	}
	
	
}
