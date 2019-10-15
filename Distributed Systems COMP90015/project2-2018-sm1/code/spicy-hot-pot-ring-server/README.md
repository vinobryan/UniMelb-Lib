# spicy-hot-pot-2

#### COMP90015 Distributed Systems
#### Semester 1 2018
#### Project 2

### Team Spicy Hot Pot

### Mentor
#### Yasmeen George

### Members
#### 675485 Ziren Xiao zirenx@student.unimelb.edu.au
#### 736901 Ivan Ken Weng Chee ichee@student.unimelb.edu.au
#### 824325 Duer Wang minghaow1@student.unimelb.edu.au
#### 920577 Yue Yang yuey16@student.unimelb.edu.au

### This application is managed by Apache Maven
#### sudo apt install maven

### Instructions
#### make install
#### mvn compile
### First server
#### mvn exec:java -Dexec.args="-lh <LOCAL_HOSTNAME> -lp <LOCAL_PORT> -s <SERVER_SECRET> -a <ACTIVITY_INTERVAL>"
### Subsequent server
#### mvn exec:java -Dexec.args="-lh <LOCAL_HOSTNAME> -lp <LOCAL_PORT> -rh <REMOTE_HOSTNAME> -rp <REMOTE_PORT> -s <SERVER_SECRET> -a <ACTIVITY_INTERVAL>"

### Other
#### JAR can be found in /target
