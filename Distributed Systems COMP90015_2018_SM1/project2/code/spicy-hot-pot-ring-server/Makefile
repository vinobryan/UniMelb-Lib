SERVER=activitystreamer/Server
CLIENT=activitystreamer/Client
EXECUTABLE=src/activitystreamer/util/*.java src/activitystreamer/client/*.java src/activitystreamer/server/*.java src/activitystreamer/*.java
COMPILER?=javac
EXECUTOR?=java
CLASSPATH?=.:lib/gson-2.2.2.jar:lib/json_simple-1.1.jar:lib/apache-log4j-2.5-bin/*:lib/commons-cli-1.3.1/*:target
DIRECTORY?=target

all:
	mvn archetype:generate -DgroupId=activitystreamer -DartifactId=spicy-hot-pot-2 -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
	mvn compile
	mvn test
	mvn package
	mvn exec:java -Dexec.args="-lh localhost -lp 8000 -s 1234567890 -a 5000"

install:
	sudo apt install maven

compile:
	mvn compile

package:
	mvn package

run:
	mvn exec:java

server1:
	mvn exec:java -Dexec.args="-lh localhost -lp 8000 -s 1234567890 -a 1000"

server2:
	mvn exec:java -Dexec.args="-lh localhost -lp 8001 -rh localhost -rp 8000 -s 1234567890 -a 1000"

server3:
	mvn exec:java -Dexec.args="-lh localhost -lp 8002 -rh localhost -rp 8001 -s 1234567890 -a 1000"

server4:
	mvn exec:java -Dexec.args="-lh localhost -lp 8003 -rh localhost -rp 8002 -s 1234567890 -a 1000"

test:
	mvn test

clean:
	mvn clean

# Windows compilation
# javac -cp .;lib/gson-2.2.2.jar;lib/json_simple-1.1.jar;lib/apache-log4j-2.5-bin/*;lib/commons-cli-1.3.1/*;target -d target src/activitystreamer/util/*.java src/activitystreamer/client/*.java src/activitystreamer/server/*.java src/activitystreamer/*.java

# Windows execution
# java -cp .;lib/gson-2.2.2.jar;lib/json_simple-1.1.jar;lib/apache-log4j-2.5-bin/*;lib/commons-cli-1.3.1/*;target activitystreamer/Client -u aaron -s fmnmpp3ai91qb3gc2bvs14g3ue -rh localhost -rp 8000

#mvn archetype:generate -DgroupId=activitystreamer -DartifactId=spicy-hot-pot-2 -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
#mvn compile
#mvn test
#mvn package
#mvn exec:java
#mvn clean
