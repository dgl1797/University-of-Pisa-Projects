# Concept
Developing a fully functional Distributed Web-Appliaction that implements a live chat for specific companies and organize work and meetings in a cooperative and smart way.

To simulate the distributed system Docker is used to instantiate separate machines:
* 2 Tomcat Servers that are dispatched by Nginx balancing and use MVC logic to create dynamic web-pages
* Erlang Distributed Chat and Notification servers to handle message exchange and in-app notifications with persistency in MySQL
* Nginx balancer to dispatch users to Tomcat available instances through IP-Hashing to maintain sessions within the same server
* MySQL instance with persistent local storaging in /Database/data

# Usage
Clone the repository using git clone, then use the build.ps1 command to run the containers. Once the process is over follow the setup steps:
* from within the database container execute the following commands:
    * ```mysql -u root -p``` and insert the password "root"
    * ```mysql> source /scripts/creation_script.sql```
    * ```mysql> source /scripts/dumb.sql```
    * ```mysql> exit```
* from within the chat and notification servers execute the following commands:
    * ```cd /workspace/chat_server``` or notification_server depending on which container you are in
    * ```./publish.sh```
* from within the tomcat servers execute the following commands:
    * ```cd /workspace```
    * ```./publish.sh```
* go to http://tomcatapp/app and use the application