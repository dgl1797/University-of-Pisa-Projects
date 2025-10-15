# Concept
The application allows to manage and share personal playlists by exploiting youtube music APIs. It is possible to create two different types of account: Validator; Common

Validators are the one entitled to update the songs and validate links, while common users are the one using the application. There is a chronjob executing in the backend that automatically updates the most popular user to validator every 12 months. Popularity and Social interactions are modeled and stored in Neo4j Graph database by exploiting Mongo's IDs to link information to users and Redis database is used to handle inconsinstencies which are temporary solved by querying the change that happened in app for then being updated with a second chronjob that automatically executes in the least populated time to not overload the main server.

Common user have multiple management options like creating, sharing and exploring playlists, but they can also interact with other users' playlists by leaving likes and getting suggestions based on Neo4j statistics computation and path mining.

# Usage
* Run ```docker-compose up frontend --build -d``` to run frontend and from within the container: ```npm run build``` to generate the build
* Copy and paste the resulting build folder inside the backend folder
* Run ```docker-compose down frontend --volumes``` to shutdown frontend service and discard temporary volumes created
* Run ```docker-compose up --build -d``` to run all the services
* Connect to http://localhost:5000 to use the application