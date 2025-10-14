# Concept
Developing a simple and handy application that allows to build decks for the popular mobile game Clash Royale.

The application exploits a very basic React-JS frontend that interacts with a Flask server in Python. Notebooks allow to view and analyze data working as a first setup for the project. It extracts data through specific clash royale's public API and sotres them in a MongoDB cloudified instance.

The application also introduces smart suggestions by exploiting statistical data patterns mined from the APIs, allowing the player to have a real-time feedback on which cards synergize to build optimal decks. After the deck composition, the application also evaluates the rank of the final deck using a Machine Learning classifier.

Further details and analysis are described in the [Documentation](Documentation.pdf) provided within this repository.