# Entities

- ## MONGODB:
  - Users: registered user with their respective level of access and a unique username
  - Songs: songs with the youtube music video obtained by youtube music api and reports for proposal/broken-link
  - Playlists: collection of songs containing only the main attributes of the song and the author aswell as a
    private/public representation in boolean
- ## NEO4J:

  - Users: single parameter entity representing the mongo entity linked by his username (or \_id)
  - Playlists: as Users
  - CLONES: relation between two playlists
  - LIKES: relation between a user and not owned public playlists
  - CREATED: relation between Users and owned playlists

- ## REDIS:
  - EditedSongs: HashMap containing the temporary inconsistent fields between songs entity and playlists one
