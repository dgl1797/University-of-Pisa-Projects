# server.ts file

Containing the server initialization aswell as the db connections

# Utils

Containing a single index.ts file exporting all the utility functions.

# Routes

- ## index.ts exporter file
  - import every route index in it as [routerName] and export everything inside of the routers object
- ## Middlewares
  - index.ts containing all the generic purposes middlewares such as authentication middleware
- ## folder in snake_case for the single route
  - index.ts file containing the router declarationa and definition importing all the handlers, the validation schemas and the route-specific middlewares
  - handlers.ts file exporting all the routes' logics
  - validators.ts file exporting all the Joi schemas
  - middlewares.ts file exporting all the route-specific middlewares

# Models

- index.ts exporter file, import every db-model in here and export them inside of a unique object
- [ModelName].ts containing all the models and the interfaces necessary to represent the entity in the databases

# Daos

- index.ts exporting all the daos
- [daoName].ts containing all the necessary to create a dao that performs the operations on a specific db
