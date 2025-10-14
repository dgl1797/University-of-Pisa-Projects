# Concept
Developing a Mobile Application that uses Mobile sensors to mine meaningful data about roads quality.

The application uses Open Street Map APIs to efficiently spot and compute areas around the globe that are sent to a Cloud Serverless backend exploiting Google Cloud Functions as a middleware to interact with Google Firestore and store statistics about gathered data. Those are then used to compute centroids from which squared areas are traced in which all collected statistics are reduced to a Street Quality Metrics able to assess how bruised an area is.

Finally, simple roads quality metrics relative the actual location's centroid and its surroundings are downloaded and displayed by the mobile application avoiding unnecessary computations to preserve device's battery. Further details can be found in the [project's documentation](./documentation/Project%20Paper.pdf)