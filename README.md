Collection of all University Projects developed as a group activity during studies at University of Pisa.

## [Large Scale and Multi-Structured Databses - JAPM](https://github.com/dgl1797/University-of-Pisa-Projects/tree/JAPM)
**Used Tech:**
- React with Hooks and Redux Store to manage requests to backend and local caching
- ExpressJS backend interacting and keeping data consistency between multiple datasets through ChronJobs developed from scratch
- MongoDB as main document-based DBMS
- Redis for temporary in-memory handling of inconsitencies and session management and expiration control
- Neo4j graph DBMS to model social interactions and statistics
- Typescript integration in both frontend and backend with prettier and other QoL tools
- Docker for containers networking (frontend isolation from data sources) and distributed environment simulation
- Docker for virtual development environment

**Learned Skills:**
- Advanced Frontend Development with React and additional libraries like:
    * Axios to handle Requests and manage their state and live notification to user
    * Redux for local caching of data and fluent transitions to new updated versions
    * CAP Theorem playarounds to offer as much Consistency; Availability, and Partition tolerance as possible
- Advanced Backend Development with ExpressJS in a RESTful setup with: 
    * authentication middlewares and session control
    * chronjobs controlling the state of the app and dynamically updating the user as new events occur
    * Advanced distributed databases coordination to keep data consistency by carefuly designing documents handled by the various DBMS employed
- Advanced employment of document-based DBMSs and indexing to speedup in-app researches over massive datasets
- Complex Social interactions modeling and profiling through Graph-structured DBMS
- Advanced Docker application for networking and distributed environment testing/simulation as well as container isolation

## [Data Mining and Machine Learning - Deck Builder](https://github.com/dgl1797/University-of-Pisa-Projects/blob/DeckBuilder/Documentation.pdf)
**Used Tech:**
- ReactJS with CSS Modules for the Frontend
- Flask for a simple Backend setup that allows interactions between ML algorithms and Frontend requests in JSON format
- MongoDB for online persistency of gathered data
- Pandas, Numpy and MlxTend for data analysis and frequent patterns mining 
- [ImbLearn](https://imbalanced-learn.org/stable/) with [Scikit Learn](https://scikit-learn.org/stable/) for dataset balancing using SMOTE hyper-sampling and pipeline setup and execution for classifications task and algorithm selection/evaluation.

**Learned Skills:**
- Data Mining to collect card and matches information for ClashRoyale
- Data Analysis, Data Cleaning, and Feature Selection with scikit-learn, pandas, numpy and matplotlib
- Association Rule Mining and confidence usage for AI-based cards association
- Class rebalancing using both undersampling and oversampling (SMOTE)
- Classification methods and differences plotting the performances obtained in the scenario
- Usage of Association confidence to improve Rank classification
- Python Flask usage to implement RESTful APIs interfacing with MongoDB and Machine Learning scripts

## [Business and Project Management - PaperAI](https://github.com/dgl1797/University-of-Pisa-Projects/blob/PaperAI/Project_Documentation.pdf)
**Used Tech:**
- Svelte + Vite + SvelteKit with TypeScript for Simple Full-Stack WebApp Development with Server-Side Rendering
- Python for Mining and Scraping integrated with Qdrant and MongoDB
- Qdrant Vectorial DBMS for semantic similarity querying and ranking
- MongoDB Document DBMS for structured data storage

**Learned Skills:**
- Svelte + Vite + SvelteKit with TypeScript WebApp development
- Python APIs integration with existing AI tools to enhance application features
- WebScraping Basics with Python
- Data Mining techniques for unstructured data
- Data Cleaning and transformation for persisting in Vectorial Database QDrant
- Integration with MongoDB for Data collection and linking to QDrant via generated ID

## [Distributed Systems and Middleware Technologies - Web Chat](https://github.com/dgl1797/University-of-Pisa-Projects/blob/DSMT/README.md)
**Used Tech:**
- Docker Compose Orchestration to simulate complex distributed environment
- Tomcat with EJBs and Jakarta Servlets to handle entry points and interactions with MySQL server in an MVC web app
- NGINX to serve the built Tomcat server in a distributed way by using multiple instances of Tomcat and IP-Hashing to keep the sessions within the same server
- ErLang servers in a Register <-> Server to handle message queues and safe interactions to store chats and notifications in MySQL 

**Learned Skills:**
- Erlang for distributed application services - Cowboy web sockets server through HTTP endpoints; Notification and Chat Registries for real-time coordination
- Docker advanced configuration of Development environments simulating the deployment environment through networking and docker-compose
- Java EJBs using DAOs and DTOs to interact with MySQL server
- Erlang MySQL handler process separated from Cowboy execution that handles queries asynchronously and coordinated through send/receive.
- Tomcat configuration and MVC web application development and deployment using JSPs and MySQL basic driver
- NGINX basic configuration for load balancing through IP-hashing over tomcat replicas to keep users' authorized sessions in the same server
- Final architecture deployment (2 instances of the tomcat server; 1 erlang server to handle chatrooms through registries; 1 erlang server to handle notifications through registries; 1 NGINX load balancer; 1 MySQL server for data persistency) 

## [Computational Intelligence and Deep Learning - Super Resolution in Computer Vision](https://github.com/dgl1797/University-of-Pisa-Projects/blob/CV_ImageUpsampling/Project%20Documentation.pdf)
**Used Tech:**
- Tensorflow-Keras APIs to build Deep Neural Networks and Training/Evaluation Pipelines
- PyTorch and TorchVision basics to compare the development of the main architecture with the Tensorflow Framework
- Pandas and Numpy for data preprocessing and organization in Computer Vision (Images Transformations, Standardization, Conversion between Visualization methods - NHWC <-> NCHW)
- Google Colab and Notebooks for both and cloudified Research on Deep Learning Approaches

**Learned Skills:**
- Tensorflow-Keras APIs to build Deep Neural Networks
- PyTorch and TorchVision comparison with Tensorflow-Keras
- Advanced reasoning on models architectures and compilation, with hyperparameters selection
- Advanced Computer Vision Techniques for Image Super Resolution
- Adversarial Losses and application to enhance and effectively influence the Generative Approach
- Noise evaluation Metrics and applications as loss in Deep Learning to reduce noise on final hyper-sampled image
- Images handling with OpenCV; PIL and tf.image
- Advanced plotting techniques for handling images and training/test charts with matplotlib
- Advanced DNN reasoning about training process as well as loss functions to be used for a specific goal
- Colab Notebooks usage for implementing and showing research results

## [Internet of Things - Industry Control](https://github.com/dgl1797/University-of-Pisa-Projects/blob/IoT_IndustryControl/IoT_Report.pdf)
**Used Tech:**
- ZorinOS
- Dockerized Contiki-NG
- C programming language for Sensors Programming
- Java programming language for Cloud Application Development
- MySQL Database for data storage and sensors orchestration
- Californium for CoAP protocol with Actuators
- Paho for MQTT protocol with Sensors
- HikariCP to instantiate a Multi-Process scalable JDBC connection pool to handle massive incoming requests to MySQL database
- Grafana for real-time visualization
- Mosquitto broker for service handling

**Learned Skills:**
- MQTT and CoAP protocols for battery-efficient message exchange between IoT nodes
- Java integration with IoT protocols to support gathered data with an intelligent centralized system
- Database Connection Pool developing nearly from scratch, using HikariCP to initialize the pool
- Advanced multi-threading in C and Java to handle massive requests to orchestrate and organize IoT nodes
- Grafana integration for real-time observation of environment control

## [Process Mining and Intelligence](https://github.com/dgl1797/University-of-Pisa-Projects/blob/AnomalyDetectionInManifacturing/ANOMALY%20DETECTION%20IN%20MANUFACTURING.pdf)
**Learned Skills:**
- Business Workflow Modeling and Process Mining for efficiency assessment
- Application of BPMSs to model processes and interaction between different sessions to simulate the workflow and cost analysis
- Optimization through process mining and bottleneck analysis

## [Symbolic and Evolutionary Artificial Intelligence - MoeLia](https://github.com/dgl1797/University-of-Pisa-Projects/blob/MoeLia/README.pdf)
**Used Tech:**
- Julia Programming Language and Package Management

**Learned Skills:**
- Dependency-Free Framework Development in Julia for Evolutionary AI systems
- Modular Programming for extensible functionalities implementation
- Pipeline Manager Development from scratch
- Multi Objective and Genetic Algorithms implementation to test their integration in MoeLia Framework 

## [Mobile and Social Sensing Systems - BikeAssistant](https://github.com/dgl1797/University-of-Pisa-Projects/blob/BikeAssistant/documentation/Project%20Paper.pdf)
**Used Tech:**
- Open Street Map APIs to gather city boundaries and information about current location
- Google Firebase and Cloud Functions to implement a cloudified and distributed serverless solution to collect gathered data from multiple users and compute statistics
- Kotlin Programming Language and Android Studio to develop and debug the application

**Learned:**
- Kotlin programming language
- debugging of Android Applications with emulators
- Google Cloud technologies to implement distributed serverless applications
- Android Studio
- Data Mining from Smartphone's sensors

## [Multimedia Information Retrieval Systems - Textual Search Engine](https://github.com/dgl1797/University-of-Pisa-Projects/blob/TextualSearchEngine/Project%20Documentation.pdf)
**Used Tech:**
- Base Java with Maven compiler

**Learned Skills:**
- Traditional techniques for indexing a textual corpus
- Traditional Text Processing and Normalization for Tokenization
- Vocabulary building and filtering with Stemming and Stopword removal
- Document Index building for documents' information storage and mapping
- Inverted Index building with both interleaved and splitted frequencies
- Inverted Index compression using Variable Byte Encoding with skips every $\sqrt{N_{postings}}$
- TFIDF and BM25 implementation for document scoring and ranked retrieval
- document and term upper bounds storage in Inverted Index
- Conjunctive and Disjunctive search algorithms implementation for ranked retrieval
- Disjunctive Document At A Time ranked retrieval (classic and pruned using MaxScore for both classic and compressed posting lists)

## [Cloud Computing - Bloom Filter](https://github.com/dgl1797/University-of-Pisa-Projects/blob/BloomFilterHadoop/Project%20Report.pdf)
**Used Tech:**
- [Hadoop](https://github.com/dgl1797/University-of-Pisa-Projects/blob/BloomFilterHadoop/hadoop3-installation.md) and HDFS for multi-node cloud setup in Java implementations
- [Spark](https://github.com/dgl1797/University-of-Pisa-Projects/blob/BloomFilterHadoop/Spark%20Installation%20Notes.pdf) on HDFS for multi-node cloud setup in Python implementations

**Learned Skills**:
- Cluster setup with HDFS for Hadoop and Spark distributed applications
- BloomFilter construction and testing using Hadoop and Spark
- MapReduce Programming paradigm in Hadoop for Java and Spark for python