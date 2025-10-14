-- MySQL dump 10.13  Distrib 8.1.0, for Linux (x86_64)
--
-- Host: localhost    Database: dsmtdb
-- ------------------------------------------------------
-- Server version	8.1.0

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `chat`
--

DROP TABLE IF EXISTS `chat`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `chat` (
  `id` int NOT NULL AUTO_INCREMENT,
  `user1` varchar(16) NOT NULL,
  `user2` varchar(16) NOT NULL,
  `creationTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE KEY `user1` (`user1`,`user2`),
  KEY `user2` (`user2`),
  CONSTRAINT `chat_ibfk_1` FOREIGN KEY (`user1`) REFERENCES `user` (`username`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `chat_ibfk_2` FOREIGN KEY (`user2`) REFERENCES `user` (`username`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `chat`
--

LOCK TABLES `chat` WRITE;
/*!40000 ALTER TABLE `chat` DISABLE KEYS */;
INSERT INTO `chat` VALUES (1,'a.bianchi','g.ferretti','2023-11-02 16:50:51'),(2,'l.russo','p.santoro','2023-11-02 16:50:51'),(3,'l.russo','v.moretti','2023-11-02 16:50:51'),(4,'f.pellegrini','mario.rossi','2023-11-02 16:50:51'),(5,'g.ferretti','s.martini','2023-11-02 16:50:51'),(6,'a.bianchi','l.russo','2023-11-02 16:50:51'),(7,'a.neri','g.ferretti','2023-11-02 16:50:51'),(8,'a.bianchi','a.neri','2023-11-02 16:50:51'),(9,'p.santoro','v.moretti','2023-11-02 16:50:51'),(10,'g.ferretti','primonome','2023-11-02 16:50:51');
/*!40000 ALTER TABLE `chat` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `message`
--

DROP TABLE IF EXISTS `message`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `message` (
  `id` int NOT NULL AUTO_INCREMENT,
  `content` varchar(256) NOT NULL,
  `sender` varchar(16) NOT NULL,
  `chatID` int NOT NULL,
  `creationTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `sender` (`sender`),
  KEY `chatID` (`chatID`),
  CONSTRAINT `message_ibfk_1` FOREIGN KEY (`sender`) REFERENCES `user` (`username`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `message_ibfk_2` FOREIGN KEY (`chatID`) REFERENCES `chat` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `message`
--

LOCK TABLES `message` WRITE;
/*!40000 ALTER TABLE `message` DISABLE KEYS */;
INSERT INTO `message` VALUES (1,'Hello, this is the start of our chat!','a.bianchi',1,'2023-11-02 16:50:51'),(2,'Hi there! Let\'s have a great conversation.','g.ferretti',1,'2023-11-02 16:50:51'),(3,'Hello, this is the start of our chat!','l.russo',2,'2023-11-02 16:50:51'),(4,'Hi there! Let\'s have a great conversation.','p.santoro',2,'2023-11-02 16:50:51'),(5,'Hello, this is the start of our chat!','l.russo',3,'2023-11-02 16:50:51'),(6,'Hi there! Let\'s have a great conversation.','v.moretti',3,'2023-11-02 16:50:51'),(7,'Hello, this is the start of our chat!','f.pellegrini',4,'2023-11-02 16:50:51'),(8,'Hi there! Let\'s have a great conversation.','mario.rossi',4,'2023-11-02 16:50:51'),(9,'Hello, this is the start of our chat!','g.ferretti',5,'2023-11-02 16:50:51'),(10,'Hi there! Let\'s have a great conversation.','s.martini',5,'2023-11-02 16:50:51'),(11,'Hello, this is the start of our chat!','a.bianchi',6,'2023-11-02 16:50:51'),(12,'Hi there! Let\'s have a great conversation.','l.russo',6,'2023-11-02 16:50:51'),(13,'Hello, this is the start of our chat!','a.neri',7,'2023-11-02 16:50:51'),(14,'Hi there! Let\'s have a great conversation.','g.ferretti',7,'2023-11-02 16:50:51'),(15,'Hello, this is the start of our chat!','a.bianchi',8,'2023-11-02 16:50:51'),(16,'Hi there! Let\'s have a great conversation.','a.neri',8,'2023-11-02 16:50:51'),(17,'Hello, this is the start of our chat!','p.santoro',9,'2023-11-02 16:50:51'),(18,'Hi there! Let\'s have a great conversation.','v.moretti',9,'2023-11-02 16:50:51'),(19,'Hello, this is the start of our chat!','g.ferretti',10,'2023-11-02 16:50:51'),(20,'Hi there! Let\'s have a great conversation.','primonome',10,'2023-11-02 16:50:51');
/*!40000 ALTER TABLE `message` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `notification`
--

DROP TABLE IF EXISTS `notification`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `notification` (
  `id` int NOT NULL AUTO_INCREMENT,
  `user` varchar(16) NOT NULL,
  `sender` varchar(16) NOT NULL,
  `chatID` int NOT NULL,
  `creationTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `user` (`user`),
  KEY `sender` (`sender`),
  KEY `chatID` (`chatID`),
  CONSTRAINT `notification_ibfk_1` FOREIGN KEY (`user`) REFERENCES `user` (`username`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `notification_ibfk_2` FOREIGN KEY (`sender`) REFERENCES `user` (`username`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `notification_ibfk_3` FOREIGN KEY (`chatID`) REFERENCES `chat` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `notification`
--

LOCK TABLES `notification` WRITE;
/*!40000 ALTER TABLE `notification` DISABLE KEYS */;
/*!40000 ALTER TABLE `notification` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `user` (
  `username` varchar(16) NOT NULL,
  `password` text NOT NULL,
  `name` varchar(16) NOT NULL,
  `surname` varchar(16) NOT NULL,
  `department` enum('IT','HR','Administration','Buying','Selling') NOT NULL,
  `onlineFlag` tinyint(1) NOT NULL,
  `creationTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES ('a.bianchi','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Alessia','Bianchi','HR',0,'2023-10-15 00:00:00'),('a.neri','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Andrea','Neri','Buying',0,'2023-10-15 00:00:00'),('f.pellegrini','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Federica','Pellegrini','Selling',0,'2023-10-15 00:00:00'),('g.ferretti','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Giulia','Ferretti','Selling',0,'2023-10-15 00:00:00'),('l.rossi','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Luca','Rossi','Buying',0,'2023-10-15 00:00:00'),('l.russo','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Laura','Russo','Selling',0,'2023-10-15 00:00:00'),('m.capelli','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Marco','Capelli','IT',0,'2023-10-15 00:00:00'),('mario.rossi','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Mario','Rossi','IT',0,'2023-10-15 00:00:00'),('p.santoro','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Paolo','Santoro','Buying',0,'2023-10-15 00:00:00'),('primonome','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','primo','nome','Administration',0,'2023-10-14 00:00:00'),('s.martini','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Stefano','Martini','IT',0,'2023-10-15 00:00:00'),('v.moretti','a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3','Valentina','Moretti','Administration',0,'2023-10-15 00:00:00');
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-11-02 16:52:12
