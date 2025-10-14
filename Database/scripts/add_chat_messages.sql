USE DSMTDB;

-- Get all existing user usernames
SET @usernames = (SELECT GROUP_CONCAT(username SEPARATOR ',') FROM user);

-- Create sample messages between existing users
DELIMITER //
CREATE PROCEDURE InsertSampleMessagesUnique()
BEGIN
    DECLARE done INT DEFAULT 0;
    DECLARE user1 VARCHAR(16);
    DECLARE user2 VARCHAR(16);

    -- Declare a cursor for selecting random user pairs
    DECLARE cur CURSOR FOR SELECT u1.username AS user1, u2.username AS user2
                           FROM user AS u1
                           JOIN user AS u2
                           WHERE u1.username < u2.username
                           ORDER BY RAND()
                           LIMIT 10;

    -- Declare continue handler
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = 1;

    OPEN cur;

    message_loop: LOOP
        FETCH cur INTO user1, user2;
        IF done THEN
            LEAVE message_loop;
        END IF;

        -- Insert some sample messages in a new chat
        INSERT INTO chat (user1, user2) VALUES (user1, user2);
        SET @chatID = LAST_INSERT_ID();

        INSERT INTO message (content, sender, chatID) VALUES
            ('Hello, this is the start of our chat!', user1, @chatID);
        INSERT INTO message (content, sender, chatID) VALUES
            ('Hi there! Let''s have a great conversation.', user2, @chatID);
    END LOOP;

    CLOSE cur;
END;
//
DELIMITER ;

-- Call the unique procedure to insert sample messages
CALL InsertSampleMessagesUnique();
