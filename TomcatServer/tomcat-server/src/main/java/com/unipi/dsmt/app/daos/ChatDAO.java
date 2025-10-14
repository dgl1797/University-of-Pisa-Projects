package com.unipi.dsmt.app.daos;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

import com.unipi.dsmt.app.dtos.ChatStorageDTO;
import com.unipi.dsmt.app.entities.Chat;

public class ChatDAO {
  private Connection chatConnection = null;

  public ChatDAO(Connection db) {
    chatConnection = db;
  }

  public int save(Chat chat) throws SQLException {
    String sqlString = "INSERT INTO chat(user1, user2, creationTime) VALUES (?, ?, ?)";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString, Statement.RETURN_GENERATED_KEYS);
    statement.setString(1, chat.getUser1());
    statement.setString(2, chat.getUser2());
    statement.setDate(3, chat.getCreationTime());
    int rowInserted = statement.executeUpdate();

    if (rowInserted == 1) {
      ResultSet generatedKey = statement.getGeneratedKeys();
      if (generatedKey.next()) {
        int insertedRowId = generatedKey.getInt(1);
        return insertedRowId;
      }
    }
    throw new SQLException("Chat not correctly inserted");
  }

  public ArrayList<ChatStorageDTO> getChatsFromUsername(String currentUsername) throws SQLException {
    ArrayList<ChatStorageDTO> result = new ArrayList<>();
    String sqlString = "SELECT latest_message.max_time, chat.user1, chat.user2, chat.id, CASE " +
        "WHEN chat.user1 = ? THEN chat.user2 ELSE chat.user1 END AS other_user, " +
        "user.onlineFlag FROM chat JOIN user ON (chat.user1=? AND chat.user2=user.username) OR (chat.user1=user.username AND chat.user2=?) "
        +
        "JOIN (SELECT chatID, MAX(creationTime) as max_time FROM message GROUP BY chatID) " +
        "AS latest_message ON chat.id = latest_message.chatID " +
        "WHERE chat.user1=? OR chat.user2=? ORDER BY user.onlineFlag DESC;";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString);
    statement.setString(1, currentUsername);
    statement.setString(2, currentUsername);
    statement.setString(3, currentUsername);
    statement.setString(4, currentUsername);
    statement.setString(5, currentUsername);
    ResultSet set = statement.executeQuery();
    while (set.next()) {
      String user = set.getString("user1").equals(currentUsername)
          ? set.getString("user2")
          : set.getString("user1");
      boolean isOnlineState = set.getBoolean("user.onlineFlag");
      ChatStorageDTO chat = new ChatStorageDTO(set.getInt("chat.id"), user, set.getDate("latest_message.max_time"),
          isOnlineState);
      result.add(chat);
    }
    return result;
  }

  public int getChatIDFromUser1User2(String user1, String user2) throws SQLException {
    String sqlString = "SELECT id FROM chat WHERE (user1 = ? and user2 = ?) or (user1=? and user2=?)";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString);
    statement.setString(1, user1);
    statement.setString(2, user2);
    statement.setString(3, user2);
    statement.setString(4, user1);
    ResultSet set = statement.executeQuery();
    if (set.next())
      return set.getInt("id");
    else
      return -1;
  }

  public void deleteChatFromChatID(int chatID) throws SQLException {
    String sqlString = "DELETE FROM chat WHERE id=?";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString);
    statement.setInt(1, chatID);
    statement.executeUpdate();
  }

  public void validateChatIDWithUsername(int chatID, String username) throws SQLException {
    String sqlString = "SELECT id FROM chat WHERE id=? and user1<>? and user2<>?";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString);
    statement.setInt(1, chatID);
    statement.setString(2, username);
    statement.setString(3, username);
    ResultSet set = statement.executeQuery();
    if (set.next())
      throw new SQLException("Invalid chat for username");

  }

  public String getDestinationOfChatID(int chatID, String currentUsername) throws SQLException {
    String sqlString = "SELECT id, user1, user2 FROM chat WHERE id=?";
    PreparedStatement statement = chatConnection.prepareStatement(sqlString);
    statement.setInt(1, chatID);
    ResultSet set = statement.executeQuery();
    if (!set.next())
      throw new SQLException("invalid chat id");
    String user1 = set.getString("user1");
    String user2 = set.getString("user2");
    return currentUsername.equals(user1) ? user2 : user1;
  }
}
