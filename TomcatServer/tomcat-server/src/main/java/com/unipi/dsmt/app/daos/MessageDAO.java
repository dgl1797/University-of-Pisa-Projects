package com.unipi.dsmt.app.daos;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import com.unipi.dsmt.app.dtos.MessageChatDTO;

public class MessageDAO {
  private Connection messageConnection = null;

  public MessageDAO(Connection db) {
    messageConnection = db;
  }

  public ArrayList<MessageChatDTO> getMessagesFromChatId(int chatId) throws SQLException {
    ArrayList<MessageChatDTO> result = new ArrayList<>();
    String sqlString = "SELECT content, sender, creationTime FROM message WHERE chatID=? ORDER BY creationTime";
    PreparedStatement statement = messageConnection.prepareStatement(sqlString);
    statement.setInt(1, chatId);
    ResultSet set = statement.executeQuery();
    while (set.next()) {
      MessageChatDTO message = new MessageChatDTO(set.getString("content"), set.getString("sender"),
          set.getTimestamp("creationTime"));
      result.add(message);
    }
    return result;
  }

  public void deleteMessageFromChatID(int chatID) throws SQLException {
    String sqlString = "DELETE FROM message WHERE chatID=?";
    PreparedStatement statement = messageConnection.prepareStatement(sqlString);
    statement.setInt(1, chatID);
    statement.executeUpdate();
  }

  public void deleteMessageFromMessageID(int messageID) throws SQLException {
    String sqlString = "DELETE FROM message WHERE id=?";
    PreparedStatement statement = messageConnection.prepareStatement(sqlString);
    statement.setInt(1, messageID);
    statement.executeUpdate();
  }
}
