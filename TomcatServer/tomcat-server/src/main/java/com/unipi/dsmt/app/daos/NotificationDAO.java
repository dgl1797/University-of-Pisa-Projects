package com.unipi.dsmt.app.daos;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;

import com.unipi.dsmt.app.dtos.NotificationDTO;

public class NotificationDAO {
  private Connection notificationConnection = null;

  public NotificationDAO(Connection db) {
    notificationConnection = db;
  }

  public ArrayList<NotificationDTO> getNotificationFromUser(String user) throws SQLException {
    ArrayList<NotificationDTO> result = new ArrayList<>();
    String sqlString = "SELECT sender, chatID, Count(*) as nMessages FROM notification WHERE user=? GROUP BY chatID,sender ORDER BY chatID";
    PreparedStatement statement = notificationConnection.prepareStatement(sqlString);
    statement.setString(1, user);
    ResultSet set = statement.executeQuery();
    while (set.next()) {
      NotificationDTO notification = new NotificationDTO(set.getString("sender"), set.getInt("chatID"),
          set.getInt("nMessages"));
      result.add(notification);
    }
    return result;
  }

  public void deleteNotificationFromChatID(int chatID) throws SQLException {
    String sqlString = "DELETE FROM notification WHERE chatID=?";
    PreparedStatement statement = notificationConnection.prepareStatement(sqlString);
    statement.setInt(1, chatID);
    statement.executeUpdate();
  }

  public int getNotificationCountForUser(String user) throws SQLException {
    String sqlString = "SELECT COUNT(*) AS notification_number FROM notification WHERE user=?";
    PreparedStatement statement = notificationConnection.prepareStatement(sqlString);
    statement.setString(1, user);
    ResultSet set = statement.executeQuery();
    if (!set.next())
      throw new SQLException("Invalid user");
    return set.getInt("notification_number");
  }

}
