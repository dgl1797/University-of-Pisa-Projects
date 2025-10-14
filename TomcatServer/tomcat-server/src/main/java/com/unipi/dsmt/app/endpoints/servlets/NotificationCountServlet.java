package com.unipi.dsmt.app.endpoints.servlets;

import java.io.IOException;
import java.sql.Connection;

import com.unipi.dsmt.app.daos.NotificationDAO;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "NotificationCount", value = "/notificationcount")
public class NotificationCountServlet extends HttpServlet {
  @Override
  protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
    try {
      NotificationDAO notificationDAO = new NotificationDAO(
          (Connection) getServletContext().getAttribute("databaseConnection"));
      String requestingUser = AccessController.getUsername(req);
      int number = notificationDAO.getNotificationCountForUser(requestingUser);
      resp.setStatus(200);
      resp.getWriter().write(String.format("{\"count\": %d}", number));
    } catch (Exception e) {
      ErrorHandler.safeDispatchToErrorPage(req, resp, e);
    }
  }
}
