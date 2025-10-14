package com.unipi.dsmt.app.endpoints.servlets;

import java.sql.Connection;
import java.sql.Date;
import java.util.List;

import com.unipi.dsmt.app.daos.ChatDAO;
import com.unipi.dsmt.app.daos.MessageDAO;
import com.unipi.dsmt.app.daos.NotificationDAO;
import com.unipi.dsmt.app.daos.UserDAO;
import com.unipi.dsmt.app.dtos.MessageChatDTO;
import com.unipi.dsmt.app.entities.Chat;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "ChatServlet", value = "/chat")
public class ChatServlet extends HttpServlet {
  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response) {
    try {
      ChatDAO chatDAO = new ChatDAO((Connection) getServletContext().getAttribute("databaseConnection"));
      MessageDAO messageDAO = new MessageDAO((Connection) getServletContext().getAttribute("databaseConnection"));
      UserDAO userDAO = new UserDAO((Connection) getServletContext().getAttribute("databaseConnection"));
      NotificationDAO notificationDAO = new NotificationDAO(
          (Connection) getServletContext().getAttribute("databaseConnection"));

      String currentUsername = AccessController.getUsername(request);

      int chatID = Integer.parseInt(request.getParameter("chatID"));
      chatDAO.validateChatIDWithUsername(chatID, currentUsername);
      notificationDAO.deleteNotificationFromChatID(chatID);
      String username = chatDAO.getDestinationOfChatID(chatID, currentUsername);
      request.setAttribute("username", username);

      boolean isOnline = userDAO.getOnlineStateOfUsername(username);
      request.setAttribute("isOnline", isOnline);

      List<MessageChatDTO> messageList = messageDAO.getMessagesFromChatId(chatID);
      request.setAttribute("messageList", messageList);

      request.getRequestDispatcher("/WEB-INF/jsp/chat.jsp").forward(request, response);
    } catch (Exception e) {
      ErrorHandler.safeDispatchToErrorPage(request, response, e);
    }
  }

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response) {
    try {
      ChatDAO chatDAO = new ChatDAO((Connection) getServletContext().getAttribute("databaseConnection"));

      String currentUsername = AccessController.getUsername(request);
      String username = (String) request.getParameter("username");

      // verifico se esiste già una chat tra currentUsername e username
      int retrievedChatID = chatDAO.getChatIDFromUser1User2(username, currentUsername);
      if (retrievedChatID == -1) {
        // se no creo una nuova chat
        Chat chat = new Chat(currentUsername, username, new Date(System.currentTimeMillis()));
        retrievedChatID = chatDAO.save(chat);
        response.sendRedirect(request.getContextPath() + "/chat?chatID=" + retrievedChatID);
        return;
      } else {
        // altrimenti faccio la get
        response.sendRedirect(request.getContextPath() + "/chat?chatID=" + retrievedChatID);
        return;
      }
    } catch (Exception e) {
      ErrorHandler.safeDispatchToErrorPage(request, response, e);
    }
  }

  @Override
  protected void doDelete(HttpServletRequest request, HttpServletResponse response){
    try {
      ChatDAO chatDAO = new ChatDAO((Connection) getServletContext().getAttribute("databaseConnection"));
			int chatID = Integer.parseInt(request.getParameter("chatID"));
      chatDAO.deleteChatFromChatID(chatID);
      response.setStatus(200);
    } catch (Exception e) {
      ErrorHandler.safeDispatchToErrorPage(request, response, e);
    }
  }
}
