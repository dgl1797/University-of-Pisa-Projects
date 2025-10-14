package com.unipi.dsmt.app.endpoints.servlets;

import java.sql.Connection;

import com.unipi.dsmt.app.daos.ChatDAO;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name="ChatID", value = "/chatID")
public class ChatIDServlet extends HttpServlet{
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp){
		try{
			ChatDAO chatDAO = new ChatDAO((Connection) getServletContext().getAttribute("databaseConnection"));
			String currentUsername = AccessController.getUsername(req);
			String sender = (String) req.getParameter("sender");
			int chatID = chatDAO.getChatIDFromUser1User2( currentUsername, sender );
			resp.setStatus(200);
      resp.getWriter().write(String.format("{\"chatID\": %d}", chatID));
		} catch(Exception e) {
			ErrorHandler.safeDispatchToErrorPage(req, resp, e);
		}
	}
}
