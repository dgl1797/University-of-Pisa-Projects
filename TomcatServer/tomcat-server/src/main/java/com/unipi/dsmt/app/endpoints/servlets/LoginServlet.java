package com.unipi.dsmt.app.endpoints.servlets;

import java.sql.Connection;

import com.unipi.dsmt.app.daos.UserDAO;
import com.unipi.dsmt.app.dtos.UserLoginDTO;
import com.unipi.dsmt.app.endpoints.handlers.AuthenticationHandlers;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "LoginServlet", value = "/login")
public class LoginServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        try {
            String token = AccessController.getToken(request);
            if (token == null) {
                request.getRequestDispatcher("/WEB-INF/jsp/login.jsp").forward(request, response);
                return;
            }
            response.sendRedirect(request.getContextPath() + "/home");
        } catch (Exception e) {
            ErrorHandler.safeDispatchToErrorPage(request, response, e);
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        try {
            UserDAO userDAO = new UserDAO((Connection) getServletContext().getAttribute("databaseConnection"));
            UserLoginDTO userInfo = AuthenticationHandlers.unpackPostLogin(request);

            if (!userDAO.exists(userInfo.getUsername())) {
                ErrorHandler.setPopupErrorMessage(request, "Username doesn't exist");
                response.sendRedirect(request.getContextPath() + "/login");
                return;
            }
            if (!userDAO.valid(userInfo.getUsername(), userInfo.getPassword())) {
                ErrorHandler.setPopupErrorMessage(request, "Incorrect Password");
                response.sendRedirect(request.getContextPath() + "/login");
                return;
            }
            AccessController.setToken(request, userInfo.getUsername());
            userDAO.setOnlineFlag(true, userInfo.getUsername());
            response.sendRedirect(request.getContextPath() + "/home");
        } catch (Exception e) {
            ErrorHandler.safeDispatchToErrorPage(request, response, e);
        }
    }
}
