package com.unipi.dsmt.app.endpoints.servlets;

import java.io.IOException;
import java.sql.Connection;

import com.unipi.dsmt.app.daos.UserDAO;
import com.unipi.dsmt.app.endpoints.handlers.AuthenticationHandlers;
import com.unipi.dsmt.app.entities.User;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "SignUp", value = "/signup")
public class SignUpServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws IOException, ServletException {

        try {
            String targetJSP = "/WEB-INF/jsp/signup.jsp";
            RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
            requestDispatcher.forward(request, response);
        } catch (Exception e) {
            ErrorHandler.safeDispatchToErrorPage(request, response, e);
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) {
        try {
            UserDAO userDAO = new UserDAO((Connection) getServletContext().getAttribute("databaseConnection"));
            User userInfo = AuthenticationHandlers.unpackPostSignup(request);
            String resultMessage = userDAO.save(userInfo);

            if (resultMessage != "") {
                ErrorHandler.setPopupErrorMessage(request, resultMessage);
                response.sendRedirect(request.getContextPath() + "/signup");
                return;
            }

            AccessController.setToken(request, userInfo.getUsername());
            response.sendRedirect(request.getContextPath() + "/home");
        } catch (Exception e) {
            ErrorHandler.safeDispatchToErrorPage(request, response, e);
        }
    }
}
