package com.unipi.dsmt.app.endpoints.servlets;

import java.sql.Connection;
import java.util.List;
import java.util.function.Predicate;

import com.unipi.dsmt.app.daos.UserDAO;
import com.unipi.dsmt.app.dtos.UserProfileDTO;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet(name = "DepartmentServlet", value = "/department")
public class DepartmentServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) {
        try {
            final String currentUsername = AccessController.getUsername(request);
            UserDAO userDAO = new UserDAO((Connection) getServletContext().getAttribute("databaseConnection"));
            // get users work in that department
            String department = (String) request.getParameter("name");
            List<UserProfileDTO> users = userDAO.getUsersFromDepartment(department);
            users = users.stream().filter(new Predicate<UserProfileDTO>() {
                @Override
                public boolean test(UserProfileDTO user) {
                    return !user.getUsername().equals(currentUsername);
                }
            }).toList();
            List<UserProfileDTO> onlineList = users.stream().filter(new Predicate<UserProfileDTO>() {
                @Override
                public boolean test(UserProfileDTO user) {
                    return user.isOnline_flag();
                }
            }).toList();
            request.setAttribute("users", users);
            request.setAttribute("onlineUsers", onlineList);
            request.getRequestDispatcher("/WEB-INF/jsp/department.jsp").forward(request, response);
        } catch (Exception e) {
            ErrorHandler.safeDispatchToErrorPage(request, response, e);
        }
    }
}
