package com.unipi.dsmt.app.filters;

import java.io.IOException;
import java.sql.Connection;

import com.unipi.dsmt.app.daos.UserDAO;
import com.unipi.dsmt.app.utils.AccessController;
import com.unipi.dsmt.app.utils.ErrorHandler;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class AccessFilter implements Filter {

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
      throws IOException, ServletException {

    try {
      HttpServletRequest req = (HttpServletRequest) request;
      HttpServletResponse resp = (HttpServletResponse) response;
      UserDAO userDAO = new UserDAO((Connection) req.getServletContext().getAttribute("databaseConnection"));
      String claimingUsername = AccessController.getUsername(req);
      String token = AccessController.getToken(req);
      if (token == null) {
        if (claimingUsername != null)
          userDAO.setOnlineFlag(false, claimingUsername);
        ErrorHandler.setPopupErrorMessage(req, "Invalid/Expired token. Login again.");
        resp.sendRedirect(req.getContextPath() + "/login");
        return;
      }
      chain.doFilter(request, response);
    } catch (Exception e) {
      ErrorHandler.safeDispatchToErrorPage((HttpServletRequest) request, (HttpServletResponse) response, e);
      return;
    }
  }

}
