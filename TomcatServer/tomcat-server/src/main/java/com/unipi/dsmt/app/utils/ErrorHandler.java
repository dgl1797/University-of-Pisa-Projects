package com.unipi.dsmt.app.utils;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public class ErrorHandler {
  public static void safeDispatchToErrorPage(HttpServletRequest request, HttpServletResponse response, Exception e) {
    try {
      System.out.println("[Server] -> An unexpected error has occurred:");
      e.printStackTrace();
      request.getRequestDispatcher("/WEB-INF/jsp/error_page.jsp").forward(request, response);
    } catch (Exception err) {
      err.printStackTrace();
      return;
    }
  }

  public static void setPopupErrorMessage(HttpServletRequest request, String message) {
    request.getSession().setAttribute("error", message);
  }

  public static String getPopupErrorMessage(HttpServletRequest request) {
    return (String) request.getSession().getAttribute("error");
  }

  public static void clearPopupErrorMessage(HttpServletRequest request) {
    request.getSession().removeAttribute("error");
  }
}
