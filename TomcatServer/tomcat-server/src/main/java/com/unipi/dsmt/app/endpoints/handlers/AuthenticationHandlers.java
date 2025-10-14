package com.unipi.dsmt.app.endpoints.handlers;

import java.security.NoSuchAlgorithmException;
import java.sql.Date;
import java.util.Optional;

import com.unipi.dsmt.app.dtos.UserLoginDTO;
import com.unipi.dsmt.app.entities.User;
import com.unipi.dsmt.app.enums.Department;
import com.unipi.dsmt.app.utils.AccessController;

import jakarta.servlet.http.HttpServletRequest;

public class AuthenticationHandlers {
  public static User unpackPostSignup(HttpServletRequest request) throws NoSuchAlgorithmException {
    String username = Optional.ofNullable(request.getParameter("username")).orElse("");
    String password = Optional.ofNullable(request.getParameter("password")).orElse("");
    password = AccessController.encryptPassword(password);
    String firstname = Optional.ofNullable(request.getParameter("name")).orElse("");
    String surname = Optional.ofNullable(request.getParameter("surname")).orElse("");
    String department = Optional.ofNullable(request.getParameter("department")).orElse("");

    User userInfo = new User(username, password, firstname, surname, true, new Date(System.currentTimeMillis()),
        Department.valueOf(department));

    System.out.println(String.format("[Server] -> Received SignUp Request: %s", userInfo.toString()));
    return userInfo;
  }

  public static UserLoginDTO unpackPostLogin(HttpServletRequest request) throws NoSuchAlgorithmException {
    String username = Optional.ofNullable(request.getParameter("username")).orElse("");
    String password = Optional.ofNullable(request.getParameter("password")).orElse("");
    password = AccessController.encryptPassword(password);

    UserLoginDTO result = new UserLoginDTO(username, password);
    System.out.println(String.format("[Server] -> Received LogIn Request: %s", result.toString()));
    return result;
  }
}
