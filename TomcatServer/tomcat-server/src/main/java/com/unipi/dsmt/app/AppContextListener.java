package com.unipi.dsmt.app;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.TimeZone;

import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;

public class AppContextListener implements ServletContextListener {
  private Connection db = null;

  private Connection initDB(String host, int port, String dbname) {
    try {
      Class.forName("com.mysql.cj.jdbc.Driver");
      db = DriverManager.getConnection(
          String.format("jdbc:mysql://%s:%d/%s", host, port, dbname),
          System.getenv("DB_ROOT"), System.getenv("DB_PASS"));
      System.out.println(String.format("[MYSQL] -> Connected to database %s", dbname));
      return db;
    } catch (SQLException sqle) {
      System.out.println("SQLException: " + sqle.getMessage());
      System.out.println("SQLState: " + sqle.getSQLState());
      System.out.println("VendorError: " + sqle.getErrorCode());
      return null;
    } catch (ClassNotFoundException cnfe) {
      System.out.println("Inner Exception: " + cnfe.getMessage());
      System.out.println("Stack\n:" + cnfe.getStackTrace());
      return null;
    }
  }

  public void contextInitialized(ServletContextEvent event) {
    Connection sqldao = initDB(
        System.getenv("DB_HOST"),
        Integer.parseInt(System.getenv("DB_PORT")),
        System.getenv("DB_NAME"));

    // Get the servlet context and store the database connection
    ServletContext context = event.getServletContext();
    context.setAttribute("databaseConnection", sqldao);
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Rome"));
  }

  public void contextDestroyed(ServletContextEvent event) {
    // Cleanup code if needed when the web application is destroyed
    ServletContext context = event.getServletContext();
    context.removeAttribute("databaseConnection");
  }
}