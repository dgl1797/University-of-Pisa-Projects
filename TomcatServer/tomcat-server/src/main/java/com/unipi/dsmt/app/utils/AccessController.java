package com.unipi.dsmt.app.utils;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import jakarta.servlet.http.HttpServletRequest;

public class AccessController {
  private static final int EXPIRATION_TIME = 86400000; // Tempo di scadenza del JWT (1 giorno)
  private static final String SECRET_KEY = System.getenv("JWT_SECRET"); // Chiave segreta per firmare il JWT

  /**
   * TOKEN CONTROLLER
   */

  public static void setToken(HttpServletRequest request, String username) {

    Map<String, Object> claims = new HashMap<>();
    claims.put("username", username);

    String token = Jwts.builder()
        .setClaims(claims)
        .setExpiration(new Date(System.currentTimeMillis() + EXPIRATION_TIME))
        .signWith(SignatureAlgorithm.HS512, SECRET_KEY)
        .compact();

    request.getSession().setAttribute("TOKEN", token);

  }

  public static String getToken(HttpServletRequest request) {
    String token = (String) request.getSession().getAttribute("TOKEN");
    if (token == null)
      return null;

    Claims claims = Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
    Date expirationDate = claims.getExpiration();
    Date now = new Date();
    if (expirationDate.before(now)) {
      request.getSession().setAttribute("TOKEN", null);
      return null;
    }

    return token;
  }

  /** END TOKEN CONTROLLER */

  /**
   * PASSWORD CONTROLLER
   * 
   * @throws NoSuchAlgorithmException
   */
  public static String encryptPassword(String plaintext) throws NoSuchAlgorithmException {
    byte[] byteString = MessageDigest.getInstance("SHA-256").digest(plaintext.getBytes(StandardCharsets.UTF_8));
    StringBuilder hexString = new StringBuilder(2 * byteString.length);
    for (int i = 0; i < byteString.length; i++) {
      String hex = Integer.toHexString(0xff & byteString[i]);
      if (hex.length() == 1) {
        hexString.append('0');
      }
      hexString.append(hex);
    }
    return hexString.toString();
  }

  /** END PASSWORD CONTROLLER */

  public static String getUsername(HttpServletRequest request) {
    String token = (String) request.getSession().getAttribute("TOKEN");
    if (token == null)
      return null;
    Claims claims = Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token).getBody();
    String username = (String) claims.get("username");
    return username;
  }
}
