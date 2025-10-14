<%@ page contentType="text/html;charset=UTF-8" %>
  <%@ page import="com.unipi.dsmt.app.utils.ErrorHandler" %>
    <!DOCTYPE html>
    <html lang="en">

    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link rel="stylesheet" href="css/authentication_page.css?v=1.3">
      <script src="js/authentication_page.js?v=1.1"></script>
      <title>Welcome</title>
    </head>

    <body style="margin: 0px;">

      <div class="page">
        <% String error=ErrorHandler.getPopupErrorMessage(request); if(error !=null){ %>
          <label class="error-label" id="error-label">
            <p>
              <%= error %>
            </p>
            <button class="error-button" onclick="hideLabel()">X</button>
          </label>
          <%} ErrorHandler.clearPopupErrorMessage(request);%>
            <form method="post" action="${pageContext.request.contextPath}/login" class="login-card">
              <h1 id="title">Log In</h1>
              <div class="input">
                <label>Username</label>
                <input type="text" name="username" required />
              </div>
              <div class="input">
                <label>Password</label>
                <input type="password" name="password" required />
              </div>
              <button type="submit">Log In</button>
              <p>You don't have an account? <a href="${pageContext.request.contextPath}/signup">Sign Up</a></p>
            </form>
      </div>

    </body>

    </html>