<%@ page contentType="text/html;charset=UTF-8" %>
  <%@ page import="com.unipi.dsmt.app.utils.ErrorHandler" %>
    <!DOCTYPE html>
    <html lang="en">

    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <link rel="stylesheet" href="css/authentication_page.css?v=1.7">
      <script src="js/authentication_page.js?v=1.1"></script>
      <title>Sign Up</title>
    </head>

    <body style="margin: 0px;">

      <div class="page">
        <% String error=ErrorHandler.getPopupErrorMessage(request); if(error !=null){ %>
          <label class="error-label" id="error-label">
            <p>
              <%= error %>
            </p>
            <button class="error-button" onclick="hideLabel(event)">X</button>
          </label>
          <%} ErrorHandler.clearPopupErrorMessage(request);%>

            <form method="post" action="${pageContext.request.contextPath}/signup" class="login-card">
              <h1 id="title">Sign up</h1>

              <div class="half">
                <div class="input">
                  <label>Name</label>
                  <input type="text" name="name" placeholder="First Name" required />
                </div>
                <div class="input">
                  <label>Surname</label>
                  <input type="text" name="surname" placeholder="Second Name" required />
                </div>
              </div>
              <div class="input">
                <label>Username</label>
                <input type="text" name="username" placeholder="username" required />
              </div>
              <div class="input">
                <label>Password</label>
                <input type="password" name="password" placeholder="password" required />
              </div>
              <div class="input">
                <label>Choose your department:</label>
                <select class="departments" id="departments" name="department">
                  <option value="IT">Information Technologies</option>
                  <option value="HR">Human Resources</option>
                  <option value="Administration">Administration</option>
                  <option value="Buying">Buying</option>
                  <option value="Selling">Selling</option>
                </select>
              </div>
              <button type="submit">Sign Up</button>
              <p>You have an account? <a href="${pageContext.request.contextPath}/login">Sign in</a></p>
            </form>
      </div>

    </body>

    </html>