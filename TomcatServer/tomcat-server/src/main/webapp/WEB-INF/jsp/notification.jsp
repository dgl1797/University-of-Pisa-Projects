<%@ page contentType="text/html;charset=UTF-8" %>
  <%@ page import="com.unipi.dsmt.app.dtos.NotificationDTO" %>
    <%@ page import="java.util.List" %>
      <%@ page import="com.unipi.dsmt.app.utils.AccessController" %>
        <!DOCTYPE html>
        <html lang="en">

        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <link rel="stylesheet" href="css/notification.css?v=1.5">
          <title>Notification page</title>
          <% List<NotificationDTO> notificationList = (List<NotificationDTO>) request.getAttribute("notificationList");
              %>
              <script>
                const currentPath = "${pageContext.request.contextPath}";
              </script>
        </head>

        <body style="margin: 0px;">
          <jsp:include page="/WEB-INF/jsp/components/nav_bar.jsp">
            <jsp:param name="loggedUser" value="<%=AccessController.getUsername(request)%>" />
          </jsp:include>
          <div class="page">
            <div class="centerize-board">
              <div class="notifications-board" id="notification-board">
                <% if (notificationList !=null){ %>
                  <% for(NotificationDTO notification : notificationList){ %>
                    <a href="${pageContext.request.contextPath}/chat?chatID=<%=notification.getChatID()%>"
                      class="anchor">
                      <div class="notification-box" id="<%=notification.getSender()%>">
                        <label> You have <%=notification.getCount()%> new messages from: <%= notification.getSender() %>
                        </label>
                      </div>
                    </a>
                    <%}%>
                      <%}%>
              </div>
            </div>
          </div>
        </body>

        </html>