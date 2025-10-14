<%@ page contentType="text/html;charset=UTF-8" %>
  <%@ page import="com.unipi.dsmt.app.dtos.UserProfileDTO" %>
    <%@ page import="com.unipi.dsmt.app.dtos.ChatStorageDTO" %>
      <%@ page import="java.util.List" %>
        <%@ page import="com.unipi.dsmt.app.utils.AccessController" %>
          <!DOCTYPE html>
          <html lang="en">

          <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <link rel="stylesheet" href="css/profile.css?v=1.15">
            <script src="js/profile.js?v=1.3"></script>
            <title>Profile page</title>
            <% UserProfileDTO requestingUser=(UserProfileDTO) request.getAttribute("user_info"); %>
              <% List<ChatStorageDTO> chatList =(List<ChatStorageDTO>) request.getAttribute("chatList"); %>
          </head>

          <body style="margin: 0px;">
            <jsp:include page="/WEB-INF/jsp/components/nav_bar.jsp">
              <jsp:param name="loggedUser" value="<%=AccessController.getUsername(request)%>" />
            </jsp:include>
            <div class="page">
              <div class="fields-container">
                <div class="field">
                  <h1>Username</h1>
                  <label>
                    <%= requestingUser.getUsername() %>
                  </label>
                </div>
                <div class="field">
                  <h1>Name</h1>
                  <label>
                    <%= requestingUser.getName() %>
                  </label>
                </div>
                <div class="field">
                  <h1>Surname</h1>
                  <label>
                    <%= requestingUser.getSurname() %>
                  </label>
                </div>
                <div class="field">
                  <h1>Department</h1>
                  <label>
                    <%= requestingUser.getDepartment() %>
                  </label>
                </div>
              </div>
              <div class="chat-history-container">
                <h1>Open Chats</h1>
                <div class="grid">
                  <% for(ChatStorageDTO chat : chatList){ %>
                    <div class="chat-box">
                      <a href="${pageContext.request.contextPath}/chat?chatID=<%=chat.getId_chat()%>">
                        <div class="chat-card" id="<%=chat.getUsername()%>">
                          <h1>
                            <%= chat.getUsername() %>
                          </h1>
                          <label>
                            <%= chat.getLastMessageTime() %>
                          </label>
                          <% String className="flag" + (chat.getOnlineState() ? " connected" : "" ) ; %>
                            <div class="<%= className %>"></div>
                        </div>
                      </a>
                      <button
                        onclick='deleteChat("<%=chat.getId_chat()%>","${pageContext.request.contextPath}/chat", "<%=chat.getUsername()%>")'>🗑</button>
                    </div>
                    <%}%>
                </div>
              </div>
            </div>
          </body>

          </html>