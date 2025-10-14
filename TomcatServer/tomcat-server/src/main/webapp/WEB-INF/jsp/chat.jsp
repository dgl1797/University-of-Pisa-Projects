<%@ page contentType="text/html;charset=UTF-8" %>
  <%@ page import="com.unipi.dsmt.app.dtos.MessageChatDTO" %>
    <%@ page import="java.util.List" %>
      <%@ page import="java.text.SimpleDateFormat" %>
        <%@ page import="com.unipi.dsmt.app.utils.AccessController" %>
          <!DOCTYPE html>
          <html lang="en">

          <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <link rel="stylesheet" href="css/chat.css?v=1.16">
            <title>Chat page</title>
            <% String currentUsername=AccessController.getUsername(request); %>
              <%String username=(String) request.getAttribute("username");%>
                <% List<MessageChatDTO> messageList = (List<MessageChatDTO>) request.getAttribute("messageList"); %>
                    <% boolean isOnline=(boolean) request.getAttribute("isOnline"); %>
                      <script src="js/chat.js?v=1.35" defer></script>
          </head>

          <body style="margin: 0px;">
            <jsp:include page="/WEB-INF/jsp/components/nav_bar.jsp">
              <jsp:param name="loggedUser" value="<%=AccessController.getUsername(request)%>" />
            </jsp:include>
            <div class="page">
              <div class="modal" id="modal">
                <div class="container">
                  <h3>This chat has been deleted by <%=username%>, You will be redirected to your Homepage</h3>
                  <form method="get" action="${pageContext.request.contextPath}/home">
                    <button>Ok</button>
                  </form>
                </div>
              </div>
              <div class="chat-name" id="<%=username%>">
                <% String flagType="flag" + (isOnline ? " connected" : "" ); %>
                  <div class="<%= flagType %>"></div>
                  <h1 id="other_user">
                    <%= username %>
                  </h1>
              </div>
              <div class="board-container">
                <div class="messages-board">
                  <% if(messageList !=null){ %>
                    <% for(MessageChatDTO message : messageList){ %>
                      <% String className="message-card " + (currentUsername.equals(message.getSender()) ? "sender"
                        : "receiver" ); %>
                        <div class="<%= className %>">
                          <div class="message-box">
                            <label>
                              <%= message.getContent() %>
                            </label>
                            <label>
                              <%= message.getCreationTime().toString() %>
                            </label>
                          </div>
                        </div>
                        <%}%>
                          <%}%>
                </div>
              </div>
              <div class="centerize-msgbar">
                <div class="message-bar">
                  <input type="text" placeholder="Send Message" onkeypress="handleSend(event)" />
                  <img src="icons/send.png">
                </div>
              </div>
            </div>
          </body>

          </html>