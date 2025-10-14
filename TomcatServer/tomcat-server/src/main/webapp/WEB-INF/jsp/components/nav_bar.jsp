<link rel="stylesheet" href="css/nav_bar.css?v=1.9">
<script src="js/navbar.js?v=1.34" defer></script>

<script>
  var currentUsername = '<%=request.getParameter("loggedUser")%>';
</script>
<nav class="navbar">
  <a href="${pageContext.request.contextPath}/home">Home</a>
  <a href="${pageContext.request.contextPath}/profile">Profile</a>
  <a onclick='handleLogout(event, "${pageContext.request.contextPath}/logout")'><img src="icons/close.png" width="30px"
      height="30px"></a>
  <div class="notification">
    <a href="${pageContext.request.contextPath}/notification"><img src="icons/bell.png" alt="" width="30px"
        height="30px"></a>
    <label>0</label>
  </div>
</nav>