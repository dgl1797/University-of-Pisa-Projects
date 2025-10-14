const nws = new WebSocket(`ws://localhost:8083/notification?username=${currentUsername}`);
const audio = new Audio("sounds/alert.wav");

function createNotificationComponent(sender, chatID) {
  const newNotificationComponent = document.createElement("a");
  newNotificationComponent.classList.add("anchor");
  newNotificationComponent.href = `${currentPath}/chat?chatID=${chatID}`;
  newNotificationComponent.innerHTML = `
    <div class="notification-box" id="${sender}">
      <label> You have 1 new messages from: ${sender}</label>
    </div>
  `;
}

async function appendAndIncrementNotificationComponent(notificationBoxID, /**@type {HTMLDivElement}*/ board) {
  const notificationBox = document.getElementById(notificationBoxID);
  if (!notificationBox) {
    const chatID = await getChatID(notificationBoxID);
    board.appendChild(createNotificationComponent(notificationBoxID, chatID));
  } else {
    const notificationLabel = notificationBox.querySelector("label");
    const currentText = notificationLabel.innerHTML;
    const currentCount = parseInt(currentText.split(" ")[3]) + 1;
    notificationLabel.innerHTML = ` You have ${currentCount} messages from: ${notificationBoxID}`;
  }
}

nws.onmessage = (event) => {
  const { type, who, from } = JSON.parse(event.data);
  if (type === "online_notification") {
    const flagElement = document.getElementById(who)?.querySelector("div") ?? undefined;
    flagElement && !flagElement.classList.contains("connected") && flagElement.classList.add("connected");
    // CASE PROFILE || HOME || DEPARTMENT (div.grid || div.users-board)
    let listElement = document.querySelector("div.grid");
    listElement && listElement.prepend(document.getElementById(who).parentNode.parentNode);
    listElement = document.querySelector("div.users-board");
    listElement && listElement.prepend(document.getElementById(who));
    return;
  }
  if (type === "offline_notification") {
    const flagElement = document.getElementById(who)?.querySelector("div") ?? undefined;
    flagElement && flagElement.classList.contains("connected") && flagElement.classList.remove("connected");
    // CASE PROFILE || HOME || DEPARTMENT (div.grid || div.users-board)
    let listElement = document.querySelector("div.grid");
    listElement && listElement.append(document.getElementById(who).parentNode.parentNode);
    listElement = document.querySelector("div.users-board");
    listElement && listElement.append(document.getElementById(who));
    return;
  }
  if (type === "message_notification") {
    audio.currentTime = 0;
    audio.play();
    const notificationLabel = document.querySelector("div.notification > label");
    notificationLabel.innerHTML = parseInt(notificationLabel.innerHTML) + 1;
    const notificationBoard = document.getElementById("notification-board");
    notificationBoard && appendAndIncrementNotificationComponent(from, notificationBoard);
    return;
  }
  if (type === "chat_deletion") {
    const endpoints = new URL(location.href).pathname.split("/");
    const isChat = endpoints[endpoints.length - 1] === "chat";
    if (isChat) {
      document.getElementById("modal").classList.add("visible");
    } else {
      const listElement = document.querySelector("div.grid");
      listElement && listElement.removeChild(document.getElementById(who).parentNode.parentNode);
    }
  }
};

async function getChatID(sender) {
  const response = await fetch(`http://localhost:80/app/chatID?sender=${sender}`, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  });
  const { chatID } = await response.json();
  return chatID;
}

async function getNotificationNumber() {
  const response = await fetch(`http://localhost:80/app/notificationcount`, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  });
  const data = await response.json();
  return data;
}

getNotificationNumber().then((data) => {
  const notificationLabel = document.querySelector("div.notification > label");
  notificationLabel.innerHTML = data.count;
});

function handleLogout(event, redirectPath) {
  event.preventDefault();
  nws.send(JSON.stringify({ type: "logout" }));
  location.href = redirectPath;
}
