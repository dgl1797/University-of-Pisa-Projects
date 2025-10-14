const cws = new WebSocket(`ws://localhost:8081/chat?username=${currentUsername}`);

const messageList = [...document.querySelectorAll(".messages-board > .message-card").values()];
const chatID = new URL(location.href).searchParams.get("chatID");
const messagesBoard = document.querySelector(".messages-board");

function format(/** @type {Date}*/ date) {
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hours = String(date.getHours()).padStart(2, "0");
  const minutes = String(date.getMinutes()).padStart(2, "0");
  const seconds = String(date.getSeconds()).padStart(2, "0");
  return `${year}:${month}:${day} ${hours}:${minutes}:${seconds}.0`;
}

function appendMessageComponent(message, instant, type) {
  const newMessageComponent = document.createElement("div");
  newMessageComponent.id = instant;
  newMessageComponent.classList.add("message-card");
  newMessageComponent.classList.add(type);
  newMessageComponent.innerHTML = `
    <div class="message-box">
    <label>
      ${message}
    </label>
    <label>
      ${format(new Date(instant))}
    </label>
    </div>
  `;
  messagesBoard.appendChild(newMessageComponent);
  messagesBoard.scrollTo({ behavior: "smooth", top: messagesBoard.scrollHeight });
  messageList.push(newMessageComponent);
}

function handleSend(event) {
  if (event.key === "Enter") {
    const other_username = document.getElementById("other_user").innerText;
    const message = event.target.value;
    const instant = Date.now();
    event.target.value = "";
    appendMessageComponent(message, instant, "sender");
    cws.send(
      JSON.stringify({
        username: other_username,
        message,
        chatID,
        timestampMillis: instant,
      })
    );
  }
}

cws.onmessage = (event) => {
  const message = JSON.parse(event.data);
  if (message.type && message.type === "message") {
    appendMessageComponent(message.content, Date.now(), "receiver");
    return;
  }
  if (message.type && message.type === "error") {
    const problematicMessage = message.messageID ? document.getElementById(message.messageID) : undefined;
    problematicMessage && problematicMessage.querySelector(".message-box").classList.add("message-error");
    return;
  }
};

cws.onerror = (event) => {
  console.error(event);
};

messagesBoard.scrollTo({ behavior: "instant", top: messagesBoard.scrollHeight });
