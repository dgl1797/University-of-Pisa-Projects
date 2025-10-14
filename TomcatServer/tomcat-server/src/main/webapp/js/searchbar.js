const usersList = [];
document.querySelectorAll(".users-board > .user-card").forEach((v) => {
  usersList.push({
    username: v.querySelector("h1").innerText,
    department: v.querySelector("h2").innerText,
    fullname: v.querySelector("h3").innerText,
    element: v,
  });
});

function handleChange(event) {
  const searchString = event?.target?.value?.toLowerCase() ?? "";
  usersList
    .filter((el) => !el.username.toLowerCase().includes(searchString) && !el.fullname.toLowerCase().includes(searchString))
    .forEach((el) => {
      el.element.classList.add("hidden");
    });
  usersList
    .filter((el) => el.username.toLowerCase().includes(searchString) || el.fullname.toLowerCase().includes(searchString))
    .forEach((el) => {
      el.element.classList.remove("hidden");
    });
}
