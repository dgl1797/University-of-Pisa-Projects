const currentUrl = new URL(location.href);
const currentTag = currentUrl.searchParams.get("name");

const selectedTag = document.getElementById(currentTag);
if (selectedTag) {
  selectedTag.classList.add("selected");
  selectedTag.style.pointerEvents = "none";
}
