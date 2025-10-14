<script lang="ts">
  import { page } from "$app/stores";
  import { goto } from "$app/navigation";
  import DuckIcon from "../assets/main-icon.ico";
  import AudioQuack from "../assets/quack.mp3";
  import { urlString, selectedLinks, quackSays } from "../store";
  import { onMount } from "svelte";

  let input: HTMLInputElement;
  let queryString: string = "";
  const quack = typeof Audio !== "undefined" && new Audio(AudioQuack);

  const enterPressed = () => {
    const resultQueryString = encodeURIComponent(queryString ?? "");
    const currentUrl = new URL(window.location.href);
    currentUrl.pathname = "/paperselect";
    currentUrl.searchParams.set("query", resultQueryString);
    urlString.set(new URL(currentUrl));
    $selectedLinks = [];
    goto(currentUrl);
  };

  const buildSummary = () => {
    if($selectedLinks.length === 0){
      if($quackSays === ""){
        quack && quack.play();
        $quackSays = "select some papers"
        setTimeout(() => $quackSays = "", 3000)
      }
      return;
    }
    if($page.url.pathname === "/summary"){
      quack && quack.play();
      $quackSays = "issues? Try to select different papers!"
      setTimeout(() => $quackSays = "", 3000)
      return;
    }
    const resultQueryString = encodeURIComponent(queryString ?? "");
    const currentUrl = new URL(window.location.href);
    currentUrl.pathname = "/summary";
    currentUrl.searchParams.set("query", resultQueryString);
    urlString.set(new URL(currentUrl));
    goto(currentUrl);
  };

  const focusInput = () => {
    input.focus();
  };

  onMount(() => {
    const handlePopState = () => {
      const urlState = new URL(window.location.href);
      if (urlState.pathname === "/paperselect" || urlState.pathname === "/summary") {
        input.value = decodeURIComponent(urlState?.searchParams?.get("query") ?? "");
        queryString = input.value;
        urlString.set(urlState);
      }
    };

    handlePopState();
    window.addEventListener("popstate", handlePopState);

    return () => {
      window.removeEventListener("popstate", handlePopState);
    };
  });

  $: isLandingPage = $page.url.pathname === "/";
  $: isSummary = $page.url.pathname === "/summary";
</script>

<slot />
<div class:landing={isLandingPage} class:navbar={!isLandingPage}>
  {#if !isSummary}
  <div class="iconContainer">
    <img
      src={DuckIcon}
      alt="PaperAI"
      on:click={!isLandingPage ? () => buildSummary() : () => {}}
      on:keypress
      />
      <p>{isLandingPage ? "PaperAI" : "Build Summary"}</p>
      {#if !isLandingPage && $quackSays !== ""}
        <div class="message">{$quackSays}</div>
      {/if}
  </div>
  {/if}
  <div class="inputwrapper" on:click={() => focusInput()} on:keypress>
    <input
      type="text"
      placeholder="search"
      bind:value={queryString}
      bind:this={input}
      on:keypress={(event) => event.key === "Enter" && enterPressed()}
    />
    <div style="cursor: pointer;" on:click={() => enterPressed()} on:keypress>🔍</div>
  </div>
</div>

<style>
  .landing {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 70vh;
    place-items: center;
    place-content: center;
    gap: 4rem;
    transition: all 0.4s;
  }
  .inputwrapper {
    padding: 1rem;
    border-radius: 4rem;
    border: 2px solid rgb(77, 0, 100);
    display: flex;
    justify-content: center;
    align-items: center;
    cursor: text;
    background: white;
  }
  .inputwrapper > input {
    width: 100%;
    outline: none;
    border: none;
    background: none;
  }
  .inputwrapper > div {
    font-size: larger;
  }
  .landing > .inputwrapper {
    width: 50%;
    box-shadow: 0px 0px 49px -3px rgb(44, 44, 44);
  }
  .landing > .iconContainer {
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
  }
  .landing > .iconContainer > p {
    font-size: 24px;
    font-weight: 900;
    margin: 0;
  }
  .navbar {
    position: fixed;
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-items: center;
    top: 0;
    width: 100%;
    height: 5rem;
    transition: all 0.4s;
  }
  .navbar > .iconContainer {
    position: fixed;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    top: 0.5rem;
    right: 2rem;
    cursor: pointer;
    z-index: 12;
  }
  .navbar > .inputwrapper {
    width: 45%;
    box-shadow: 0px 0px 49px -3px rgb(44, 44, 44);
  }
  .navbar > .iconContainer > img {
    width: 4rem;
    height: 4rem;
    gap: 0px;
  }
  .navbar > .iconContainer > p {
    font-weight: bolder;
    margin: 0;
    pointer-events: none;
  }
  .message{
    position: absolute;
    opacity: 0;
    max-width: 100%;
    left: -68%;
    background: rgba(128, 128, 128, 0.8);
    box-shadow: 0px 0px 36px -5px rgba(0,0,0,0.4);
    animation: fadein .2s ease-in-out forwards;
    border-radius: 3rem;
    text-align: center;
    padding: .1rem;
    font-size: smaller;
    backdrop-filter: blur(5px);
  }
  @keyframes fadein {
    0%{
      opacity: 0;
    }
    100%{
      opacity: 1;
    }
  }
  @media screen and (max-width: 500px){
    .navbar > .iconContainer{
      top: 1.5rem;
      right: 0.5rem;
    }
    .navbar > .iconContainer > p {
      font-size: smaller;
    }
    .navbar > .iconContainer > img {
      width: 2rem;
      height: 2rem;
    }
    .navbar{
      justify-content: start;
    }
  }
</style>
