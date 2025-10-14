<script lang="ts">
  import RollerLoader from "../../components/rollerloader.svelte"
  import Card from '../../components/card.svelte';
  import type { CardContent } from '../../models/models.js';
  import { selectedLinks, quackSays } from "../../store";
  import QuackAudio from "../../assets/quack.mp3";

  export let data;
  const quack = typeof Audio !== "undefined" && new Audio(QuackAudio);

  let cardsContainer: HTMLDivElement;
  let currentList: CardContent[] = data.responseData;
  let oldResponse: CardContent[] = data.responseData;
  let isLoading: boolean = false;
  let currentPage: number = 0;
  $:currentList
  $:isLoading
  $:if(data.responseData !== oldResponse){
    currentList = data.responseData;
    cardsContainer.scrollTop = 0;
  }

  const handleSelect = (link: string) => {
    if($selectedLinks.length === 4) {
      if($quackSays === ""){
        quack && quack.play()
        $quackSays = "subscribe to select more"
        setTimeout(() => $quackSays = "", 3000)
      }
      return false;
    };
    $selectedLinks = $selectedLinks.concat(link);
    return true;
  }
  const handleDeselect = (link: string) => {
    $selectedLinks = $selectedLinks.filter((l) => l!==link);
  }
  const checkScrollEnd = async (scrollEvent: UIEvent) => {
    const caller = scrollEvent.target as HTMLDivElement;
    if(Math.round(caller.offsetHeight + caller.scrollTop)-Math.round(caller.scrollHeight) >= 0){
      const resultPromise = addContent(++currentPage);
      isLoading = true;
      const responseData = await resultPromise;
      isLoading = false;
      responseData.filter((rd) => !currentList.map(cl => cl.link).includes(rd.link))
      currentList = currentList.concat(...responseData);
      return;
    }
    return;
  }
  const addContent = async (page: number): Promise<CardContent[]> => {
    try{
      const response = await (
      await fetch(`${data.host}/search?query=${data.query}&page=${page}`, {
        method: "GET",
        headers: {
          "content-type": "application/json",
        },
      })).json();
      return response;
    }catch(error){
      quack && quack.play();
      $quackSays = "failed to fetch items";
      setTimeout(() => $quackSays="", 3000);
      return [];
    }
  }

</script>

<div class="cardsContainer" on:scroll={checkScrollEnd} bind:this={cardsContainer}>
  {#each currentList as paper}
    <Card 
      {...paper} 
      onSelectCallback={handleSelect} 
      onDeselectCallback={handleDeselect} 
      initialSelected={$selectedLinks.includes(paper.link)} 
    />
  {/each}
  {#if isLoading}
    <RollerLoader />    
  {/if}
</div>
<div class="counter">Selected: {$selectedLinks.length}/4</div>
<div class="counter total">Displayed: {currentList.length}</div>

<style>
  :root {
    --navbar-heigth: 7rem;
    --footbar-height: 3rem;
  }
  .counter{
    position: fixed;
    bottom: 1.3%;
    right: 3.4%;
    font-size: 15px;
    font-weight: 600;
    color: rgb(44, 0, 238);
    user-select: none;
  }
  .total{
    left: 45%;
  }
  .cardsContainer {
    margin-top: var(--navbar-heigth);
    display: grid;
    grid-template-columns: auto auto auto;
    gap: 2rem;
    height: calc(100vh - var(--navbar-heigth) - 0.5rem);
    margin-left: 2rem;
    margin-right: 3rem;
    overflow: hidden;
    overflow-y: auto;
    height: 560px;
    column-gap: 2rem;
  }
  @media screen and (max-width: 800px) {
    .cardsContainer {
      grid-template-columns: auto auto;
      margin-left: 1.5rem;
    }
  }
  @media screen and (max-width: 500px) {
    .cardsContainer {
      grid-template-columns: auto;
      margin-left: 0.5rem;
      margin-right: 0.5rem;
    }
    .total{
      left: 0;
    }
  }
</style>
