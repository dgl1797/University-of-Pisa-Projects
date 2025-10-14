<!-- semplice testo diviso in sezioni -->
<script lang="ts">
  import { onMount } from "svelte";
  // @ts-ignore
  import { selectedLinks } from "../../store"
  import Rollerloader from "../../components/rollerloader.svelte";

  // @ts-ignore
  export let data;
  let isLoading: boolean = false;
  let doc: HTMLDivElement
  $:isLoading

  onMount(async () => {
    isLoading = true;
    const response = await (await fetch(`${data.host}/summary`, {
      method: "PUT",
      // @ts-ignore
      body: JSON.stringify({links: $selectedLinks, query: data.queryString}),
      headers:{
        "content-type": "application/json"
      }
    })).json();
    isLoading = false;
    doc.innerHTML = response?.code ? `<h1 style="color: red;">Error: ${response.code}</h1><p>${response.message}</p>` : response;
  })
</script>

{#if isLoading}
  <Rollerloader />
{/if}
<div class="page">
  <div class="doc" bind:this={doc}></div>
</div>

<style>
  :root {
    --navbar-heigth: 7rem;
    --footbar-height: 3rem;
  }
  .page{
    margin-top: calc(var(--navbar-heigth) - 0.5rem);
    display: flex;
    justify-content: center;
    align-items: center;
  }
  .doc{
    width: 90%;
    height: calc(100vh - var(--navbar-heigth) - 4rem - 1rem);
    overflow-y: auto;
    padding: 2rem;
    margin-bottom: 1rem;
    border-radius: 10px;
    background-color: white;
    box-shadow: 0px 0px 36px -5px rgba(0,0,0,0.4);
  }
  .doc::-webkit-scrollbar{
    width: 10px;
  }
  .doc::-webkit-scrollbar-track{
    width: 100%;
    background: none;
  }
  .doc::-webkit-scrollbar-thumb{
    width: 100%;
    background: rgb(77, 0, 100);
    border-radius: 2em;
  }
  .doc::-webkit-scrollbar-thumb:hover{
    background: blue;
  }
</style>