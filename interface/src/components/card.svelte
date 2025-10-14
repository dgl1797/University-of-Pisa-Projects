<script lang="ts">
  import {selectedLinks} from "../store"

  export let title: string;
  export let summary: string;
  export let link: string;
  export let date: string;
  export let authors: string[];
  export let onSelectCallback: (l: string) => boolean;
  export let onDeselectCallback: (l: string) => void;
  export let initialSelected: boolean | undefined = undefined;

  let formattedDate: any = new Date(date);
  formattedDate = `${formattedDate.getDate()}-${formattedDate.getMonth()}-${formattedDate.getFullYear()}`
  
  $: selected = initialSelected ?? false;

  const modifyStatus = (value: string) => {
    const isSelected = !selected && onSelectCallback(value);
    selected = isSelected;
    !selected && onDeselectCallback(value);
  };
</script>

<div class="card" class:active={selected} class:inactive={!selected}>
  <div class="date">{formattedDate}</div>
  <h2>
    <a href={link} target="_blank">{title}</a>
    [{authors.join(", ")}]
  </h2>
  <div class="summaryContainer">
    <div on:click={() => modifyStatus(link)} on:keypress class="overlay"></div>
    {summary}
  </div>
  <input
  type="checkbox"
  value={link}
  checked={selected}
  disabled={$selectedLinks.length === 4}
  on:change={() => modifyStatus(link)}
  />
</div>

<style>
  .date{
    position: absolute;
    top: 6px;
    text-align: center;
    width: 100%;
  }
  .overlay{
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    cursor: pointer;
  }
  .summaryContainer{
    position: relative;
    margin-left: 2rem;
    margin-right: 2rem;
    margin-top: 2rem;
    font-size: 13px;
  }
  .card {
    position: relative;
    width: 100%;
    height: 100%;
    border-radius: 10px;
  }
  .inactive {
    background-color: rgba(128, 128, 128, 0.8);
    box-shadow: 0px 0px 36px -5px rgba(0,0,0,0.4);
    transition: all 0.4s;
  }
  h2 {
    margin-left: 2rem;
    margin-right: 2rem;
    margin-top: 1.8rem;
    font-weight: normal;
    font-size: 10px;
  }
  h2 > a{
    font-weight: bold;
    font-size: large;
  }

  input {
    position: absolute;
    top: 0.5rem;
    right: 0.5rem;
    width: 15px;
    height: 15px;
  }
  .active {
    background-color: rgba(69, 183, 82, 0.8);
    box-shadow: 0px 0px 36px -5px rgba(69, 183, 82,0.4);
    transition: all 0.4s;
  }
</style>
