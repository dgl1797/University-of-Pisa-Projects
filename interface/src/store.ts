import { writable } from "svelte/store";

const urlString = writable(new URL("http://undefined.undefined/"));
const selectedLinks = writable<string[]>([]);
const quackSays = writable<string>("");

export { urlString, selectedLinks, quackSays };
