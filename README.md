# Concept
This project aims at developing an AI-augmented Search Engine for Papers and scientific works that are crucial for R&D. The idea comes from the necessity for new searching tools able to further analyze the body of a paper and gather specific information to better sort-out unecessary research and let the researcher focus on what it is really interesting to him.

The project uses notebooks as a data preparation tool with fast debugging and then stores gathered information in MongoDB to further operate multiple researches and scrape libraries containing Scientific Works. It then tokenizes and stores the papers in [Qdrant](https://qdrant.tech/) vector-based DBMS, allowing semantic similarity scores for information retrieval.

Finally, it provides AI-enhanced tools exploiting GPT to summarize a subset of interesting papers to specifically create a resume answering how the selected papers could provide useful intels for the search prompt. Everything is delivered in an easy-to-use GUI developed with SvelteJS and Svelte-Kit to allow also server-side renderings

Further information can be checked in the [Project's Documentation](./Project_Documentation.pdf)