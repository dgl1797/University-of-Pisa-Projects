import React, { memo } from "react";
import classes from "../pbrowser.module.css";

enum suggestions {
  CLONES = 1,
  LIKES = 2,
}

type SuggestionsProps = {
  activeSuggestion: suggestions;
  setActiveSuggestion: (newValue: suggestions) => void;
};

const Suggestions = ({ activeSuggestion, setActiveSuggestion }: SuggestionsProps) => {
  return (
    <div className={classes.suggestionsContainer}>
      <button
        className={activeSuggestion === suggestions.CLONES ? classes.active : ""}
        onClick={() => setActiveSuggestion(suggestions.CLONES)}
      >
        BY CLONES
      </button>
      <button
        className={activeSuggestion === suggestions.LIKES ? classes.active : ""}
        onClick={() => setActiveSuggestion(suggestions.LIKES)}
      >
        BY LIKES
      </button>
    </div>
  );
};

export default memo(Suggestions);
