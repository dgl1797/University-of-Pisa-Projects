import axios from "axios";
import { useCallback, useEffect, useState, useMemo } from "react";
import emptyIcon from "./empty.jpg";

const Tabs = {
  all: 0,
  suggestions: 1,
};

const useApp = () => {
  const [cards, setcards] = useState([]);
  const [selectedCards, setSelectedCards] = useState(
    new Array(8).fill({
      name: "empty",
      CID: -1,
      imageUrl: emptyIcon,
    })
  );
  const [selectedTab, setSelectedTab] = useState(Tabs.all);
  const [modalOpen, setModalOpen] = useState(false);
  const [infoedCard, setInfoedCard] = useState(undefined);
  const [rankResult, setRankResult] = useState("Calculating...");
  const [rankerOpen, setRankerOpen] = useState(false);

  useEffect(() => {
    if (selectedTab === Tabs.all) {
      axios
        .get("http://localhost:5000/api/cards")
        .then((response) => {
          setcards(response.data);
        })
        .catch((err) => {
          console.log(err);
        });
    }
  }, [selectedTab]);

  useEffect(() => {
    if (selectedTab === Tabs.suggestions) {
      axios
        .get("http://localhost:5000/api/suggestions", {
          params: {
            c1: selectedCards?.[0]?.CID ?? undefined,
            c2: selectedCards?.[1]?.CID ?? undefined,
            c3: selectedCards?.[2]?.CID ?? undefined,
            c4: selectedCards?.[3]?.CID ?? undefined,
            c5: selectedCards?.[4]?.CID ?? undefined,
            c6: selectedCards?.[5]?.CID ?? undefined,
            c7: selectedCards?.[6]?.CID ?? undefined,
            c8: selectedCards?.[7]?.CID ?? undefined,
          },
        })
        .then((response) => {
          setcards(response.data);
        })
        .catch((err) => {
          console.log(err);
        });
    }
  }, [selectedTab, selectedCards]);

  const selectedCardsLength = useMemo(() => {
    return selectedCards.reduce((sum, card) => (card.CID === -1 ? sum : sum + 1), 0);
  }, [selectedCards]);

  useEffect(() => {
    if (selectedCardsLength === 8) {
      axios
        .get("http://localhost:5000/api/base_classification", {
          params: {
            c1: selectedCards?.[0]?.CID ?? undefined,
            c2: selectedCards?.[1]?.CID ?? undefined,
            c3: selectedCards?.[2]?.CID ?? undefined,
            c4: selectedCards?.[3]?.CID ?? undefined,
            c5: selectedCards?.[4]?.CID ?? undefined,
            c6: selectedCards?.[5]?.CID ?? undefined,
            c7: selectedCards?.[6]?.CID ?? undefined,
            c8: selectedCards?.[7]?.CID ?? undefined,
          },
        })
        .then((response) => {
          setRankResult(response.data);
        })
        .catch((err) => {
          console.log(err);
        });
    } else {
      setRankResult("Calculating...");
    }
    // eslint-disable-next-line
  }, [selectedCardsLength]);

  const addCard = useCallback(
    (card) => {
      if (selectedCards.map((sc) => sc.CID).includes(card.CID)) return;
      if (selectedCardsLength >= 8) return;
      const newList = selectedCards;
      for (let _card in newList) {
        if (newList[_card].CID === -1) {
          newList[_card] = card;
          break;
        }
      }
      setSelectedCards([...newList]);
    },
    [selectedCards, selectedCardsLength]
  );

  const removeCard = useCallback(
    (card) => {
      const newList = selectedCards;
      for (let _card in newList) {
        if (newList[_card].CID === card.CID) {
          newList[_card] = {
            name: "empty",
            CID: -1,
            imageUrl: emptyIcon,
          };
        }
      }
      setSelectedCards([...newList]);
    },
    [selectedCards]
  );

  const openModal = useCallback((card) => {
    setInfoedCard(card);
    setModalOpen(true);
  }, []);
  const closeModal = useCallback(() => {
    setModalOpen(false);
  }, []);
  const closeRanker = useCallback(() => {
    setRankerOpen(false);
  }, []);
  const openRanker = useCallback(() => {
    setRankerOpen(true);
  }, []);

  return {
    cards,
    selectedCards,
    removeCard,
    addCard,
    Tabs,
    selectedTab,
    setSelectedTab,
    selectedCardsLength,
    openModal,
    closeModal,
    modalOpen,
    infoedCard,
    rankResult,
    rankerOpen,
    closeRanker,
    openRanker,
  };
};
export default useApp;
