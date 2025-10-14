import Popup from "./Popup";
import Ranker from "./Ranker";
import style from "./style.module.css";
import useApp from "./useApp.js";

function App() {
  const {
    cards,
    selectedCards,
    addCard,
    removeCard,
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
  } = useApp();

  return (
    <div className={style.page}>
      <Popup isOpen={modalOpen} onClose={closeModal} infoCard={infoedCard} />
      <Ranker isOpen={rankerOpen} onClose={closeRanker} selCards={selectedCards} />
      <div className={style.container}>
        <div className={style.header}>
          <div className={style.appTitle}>CR-DECK BUILDER</div>
          <div
            className={`${style.tab}${selectedTab === Tabs.all ? " " + style.activeTab : ""}`}
            onClick={() => setSelectedTab(Tabs.all)}
          >
            ALL CARDS
          </div>
          <div
            className={`${style.tab}${
              selectedTab === Tabs.suggestions ? " " + style.activeTab : ""
            }`}
            onClick={() => setSelectedTab(Tabs.suggestions)}
          >
            SUGGESTIONS
          </div>
          <div className={style.appTitle}>
            {selectedCardsLength === 8 ? (
              rankResult === "Competitive" ? (
                <div className={style.clickable} onClick={openRanker}>{`${rankResult} 🛈`}</div>
              ) : (
                rankResult
              )
            ) : (
              `add ${8 - selectedCardsLength} cards to see the ranking`
            )}
          </div>
        </div>
        <div className={style.contentsgrid}>
          <div className={`${style.content1} ${style.deckgrid}`}>
            {selectedCards.map((card, index) => (
              <div className={style.card} key={card.CID !== -1 ? card.CID : card.CID - index}>
                <img
                  className={style.image}
                  src={card.imageUrl}
                  alt={card.name}
                  onClick={() => removeCard(card)}
                ></img>
                <div>
                  <div>{card.name}</div>
                  {card.CID !== -1 && (
                    <div>
                      ATKSpeed: {card?.hps ?? card?.hitspeed ?? undefined}
                      {"\n"}HP:{" "}
                      {card?.hitpoints?.[card?.hitpoints?.length - 1] ??
                        card?.hitpoints ??
                        undefined}
                      {"\n"}DMG:{" "}
                      {card?.damage?.[card?.damage?.length - 1] ?? card?.damage ?? undefined}
                    </div>
                  )}
                </div>
              </div>
            ))}
          </div>
          <div className={`${style.content2} ${style.cardsgrid}`}>
            {cards.map((card) => {
              return (
                <div className={style.card} key={card.CID}>
                  <img
                    className={style.image}
                    src={card.imageUrl}
                    alt={card.name}
                    onClick={() => addCard(card)}
                  ></img>
                  <div className={style.info} onClick={() => openModal(card)}>
                    info
                  </div>
                </div>
              );
            })}
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
