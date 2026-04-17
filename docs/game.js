(() => {
  const SUITS = ["S", "H", "D", "C"];
  const RANKS = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"];
  const SUIT_SYMBOL = { S: "♠", H: "♥", D: "♦", C: "♣" };
  const FOUNDATION_PLACEHOLDERS = { S: "♠", H: "♥", D: "♦", C: "♣" };

  const stockEl = document.getElementById("stock");
  const wasteEl = document.getElementById("waste");
  const foundationsEl = document.getElementById("foundations");
  const tableauEl = document.getElementById("tableau");
  const scoreEl = document.getElementById("score");
  const movesEl = document.getElementById("moves");
  const elapsedEl = document.getElementById("elapsed");
  const messageEl = document.getElementById("message");
  const drawCountEl = document.getElementById("draw-count");
  const newGameBtn = document.getElementById("new-game-btn");
  const autoBtn = document.getElementById("auto-btn");
  const overlapDownEl = document.getElementById("overlap-down");
  const overlapUpEl = document.getElementById("overlap-up");
  const overlapDownValueEl = document.getElementById("overlap-down-value");
  const overlapUpValueEl = document.getElementById("overlap-up-value");

  let state = null;
  let timerId = null;
  let dragPayload = null;

  function suitColor(suit) {
    return ["H", "D"].includes(suit) ? "red" : "black";
  }

  function rankValue(rank) {
    return RANKS.indexOf(rank) + 1;
  }

  function makeDeck() {
    const deck = [];
    for (const suit of SUITS) {
      for (const rank of RANKS) {
        deck.push({
          id: `${rank}${suit}`,
          rank,
          suit,
          color: suitColor(suit),
        });
      }
    }
    return deck;
  }

  function getCard(id) {
    return state.deckMap[id] || null;
  }

  function topCard(arr) {
    return arr.length ? arr[arr.length - 1] : null;
  }

  function shuffle(array) {
    const copy = [...array];
    for (let i = copy.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [copy[i], copy[j]] = [copy[j], copy[i]];
    }
    return copy;
  }

  function createState(drawCount = 3) {
    const deck = makeDeck();
    const deckMap = Object.fromEntries(deck.map(card => [card.id, card]));
    const shuffled = shuffle(deck.map(c => c.id));

    const tableau = [];
    let pos = 0;
    for (let i = 1; i <= 7; i++) {
      const ids = shuffled.slice(pos, pos + i);
      const up = Array(i - 1).fill(false).concat(true);
      tableau.push({ ids, up });
      pos += i;
    }

    return {
      deckMap,
      drawCount,
      stock: shuffled.slice(pos),
      waste: [],
      foundations: { S: [], H: [], D: [], C: [] },
      tableau,
      selected: null,
      moves: 0,
      score: 0,
      startedAt: Date.now(),
      message: "Bienvenue !",
      wonBonusApplied: false,
    };
  }

  function flipLastIfNeeded(pile) {
    if (pile.ids.length && !pile.up.some(Boolean)) {
      pile.up[pile.up.length - 1] = true;
    }
  }

  function isWon() {
    return SUITS.reduce((sum, suit) => sum + state.foundations[suit].length, 0) === 52;
  }

  function canPlaceOnTableau(movingId, targetId) {
    const moving = getCard(movingId);
    const target = getCard(targetId);
    return !!moving && !!target && moving.color !== target.color && rankValue(moving.rank) === rankValue(target.rank) - 1;
  }

  function canMoveToEmptyTableau(cardId) {
    const card = getCard(cardId);
    return !!card && card.rank === "K";
  }

  function canPlaceOnFoundation(cardId, suit) {
    const card = getCard(cardId);
    if (!card || card.suit !== suit) return false;
    const pile = state.foundations[suit];
    if (!pile.length) return card.rank === "A";
    const top = getCard(topCard(pile));
    return rankValue(card.rank) === rankValue(top.rank) + 1;
  }

  function removeTopTableauCard(col) {
    const pile = state.tableau[col];
    const id = pile.ids.pop() || null;
    if (id !== null) {
      pile.up.pop();
      flipLastIfNeeded(pile);
    }
    return id;
  }

  function moveTableauSequence(fromCol, fromIdx, toCol) {
    const src = state.tableau[fromCol];
    const dst = state.tableau[toCol];
    const movingIds = src.ids.slice(fromIdx);
    const movingUp = src.up.slice(fromIdx);
    src.ids = src.ids.slice(0, fromIdx);
    src.up = src.up.slice(0, fromIdx);
    flipLastIfNeeded(src);
    dst.ids.push(...movingIds);
    dst.up.push(...movingUp);
    state.moves += 1;
  }

  function sendToFoundation(fromType, opts = {}) {
    if (fromType === "waste") {
      const cardId = topCard(state.waste);
      if (!cardId) return setMessage("Aucune carte dans la défausse.");
      const suit = getCard(cardId).suit;
      if (!canPlaceOnFoundation(cardId, suit)) return setMessage("Cette carte ne peut pas aller en fondation.");
      state.waste.pop();
      state.foundations[suit].push(cardId);
      state.moves += 1;
      state.score += 10;
      return setMessage(`Carte envoyée en fondation : ${cardLabel(cardId)}`);
    }

    if (fromType === "tableau") {
      const pile = state.tableau[opts.col];
      if (!pile.ids.length || !pile.up[pile.up.length - 1]) return setMessage("Aucune carte visible à envoyer.");
      const cardId = topCard(pile.ids);
      const suit = getCard(cardId).suit;
      if (!canPlaceOnFoundation(cardId, suit)) return setMessage("Cette carte ne peut pas aller en fondation.");
      removeTopTableauCard(opts.col);
      state.foundations[suit].push(cardId);
      state.moves += 1;
      state.score += 10;
      return setMessage(`Carte envoyée en fondation : ${cardLabel(cardId)}`);
    }

    return setMessage("La carte est déjà en fondation.");
  }

  function drawFromStock() {
    if (!state.stock.length) {
      if (!state.waste.length) return setMessage("Le talon et la défausse sont vides.");
      state.stock = [...state.waste].reverse();
      state.waste = [];
      return setMessage("Le talon a été reconstitué.");
    }
    const n = Math.min(state.drawCount, state.stock.length);
    const drawn = state.stock.splice(0, n);
    state.waste.push(...drawn);
    state.moves += 1;
    setMessage(n === 1 ? "1 carte tirée." : `${n} cartes tirées.`);
  }

  function tryMoveToTableau(movingId, targetCol, sourceType, opts = {}) {
    const dst = state.tableau[targetCol];
    if (!movingId) return;

    if (!dst.ids.length) {
      if (!canMoveToEmptyTableau(movingId)) return setMessage("Seul un Roi peut aller sur une colonne vide.");
      if (sourceType === "waste") {
        state.waste.pop();
        dst.ids.push(movingId);
        dst.up.push(true);
      } else if (sourceType === "foundation") {
        state.foundations[opts.suit].pop();
        dst.ids.push(movingId);
        dst.up.push(true);
      } else if (sourceType === "tableau") {
        moveTableauSequence(opts.col, opts.idx, targetCol);
        state.score += 5;
        return setMessage(`Pile déplacée vers la colonne ${targetCol + 1}`);
      }
      state.moves += 1;
      state.score += 5;
      return setMessage(`Carte déplacée vers la colonne ${targetCol + 1}`);
    }

    const targetId = topCard(dst.ids);
    if (!canPlaceOnTableau(movingId, targetId)) return setMessage("Déplacement invalide.");

    if (sourceType === "waste") {
      state.waste.pop();
      dst.ids.push(movingId);
      dst.up.push(true);
    } else if (sourceType === "foundation") {
      state.foundations[opts.suit].pop();
      dst.ids.push(movingId);
      dst.up.push(true);
    } else if (sourceType === "tableau") {
      if (opts.col === targetCol) return;
      moveTableauSequence(opts.col, opts.idx, targetCol);
      state.score += 5;
      return setMessage(`Pile déplacée vers la colonne ${targetCol + 1}`);
    }

    state.moves += 1;
    state.score += 5;
    setMessage(`Carte déplacée vers la colonne ${targetCol + 1}`);
  }

  function tryMoveToFoundation(movingId, foundationSuit, sourceType, opts = {}) {
    if (!canPlaceOnFoundation(movingId, foundationSuit)) return setMessage("Déplacement invalide vers la fondation.");

    if (sourceType === "waste") {
      state.waste.pop();
    } else if (sourceType === "foundation") {
      if (opts.suit === foundationSuit) return;
      state.foundations[opts.suit].pop();
    } else if (sourceType === "tableau") {
      const pile = state.tableau[opts.col];
      if (opts.idx !== pile.ids.length - 1) return setMessage("Seule la carte du dessus peut aller en fondation.");
      removeTopTableauCard(opts.col);
    }

    state.foundations[foundationSuit].push(movingId);
    state.moves += 1;
    state.score += 10;
    setMessage(`Carte envoyée en fondation : ${cardLabel(movingId)}`);
  }

  function findAutoTableauTarget(movingId, excludeCol = null) {
    for (let col = 0; col < 7; col++) {
      if (excludeCol !== null && col === excludeCol) continue;
      const pile = state.tableau[col];
      if (!pile.ids.length && canMoveToEmptyTableau(movingId)) return col;
      if (pile.ids.length && canPlaceOnTableau(movingId, topCard(pile.ids))) return col;
    }
    return null;
  }

  function autoMoveSelected() {
    const sel = state.selected;
    if (!sel) return;

    if (sel.type === "waste") {
      const cardId = topCard(state.waste);
      if (!cardId) return;
      const suit = getCard(cardId).suit;
      if (canPlaceOnFoundation(cardId, suit)) {
        sendToFoundation("waste");
        state.selected = null;
        return;
      }
      const target = findAutoTableauTarget(cardId);
      if (target !== null) {
        tryMoveToTableau(cardId, target, "waste");
        state.selected = null;
        return;
      }
      setMessage("Aucun déplacement automatique possible.");
      return;
    }

    if (sel.type === "tableau") {
      const pile = state.tableau[sel.col];
      if (sel.idx >= pile.ids.length || !pile.up[sel.idx]) {
        state.selected = null;
        return;
      }
      const cardId = pile.ids[sel.idx];
      if (sel.idx === pile.ids.length - 1) {
        const suit = getCard(cardId).suit;
        if (canPlaceOnFoundation(cardId, suit)) {
          sendToFoundation("tableau", { col: sel.col });
          state.selected = null;
          return;
        }
      }
      const target = findAutoTableauTarget(cardId, sel.col);
      if (target !== null) {
        tryMoveToTableau(cardId, target, "tableau", { col: sel.col, idx: sel.idx });
        state.selected = null;
        return;
      }
      setMessage("Aucun déplacement automatique possible.");
      return;
    }

    if (sel.type === "foundation") {
      const cardId = topCard(state.foundations[sel.suit]);
      if (!cardId) return;
      const target = findAutoTableauTarget(cardId);
      if (target !== null) {
        tryMoveToTableau(cardId, target, "foundation", { suit: sel.suit });
        state.selected = null;
        return;
      }
      setMessage("Aucun déplacement automatique possible.");
    }
  }

  function handleClickStock() {
    state.selected = null;
    drawFromStock();
    updateAfterAction();
  }

  function handleClickWaste() {
    if (!state.waste.length) return;
    if (state.selected && state.selected.type === "waste") {
      state.selected = null;
      setMessage("Sélection annulée.");
    } else {
      state.selected = { type: "waste" };
      setMessage("Carte de la défausse sélectionnée.");
    }
    render();
  }

  function handleClickFoundation(suit) {
    if (!state.selected) {
      if (!state.foundations[suit].length) return setMessageAndRender("Fondation vide.");
      state.selected = { type: "foundation", suit };
      return setMessageAndRender(`Fondation ${SUIT_SYMBOL[suit]} sélectionnée.`);
    }

    const sel = state.selected;
    if (sel.type === "waste") {
      tryMoveToFoundation(topCard(state.waste), suit, "waste");
    } else if (sel.type === "tableau") {
      tryMoveToFoundation(state.tableau[sel.col].ids[sel.idx], suit, "tableau", { col: sel.col, idx: sel.idx });
    } else if (sel.type === "foundation") {
      if (sel.suit === suit) {
        state.selected = null;
        return setMessageAndRender("Sélection annulée.");
      }
      tryMoveToFoundation(topCard(state.foundations[sel.suit]), suit, "foundation", { suit: sel.suit });
    }
    state.selected = null;
    updateAfterAction();
  }

  function handleClickTableau(col, idx = null, empty = false) {
    if (empty) {
      if (!state.selected) return setMessageAndRender("Sélectionnez d’abord une carte.");
      const sel = state.selected;
      if (sel.type === "waste") {
        tryMoveToTableau(topCard(state.waste), col, "waste");
      } else if (sel.type === "foundation") {
        tryMoveToTableau(topCard(state.foundations[sel.suit]), col, "foundation", { suit: sel.suit });
      } else if (sel.type === "tableau") {
        tryMoveToTableau(state.tableau[sel.col].ids[sel.idx], col, "tableau", { col: sel.col, idx: sel.idx });
      }
      state.selected = null;
      return updateAfterAction();
    }

    const pile = state.tableau[col];
    if (idx === null || idx >= pile.ids.length) return;
    if (!pile.up[idx]) return setMessageAndRender("Cette carte est retournée.");

    if (!state.selected) {
      state.selected = { type: "tableau", col, idx };
      return setMessageAndRender(`Sélection dans la colonne ${col + 1}`);
    }

    const sel = state.selected;
    if (sel.type === "tableau" && sel.col === col && sel.idx === idx) {
      autoMoveSelected();
      if (state.message !== "Aucun déplacement automatique possible.") state.selected = null;
      return updateAfterAction(false);
    }

    if (sel.type === "waste") {
      tryMoveToTableau(topCard(state.waste), col, "waste");
    } else if (sel.type === "foundation") {
      tryMoveToTableau(topCard(state.foundations[sel.suit]), col, "foundation", { suit: sel.suit });
    } else if (sel.type === "tableau") {
      tryMoveToTableau(state.tableau[sel.col].ids[sel.idx], col, "tableau", { col: sel.col, idx: sel.idx });
    }
    state.selected = null;
    updateAfterAction();
  }

  function applyDragMove(source, target) {
    if (!source || !target) return;
    if (target.targetType === "tableau") {
      if (source.type === "waste") {
        tryMoveToTableau(topCard(state.waste), target.col, "waste");
      } else if (source.type === "foundation") {
        tryMoveToTableau(topCard(state.foundations[source.suit]), target.col, "foundation", { suit: source.suit });
      } else if (source.type === "tableau") {
        tryMoveToTableau(state.tableau[source.col].ids[source.idx], target.col, "tableau", { col: source.col, idx: source.idx });
      }
    }
    if (target.targetType === "foundation") {
      if (source.type === "waste") {
        tryMoveToFoundation(topCard(state.waste), target.suit, "waste");
      } else if (source.type === "foundation") {
        tryMoveToFoundation(topCard(state.foundations[source.suit]), target.suit, "foundation", { suit: source.suit });
      } else if (source.type === "tableau") {
        tryMoveToFoundation(state.tableau[source.col].ids[source.idx], target.suit, "tableau", { col: source.col, idx: source.idx });
      }
    }
    state.selected = null;
    updateAfterAction();
  }

  function cardLabel(id) {
    const card = getCard(id);
    return `${card.rank} ${SUIT_SYMBOL[card.suit]}`;
  }

  function setMessage(msg) {
    state.message = msg;
  }

  function setMessageAndRender(msg) {
    setMessage(msg);
    render();
  }

  function formatElapsed() {
    const secs = Math.max(0, Math.floor((Date.now() - state.startedAt) / 1000));
    return `${String(Math.floor(secs / 60)).padStart(2, "0")}:${String(secs % 60).padStart(2, "0")}`;
  }

  function assetUrl(cardId, back = false) {
    const name = back ? "back" : cardId;
    return `assets/cards/default/${name}.png`;
  }

  function stockAssetUrl() {
    return `assets/cards/default/stock.png`;
  }

  function setCssVar(name, value) {
    document.documentElement.style.setProperty(name, value);
  }

  function applyOverlapSettings() {
    const down = Number(overlapDownEl?.value ?? 90);
    const up = Number(overlapUpEl?.value ?? 72);
    setCssVar("--overlap-down-percent", String(down));
    setCssVar("--overlap-up-percent", String(up));
    if (overlapDownValueEl) overlapDownValueEl.textContent = String(down);
    if (overlapUpValueEl) overlapUpValueEl.textContent = String(up);
  }

  function buildCardElement({ cardId = null, faceUp = true, placeholder = false, selected = false, clickable = false, onClick = null, draggable = false, dragData = null, dropData = null, emptyLabel = "" }) {
    let el;
    if (placeholder) {
      el = document.createElement("div");
      el.className = "card-placeholder";
      el.textContent = emptyLabel;
    } else {
      el = document.createElement("div");
      const card = getCard(cardId);
      el.className = `card ${faceUp ? "" : "back"} ${selected ? "selected" : ""} ${clickable ? "clickable" : ""} ${draggable ? "draggable" : ""} ${faceUp && card && card.color === "red" ? "red" : ""}`.trim();

      if (faceUp) {
        const img = document.createElement("img");
        img.className = "card-image";
        img.alt = cardLabel(cardId);
        img.src = assetUrl(cardId);
        img.draggable = false;
        img.onerror = () => {
          img.remove();
          if (!el.querySelector(".fallback")) el.appendChild(buildFallbackCard(card));
        };
        el.appendChild(img);
      } else {
        const img = document.createElement("img");
        img.className = "card-image";
        img.alt = "Dos de carte";
        img.src = assetUrl(null, true);
        img.draggable = false;
        img.onerror = () => {
          img.remove();
          const filler = document.createElement("div");
          filler.className = "fallback";
          filler.innerHTML = `<div class="center">✶</div>`;
          el.appendChild(filler);
        };
        el.appendChild(img);
      }
    }

    if (selected && !placeholder) el.classList.add("selected");
    if (onClick) el.addEventListener("click", onClick);

    if (draggable) {
      el.setAttribute("draggable", "true");
      el.addEventListener("dragstart", event => {
        dragPayload = dragData;
        event.dataTransfer.setData("text/plain", JSON.stringify(dragData));
        event.dataTransfer.effectAllowed = "move";
      });
      el.addEventListener("dragend", clearDropHints);
    }

    if (dropData) {
      el.dataset.drop = JSON.stringify(dropData);
      wireDropzone(el);
    }

    return el;
  }

  function buildFallbackCard(card) {
    const wrap = document.createElement("div");
    wrap.className = "fallback";
    wrap.innerHTML = `
      <div class="corner">${card.rank}<br>${SUIT_SYMBOL[card.suit]}</div>
      <div class="center">${SUIT_SYMBOL[card.suit]}</div>
      <div class="corner bottom">${card.rank}<br>${SUIT_SYMBOL[card.suit]}</div>
    `;
    return wrap;
  }

  function wireDropzone(el) {
    el.addEventListener("dragenter", event => {
      event.preventDefault();
      el.classList.add("dropzone-active");
    });
    el.addEventListener("dragover", event => {
      event.preventDefault();
      event.dataTransfer.dropEffect = "move";
      el.classList.add("dropzone-active");
    });
    el.addEventListener("dragleave", () => el.classList.remove("dropzone-active"));
    el.addEventListener("drop", event => {
      event.preventDefault();
      el.classList.remove("dropzone-active");
      const target = JSON.parse(el.dataset.drop || "null");
      let source = dragPayload;
      if (!source) {
        try {
          source = JSON.parse(event.dataTransfer.getData("text/plain"));
        } catch (_) {
          source = null;
        }
      }
      clearDropHints();
      if (source && target) applyDragMove(source, target);
    });
  }

  function clearDropHints() {
    dragPayload = null;
    document.querySelectorAll(".dropzone-active").forEach(el => el.classList.remove("dropzone-active"));
  }

  function renderStock() {
    stockEl.innerHTML = "";
    if (state.stock.length) {
      const stack = document.createElement("div");
      stack.className = "stock-stack";
      stack.appendChild(Object.assign(document.createElement("div"), { className: "stock-shadow" }));
      stack.appendChild(Object.assign(document.createElement("div"), { className: "stock-shadow offset-1" }));
      stack.appendChild(Object.assign(document.createElement("div"), { className: "stock-shadow offset-2" }));
      const top = buildCardElement({ cardId: null, faceUp: false, clickable: true, onClick: handleClickStock });
      top.classList.add("stock-top");
      top.setAttribute("aria-label", "Pioche");
      const stockImg = document.createElement("img");
      stockImg.className = "stock-png";
      stockImg.alt = "";
      stockImg.setAttribute("aria-hidden", "true");
      stockImg.decoding = "async";
      stockImg.src = stockAssetUrl();
      stockImg.draggable = false;
      stockImg.onload = () => top.classList.add("has-stock-image");
      stockImg.onerror = () => stockImg.remove();
      top.appendChild(stockImg);
      stack.appendChild(top);
      stockEl.appendChild(stack);
      return;
    }
    const recycle = document.createElement("div");
    recycle.className = "recycle";
    recycle.textContent = state.waste.length ? "↺" : "∅";
    recycle.addEventListener("click", handleClickStock);
    stockEl.appendChild(recycle);
  }

  function renderWaste() {
    wasteEl.innerHTML = "";
    if (!state.waste.length) {
      wasteEl.appendChild(buildCardElement({ placeholder: true }));
      return;
    }
    const visible = state.waste.slice(-Math.min(state.waste.length, state.drawCount));
    const fan = document.createElement("div");
    fan.className = "waste-fan";
    fan.style.width = `${100 + (visible.length - 1) * 26}px`;
    visible.forEach((cardId, index) => {
      const isTop = index === visible.length - 1;
      const layer = document.createElement("div");
      layer.className = `waste-layer ${isTop ? "top-layer" : "under-layer"}`;
      layer.style.left = `${index * 26}px`;
      layer.appendChild(buildCardElement({
        cardId,
        faceUp: true,
        clickable: isTop,
        onClick: isTop ? handleClickWaste : null,
        selected: isTop && state.selected && state.selected.type === "waste",
        draggable: isTop,
        dragData: isTop ? { type: "waste" } : null,
      }));
      fan.appendChild(layer);
    });
    wasteEl.appendChild(fan);
  }

  function renderFoundations() {
    foundationsEl.innerHTML = "";
    for (const suit of SUITS) {
      const pile = state.foundations[suit];
      const selected = !!state.selected && state.selected.type === "foundation" && state.selected.suit === suit;
      if (!pile.length) {
        const empty = document.createElement("div");
        empty.className = "foundation-empty";
        empty.textContent = FOUNDATION_PLACEHOLDERS[suit];
        empty.dataset.drop = JSON.stringify({ targetType: "foundation", suit });
        empty.addEventListener("click", () => handleClickFoundation(suit));
        wireDropzone(empty);
        if (selected) empty.classList.add("selected");
        foundationsEl.appendChild(empty);
      } else {
        foundationsEl.appendChild(buildCardElement({
          cardId: topCard(pile),
          faceUp: true,
          clickable: true,
          onClick: () => handleClickFoundation(suit),
          selected,
          draggable: true,
          dragData: { type: "foundation", suit },
          dropData: { targetType: "foundation", suit },
        }));
      }
    }
  }

  function renderTableau() {
    tableauEl.innerHTML = "";
    state.tableau.forEach((pile, col) => {
      const colEl = document.createElement("div");
      colEl.className = "tableau-col";

      if (!pile.ids.length) {
        const empty = document.createElement("div");
        empty.className = "tableau-empty";
        empty.textContent = "K";
        empty.dataset.drop = JSON.stringify({ targetType: "tableau", col });
        empty.addEventListener("click", () => handleClickTableau(col, null, true));
        wireDropzone(empty);
        colEl.appendChild(empty);
        tableauEl.appendChild(colEl);
        return;
      }

      pile.ids.forEach((id, idx) => {
        const faceUp = pile.up[idx];
        const selected = !!state.selected && state.selected.type === "tableau" && state.selected.col === col && idx >= state.selected.idx;
        const wrap = document.createElement("div");
        wrap.className = `tableau-card-wrap ${idx === 0 ? "first-card" : faceUp ? "face-up-card" : "face-down-card"}`;
        wrap.appendChild(buildCardElement({
          cardId: id,
          faceUp,
          clickable: true,
          onClick: () => handleClickTableau(col, idx, false),
          selected,
          draggable: faceUp,
          dragData: faceUp ? { type: "tableau", col, idx } : null,
          dropData: faceUp && idx === pile.ids.length - 1 ? { targetType: "tableau", col } : null,
        }));
        colEl.appendChild(wrap);
      });
      tableauEl.appendChild(colEl);
    });
  }

  function renderStatus() {
    scoreEl.textContent = String(state.score);
    movesEl.textContent = String(state.moves);
    elapsedEl.textContent = formatElapsed();
    messageEl.textContent = state.message;
  }

  function render() {
    renderStatus();
    renderStock();
    renderWaste();
    renderFoundations();
    renderTableau();
  }

  function updateAfterAction(checkWin = true) {
    if (checkWin && isWon() && !state.wonBonusApplied) {
      state.wonBonusApplied = true;
      state.score += 100;
      state.message = "Bravo, partie gagnée !";
    }
    render();
  }

  function startTimer() {
    if (timerId) clearInterval(timerId);
    timerId = setInterval(() => {
      if (state) elapsedEl.textContent = formatElapsed();
    }, 1000);
  }

  function newGame() {
    state = createState(Number(drawCountEl.value));
    render();
    startTimer();
  }

  newGameBtn.addEventListener("click", newGame);
  overlapDownEl?.addEventListener("input", () => { applyOverlapSettings(); renderTableau(); });
  overlapUpEl?.addEventListener("input", () => { applyOverlapSettings(); renderTableau(); });
  drawCountEl.addEventListener("change", () => {
    state = createState(Number(drawCountEl.value));
    state.message = `Nouvelle partie en mode tirage ${drawCountEl.value}`;
    render();
    startTimer();
  });
  autoBtn.addEventListener("click", () => {
    autoMoveSelected();
    updateAfterAction(false);
  });
  document.addEventListener("dragover", event => event.preventDefault());
  document.addEventListener("drop", event => {
    if (!event.target.closest("[data-drop]")) clearDropHints();
  });

  applyOverlapSettings();
  newGame();
})();
