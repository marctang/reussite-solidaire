library(shiny)
library(htmltools)
library(jsonlite)

# =========================
# Helpers: deck + game logic
# =========================

suits <- c("S", "H", "D", "C")
ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
suit_symbols <- c(S = "♠", H = "♥", D = "♦", C = "♣")
rank_value <- function(rank) match(rank, ranks)
suit_color <- function(suit) if (suit %in% c("H", "D")) "red" else "black"

make_deck <- function() {
  deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)
  deck$id <- paste0(deck$rank, deck$suit)
  deck$color <- vapply(deck$suit, suit_color, character(1))
  deck
}

get_card <- function(id, deck) deck[deck$id == id, , drop = FALSE]

top_card <- function(vec) if (length(vec) == 0) NULL else vec[[length(vec)]]

new_game_state <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  deck <- make_deck()
  ids <- sample(deck$id)

  tableau <- vector("list", 7)
  pos <- 1
  for (i in 1:7) {
    pile_ids <- ids[pos:(pos + i - 1)]
    pos <- pos + i
    tableau[[i]] <- list(ids = pile_ids, up = c(rep(FALSE, i - 1), TRUE))
  }

  stock_ids <- if (pos <= length(ids)) ids[pos:length(ids)] else character(0)

  list(
    deck = deck,
    stock = stock_ids,
    waste = character(0),
    foundations = list(S = character(0), H = character(0), D = character(0), C = character(0)),
    tableau = tableau,
    selected = NULL,
    message = "Nouvelle partie."
  )
}

flip_if_needed <- function(pile) {
  if (length(pile$ids) > 0 && !any(pile$up)) {
    pile$up[length(pile$up)] <- TRUE
  }
  pile
}

is_tableau_build_valid <- function(moving_id, target_id, deck) {
  moving <- get_card(moving_id, deck)
  target <- get_card(target_id, deck)
  nrow(moving) == 1 && nrow(target) == 1 &&
    moving$color != target$color &&
    rank_value(moving$rank) == rank_value(target$rank) - 1
}

is_foundation_build_valid <- function(card_id, foundation_vec, deck) {
  card <- get_card(card_id, deck)
  if (nrow(card) == 0) return(FALSE)

  if (length(foundation_vec) == 0) return(card$rank == "A")

  topf <- get_card(top_card(foundation_vec), deck)
  nrow(topf) == 1 &&
    card$suit == topf$suit &&
    rank_value(card$rank) == rank_value(topf$rank) + 1
}

move_tableau_sequence <- function(state, from_col, start_idx, to_col) {
  source <- state$tableau[[from_col]]
  target <- state$tableau[[to_col]]

  moving_ids <- source$ids[start_idx:length(source$ids)]
  moving_up  <- source$up[start_idx:length(source$up)]

  source$ids <- if (start_idx > 1) source$ids[1:(start_idx - 1)] else character(0)
  source$up  <- if (start_idx > 1) source$up[1:(start_idx - 1)] else logical(0)

  target$ids <- c(target$ids, moving_ids)
  target$up  <- c(target$up, moving_up)

  state$tableau[[from_col]] <- flip_if_needed(source)
  state$tableau[[to_col]] <- target
  state
}

check_win <- function(state) sum(vapply(state$foundations, length, integer(1))) == 52

# =========================
# Asset handling
# =========================
# Shiny serves files from ./www automatically.
# We also support a legacy ./cards folder during local dev if present.

if (dir.exists("cards") && !dir.exists("www/cards")) {
  addResourcePath("cards", "cards")
}

card_asset_exists <- function(card_id) {
  file.exists(file.path("www", "cards", paste0(card_id, ".png"))) ||
    file.exists(file.path("cards", paste0(card_id, ".png")))
}

back_asset_exists <- function() {
  file.exists(file.path("www", "cards", "back.png")) ||
    file.exists(file.path("cards", "back.png"))
}

card_label <- function(card_id) {
  rank <- gsub("([0-9JQKA]+)([SHDC])", "\\1", card_id)
  suit <- gsub("([0-9JQKA]+)([SHDC])", "\\2", card_id)
  paste(rank, suit_symbols[[suit]])
}

source_payload <- function(type, col = NULL, idx = NULL, suit = NULL) {
  payload <- list(type = type)
  if (!is.null(col)) payload$col <- col
  if (!is.null(idx)) payload$idx <- idx
  if (!is.null(suit)) payload$suit <- suit
  toJSON(payload, auto_unbox = TRUE)
}

drop_payload <- function(target_type, col = NULL, suit = NULL) {
  payload <- list(target_type = target_type)
  if (!is.null(col)) payload$col <- col
  if (!is.null(suit)) payload$suit <- suit
  toJSON(payload, auto_unbox = TRUE)
}

card_div <- function(card_id = NULL, face_up = TRUE, clickable = FALSE, click_value = NULL,
                     selected = FALSE, deck = NULL, placeholder = FALSE,
                     draggable = FALSE, drag_payload = NULL, drop_payload_json = NULL,
                     extra_classes = character()) {
  classes <- c("card", extra_classes)
  if (!face_up) classes <- c(classes, "card-back")
  if (selected) classes <- c(classes, "selected")
  if (placeholder) classes <- c(classes, "placeholder")
  if (draggable && face_up && !placeholder) classes <- c(classes, "draggable-card")
  if (!is.null(drop_payload_json)) classes <- c(classes, "dropzone")

  attrs <- list(class = paste(classes, collapse = " "))
  if (clickable && !is.null(click_value)) {
    attrs$onclick <- sprintf("Shiny.setInputValue('card_click','%s',{priority:'event'})", click_value)
  }
  if (draggable && face_up && !placeholder && !is.null(drag_payload)) {
    attrs$draggable <- "true"
    attrs[['data-drag']] <- drag_payload
  }
  if (!is.null(drop_payload_json)) {
    attrs[['data-drop']] <- drop_payload_json
  }

  if (placeholder) {
    return(do.call(div, c(attrs, list())))
  }

  if (!face_up) {
    if (back_asset_exists()) {
      return(do.call(div, c(attrs, list(img(src = "cards/back.png", class = "card-img")))))
    }
    return(do.call(div, c(attrs, list(div(class = "fallback-back", "✶")))))
  }

  if (card_asset_exists(card_id)) {
    return(do.call(div, c(attrs, list(img(src = paste0("cards/", card_id, ".png"), class = "card-img")))))
  }

  card <- get_card(card_id, deck)
  col <- if (nrow(card) == 1 && card$color == "red") "#8d2e2b" else "#1f2328"
  do.call(div, c(attrs, list(
    style = sprintf("color:%s;", col),
    div(class = "fallback-rank tl", card_label(card_id)),
    div(class = "fallback-center", card_label(card_id)),
    div(class = "fallback-rank br", card_label(card_id))
  )))
}

render_stock <- function(state) {
  clickable <- !(length(state$stock) == 0 && length(state$waste) == 0)
  if (length(state$stock) > 0) {
    return(card_div(face_up = FALSE, clickable = clickable, click_value = "stock"))
  }
  div(class = "pile-wrap",
      div(class = "card recycle",
          onclick = if (clickable) "Shiny.setInputValue('card_click','stock',{priority:'event'})" else NULL,
          "↺"))
}

render_waste <- function(state) {
  if (length(state$waste) == 0) return(card_div(placeholder = TRUE))
  card_id <- top_card(state$waste)
  sel <- identical(state$selected, list(type = "waste"))
  card_div(
    card_id = card_id,
    face_up = TRUE,
    clickable = TRUE,
    click_value = "waste",
    selected = sel,
    deck = state$deck,
    draggable = TRUE,
    drag_payload = source_payload("waste"),
    drop_payload_json = NULL
  )
}

render_foundation <- function(state, suit) {
  pile <- state$foundations[[suit]]
  topid <- top_card(pile)
  sel <- identical(state$selected, list(type = "foundation", suit = suit))
  dp <- drop_payload("foundation", suit = suit)

  if (is.null(topid)) {
    return(div(
      class = paste("card foundation-empty dropzone", if (sel) "selected" else ""),
      `data-drop` = dp,
      onclick = sprintf("Shiny.setInputValue('card_click','foundation:%s',{priority:'event'})", suit),
      suit_symbols[[suit]]
    ))
  }

  card_div(
    card_id = topid,
    face_up = TRUE,
    clickable = TRUE,
    click_value = paste0("foundation:", suit),
    selected = sel,
    deck = state$deck,
    draggable = TRUE,
    drag_payload = source_payload("foundation", suit = suit),
    drop_payload_json = dp
  )
}

render_tableau_col <- function(state, col_index) {
  pile <- state$tableau[[col_index]]
  dp_col <- drop_payload("tableau", col = col_index)

  if (length(pile$ids) == 0) {
    return(div(
      class = "tableau-col",
      div(class = "card tableau-empty dropzone",
          `data-drop` = dp_col,
          onclick = sprintf("Shiny.setInputValue('card_click','tableau:%s:empty',{priority:'event'})", col_index),
          "K")
    ))
  }

  cards_ui <- lapply(seq_along(pile$ids), function(i) {
    id <- pile$ids[i]
    is_up <- pile$up[i]

    sel <- FALSE
    if (!is.null(state$selected) && identical(state$selected$type, "tableau")) {
      if (state$selected$col == col_index && i >= state$selected$idx) sel <- TRUE
    }

    div(
      class = "tableau-card-wrap",
      style = sprintf("margin-top:%spx;", if (i == 1) 0 else 28),
      card_div(
        card_id = id,
        face_up = is_up,
        clickable = TRUE,
        click_value = paste0("tableau:", col_index, ":", i),
        selected = sel,
        deck = state$deck,
        draggable = is_up,
        drag_payload = if (is_up) source_payload("tableau", col = col_index, idx = i) else NULL,
        drop_payload_json = if (i == length(pile$ids) && is_up) dp_col else NULL,
        extra_classes = if (i == length(pile$ids) && is_up) "top-drop-target" else character()
      )
    )
  })

  div(class = "tableau-col", cards_ui)
}

apply_drag_move <- function(st, source, target) {
  # Tableau target
  if (identical(target$target_type, "tableau")) {
    col <- as.integer(target$col)
    pile <- st$tableau[[col]]

    # empty tableau
    if (length(pile$ids) == 0) {
      if (identical(source$type, "waste")) {
        moving_id <- top_card(st$waste)
        if (is.null(moving_id) || get_card(moving_id, st$deck)$rank != "K") {
          st$message <- "Seul un Roi peut aller sur une colonne vide."
          return(st)
        }
        st$waste <- head(st$waste, -1)
        st$tableau[[col]]$ids <- c(st$tableau[[col]]$ids, moving_id)
        st$tableau[[col]]$up <- c(st$tableau[[col]]$up, TRUE)
        st$message <- paste("Roi déplacé vers la colonne", col)
        return(st)
      }
      if (identical(source$type, "tableau")) {
        moving_id <- st$tableau[[source$col]]$ids[source$idx]
        if (get_card(moving_id, st$deck)$rank != "K") {
          st$message <- "Seul un Roi peut aller sur une colonne vide."
          return(st)
        }
        st <- move_tableau_sequence(st, source$col, source$idx, col)
        st$message <- paste("Pile déplacée vers la colonne vide", col)
        return(st)
      }
      if (identical(source$type, "foundation")) {
        moving_id <- top_card(st$foundations[[source$suit]])
        if (is.null(moving_id) || get_card(moving_id, st$deck)$rank != "K") {
          st$message <- "Seul un Roi peut aller sur une colonne vide."
          return(st)
        }
        st$foundations[[source$suit]] <- head(st$foundations[[source$suit]], -1)
        st$tableau[[col]]$ids <- c(st$tableau[[col]]$ids, moving_id)
        st$tableau[[col]]$up <- c(st$tableau[[col]]$up, TRUE)
        st$message <- paste("Carte déplacée vers la colonne", col)
        return(st)
      }
      return(st)
    }

    target_id <- top_card(pile$ids)

    if (identical(source$type, "waste")) {
      moving_id <- top_card(st$waste)
      if (!is.null(moving_id) && is_tableau_build_valid(moving_id, target_id, st$deck)) {
        st$waste <- head(st$waste, -1)
        st$tableau[[col]]$ids <- c(st$tableau[[col]]$ids, moving_id)
        st$tableau[[col]]$up <- c(st$tableau[[col]]$up, TRUE)
        st$message <- paste("Carte déplacée vers colonne", col)
      } else st$message <- "Déplacement invalide."
      return(st)
    }

    if (identical(source$type, "tableau")) {
      if (source$col == col) return(st)
      moving_id <- st$tableau[[source$col]]$ids[source$idx]
      if (is_tableau_build_valid(moving_id, target_id, st$deck)) {
        st <- move_tableau_sequence(st, source$col, source$idx, col)
        st$message <- paste("Pile déplacée de", source$col, "vers", col)
      } else st$message <- "Déplacement invalide."
      return(st)
    }

    if (identical(source$type, "foundation")) {
      moving_id <- top_card(st$foundations[[source$suit]])
      if (!is.null(moving_id) && is_tableau_build_valid(moving_id, target_id, st$deck)) {
        st$foundations[[source$suit]] <- head(st$foundations[[source$suit]], -1)
        st$tableau[[col]]$ids <- c(st$tableau[[col]]$ids, moving_id)
        st$tableau[[col]]$up <- c(st$tableau[[col]]$up, TRUE)
        st$message <- paste("Carte de fondation déplacée vers colonne", col)
      } else st$message <- "Déplacement invalide."
      return(st)
    }
  }

  # Foundation target
  if (identical(target$target_type, "foundation")) {
    suit <- target$suit

    if (identical(source$type, "waste")) {
      card_id <- top_card(st$waste)
      if (!is.null(card_id) && is_foundation_build_valid(card_id, st$foundations[[suit]], st$deck)) {
        st$waste <- head(st$waste, -1)
        st$foundations[[suit]] <- c(st$foundations[[suit]], card_id)
        st$message <- paste("Carte envoyée en fondation :", card_id)
      } else st$message <- "Déplacement invalide vers la fondation."
      return(st)
    }

    if (identical(source$type, "tableau")) {
      pile <- st$tableau[[source$col]]
      if (source$idx != length(pile$ids) || !pile$up[source$idx]) {
        st$message <- "Seule la carte visible du dessus peut aller en fondation."
        return(st)
      }
      card_id <- pile$ids[source$idx]
      if (is_foundation_build_valid(card_id, st$foundations[[suit]], st$deck)) {
        pile$ids <- head(pile$ids, -1)
        pile$up <- head(pile$up, -1)
        st$tableau[[source$col]] <- flip_if_needed(pile)
        st$foundations[[suit]] <- c(st$foundations[[suit]], card_id)
        st$message <- paste("Carte envoyée en fondation :", card_id)
      } else st$message <- "Déplacement invalide vers la fondation."
      return(st)
    }
  }

  st$message <- "Déplacement impossible."
  st
}

# =========================
# UI
# =========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      :root {
        --nuees-cream: #f5efe4;
        --nuees-paper: #fbf7f0;
        --nuees-ink: #24343b;
        --nuees-moss: #6d8163;
        --nuees-sage: #a5b49a;
        --nuees-rust: #b56b5c;
        --nuees-gold: #d0b173;
        --nuees-shadow: rgba(34, 45, 41, 0.18);
      }
      body {
        background:
          radial-gradient(circle at top left, rgba(255,255,255,0.30), transparent 30%),
          linear-gradient(180deg, #d7dfcf 0%, #c7d1bc 30%, #9dae8d 100%);
        color: var(--nuees-ink);
        font-family: Georgia, 'Times New Roman', serif;
      }
      .container-fluid {
        max-width: 1320px;
        padding-top: 18px;
        padding-bottom: 40px;
      }
      .app-shell {
        background: rgba(251,247,240,0.50);
        border: 1px solid rgba(255,255,255,0.6);
        box-shadow: 0 18px 40px var(--nuees-shadow);
        border-radius: 28px;
        padding: 20px 22px 28px;
        backdrop-filter: blur(4px);
      }
      h2, h4 { color: var(--nuees-ink); }
      .subtitle {
        color: #56655d;
        font-style: italic;
        margin-top: -6px;
        margin-bottom: 14px;
      }
      .help, .statusbox {
        background: rgba(255,255,255,0.72);
        border: 1px solid rgba(122, 139, 112, 0.25);
        color: var(--nuees-ink);
        border-radius: 18px;
        box-shadow: 0 8px 24px rgba(80,90,70,0.10);
      }
      .help { padding: 14px 16px; margin-bottom: 16px; }
      .statusbox { padding: 12px 16px; min-width: 280px; }
      .topbar {
        display:flex; gap:12px; align-items:center; justify-content:space-between;
        margin-bottom:18px; flex-wrap:wrap;
      }
      .controls { display:flex; gap:10px; align-items:center; flex-wrap:wrap; }
      .btn, .btn-default, .btn-primary {
        background: var(--nuees-paper) !important;
        color: var(--nuees-ink) !important;
        border: 1px solid rgba(109,129,99,0.45) !important;
        border-radius: 999px !important;
        padding: 8px 16px !important;
        box-shadow: 0 4px 14px rgba(80,90,70,0.10);
      }
      .btn:hover { background: #fffdf9 !important; }
      .board-row { display:flex; gap:20px; align-items:flex-start; margin-bottom:20px; flex-wrap:wrap; }
      .left-group, .right-group { display:flex; gap:18px; flex-wrap:wrap; }
      .tableau-row { display:flex; gap:18px; align-items:flex-start; overflow-x:auto; padding-bottom:10px; }
      .tableau-col { width:118px; min-width:118px; min-height:166px; }
      .card, .foundation-empty, .tableau-empty {
        width:118px; height:165px; border-radius:18px; background: var(--nuees-paper);
        border: 1px solid rgba(71, 88, 72, 0.16); box-shadow: 0 10px 18px var(--nuees-shadow);
        position:relative; cursor:pointer; user-select:none; overflow:hidden;
        transition: transform .12s ease, box-shadow .12s ease, outline-color .12s ease;
      }
      .card::after {
        content:''; position:absolute; inset:0;
        background: linear-gradient(180deg, rgba(255,255,255,0.34), transparent 16%, transparent 84%, rgba(181,107,92,0.06));
        pointer-events:none;
      }
      .card:hover, .foundation-empty:hover, .tableau-empty:hover {
        transform: translateY(-2px);
        box-shadow: 0 14px 24px rgba(50,60,50,0.18);
      }
      .selected { outline: 4px solid rgba(208,177,115,0.75); }
      .dragging { opacity: 0.55; transform: rotate(2deg); }
      .drop-hover { outline: 4px dashed rgba(109,129,99,0.72); }
      .placeholder, .foundation-empty, .tableau-empty, .recycle {
        background: rgba(255,249,240,0.55);
        border: 2px dashed rgba(109,129,99,0.42);
        box-shadow: none;
      }
      .foundation-empty, .tableau-empty, .recycle {
        display:flex; align-items:center; justify-content:center; font-size:38px; color:#66735f;
      }
      .card-back {
        background: linear-gradient(135deg, #a1b091, #6c8162 55%, #506b57 100%);
      }
      .card-back::before {
        content:''; position:absolute; inset:12px; border-radius:12px;
        border: 1px solid rgba(255,255,255,0.35);
        background:
          radial-gradient(circle at 25% 30%, rgba(255,255,255,.20), transparent 18%),
          radial-gradient(circle at 70% 65%, rgba(255,255,255,.18), transparent 16%),
          linear-gradient(45deg, rgba(255,255,255,.08) 25%, transparent 25%, transparent 50%, rgba(255,255,255,.08) 50%, rgba(255,255,255,.08) 75%, transparent 75%);
        background-size: auto, auto, 22px 22px;
      }
      .card-img { width:100%; height:100%; object-fit:cover; display:block; }
      .fallback-back { width:100%; height:100%; display:flex; align-items:center; justify-content:center; font-size:48px; color:white; }
      .fallback-rank { position:absolute; font-weight:700; font-size:18px; z-index:1; }
      .fallback-rank.tl { top:9px; left:10px; }
      .fallback-rank.br { bottom:9px; right:10px; transform: rotate(180deg); }
      .fallback-center { position:absolute; top:50%; left:50%; transform:translate(-50%, -50%); font-size:28px; font-weight:700; z-index:1; }
      .tableau-card-wrap { position: relative; }
      .tableau-title { margin: 8px 0 10px; font-variant: small-caps; letter-spacing: .04em; color: #4e5d56; }
      .footer-note { margin-top: 18px; color: #5e6b65; font-size: 0.95em; }
    ")),
    tags$script(HTML(" 
      document.addEventListener('DOMContentLoaded', function () {
        let currentDrag = null;

        document.addEventListener('dragstart', function(e) {
          const el = e.target.closest('.draggable-card');
          if (!el) return;
          currentDrag = el.dataset.drag || null;
          e.dataTransfer.setData('text/plain', currentDrag || '');
          e.dataTransfer.effectAllowed = 'move';
          el.classList.add('dragging');
        });

        document.addEventListener('dragend', function(e) {
          const el = e.target.closest('.draggable-card');
          if (el) el.classList.remove('dragging');
          document.querySelectorAll('.drop-hover').forEach(x => x.classList.remove('drop-hover'));
        });

        document.addEventListener('dragover', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone || !currentDrag) return;
          e.preventDefault();
          e.dataTransfer.dropEffect = 'move';
        });

        document.addEventListener('dragenter', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone || !currentDrag) return;
          zone.classList.add('drop-hover');
        });

        document.addEventListener('dragleave', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone) return;
          if (!zone.contains(e.relatedTarget)) zone.classList.remove('drop-hover');
        });

        document.addEventListener('drop', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone || !currentDrag) return;
          e.preventDefault();
          zone.classList.remove('drop-hover');
          const payload = JSON.stringify({ source: JSON.parse(currentDrag), target: JSON.parse(zone.dataset.drop) });
          Shiny.setInputValue('drag_move', payload, {priority: 'event'});
          currentDrag = null;
        });
      });
    "))
  ),

  div(class = "app-shell",
      h2("Solitaire — à développer pour Nuées"),
      div(class = "subtitle", "Texte à ajouter."),
      div(class = "help",
          HTML('<strong>Contexte :</strong> texte à ajouter.<br><br>
               <strong>Utilisation :</strong> clique pour jouer comme avant, ou glisse-dépose une carte / pile visible sur une colonne ou une fondation. <br>
               On peut aussi ajouter des liens comme <a href="https://nuees.net/jeux/" target="_blank">jeu Nuées</a>')
      ),
      div(class = "topbar",
          div(class = "controls",
              actionButton("new_game", "Nouvelle partie"),
              actionButton("clear_sel", "Désélectionner")
          ),
          div(class = "statusbox", textOutput("status", inline = TRUE))
      ),
      div(class = "board-row",
          div(class = "left-group", uiOutput("stock_ui"), uiOutput("waste_ui")),
          div(class = "right-group",
              uiOutput("foundation_S"), uiOutput("foundation_H"), uiOutput("foundation_D"), uiOutput("foundation_C"))
      ),
      div(class = "tableau-title", "Tableau"),
      div(class = "tableau-row",
          uiOutput("tab_1"), uiOutput("tab_2"), uiOutput("tab_3"), uiOutput("tab_4"),
          uiOutput("tab_5"), uiOutput("tab_6"), uiOutput("tab_7")),
      div(class = "footer-note", "Note de bas de page à ajouter.")
  )
)

# =========================
# Server
# =========================

server <- function(input, output, session) {
  state <- reactiveVal(new_game_state())

  output$status <- renderText({
    st <- state()
    if (check_win(st)) paste0("Bravo, partie gagnée ! ", st$message) else st$message
  })

  observeEvent(input$new_game, { state(new_game_state()) })

  observeEvent(input$clear_sel, {
    st <- state(); st$selected <- NULL; st$message <- "Sélection effacée."; state(st)
  })

  output$stock_ui <- renderUI(render_stock(state()))
  output$waste_ui <- renderUI(render_waste(state()))
  output$foundation_S <- renderUI(render_foundation(state(), "S"))
  output$foundation_H <- renderUI(render_foundation(state(), "H"))
  output$foundation_D <- renderUI(render_foundation(state(), "D"))
  output$foundation_C <- renderUI(render_foundation(state(), "C"))
  for (i in 1:7) local({
    ii <- i
    output[[paste0("tab_", ii)]] <- renderUI(render_tableau_col(state(), ii))
  })

  observeEvent(input$drag_move, {
    payload <- tryCatch(fromJSON(input$drag_move), error = function(e) NULL)
    if (is.null(payload) || is.null(payload$source) || is.null(payload$target)) return()
    st <- state()
    st <- apply_drag_move(st, payload$source, payload$target)
    st$selected <- NULL
    state(st)
  })

  observeEvent(input$card_click, {
    click <- input$card_click
    st <- state()

    if (identical(click, "stock")) {
      st$selected <- NULL
      if (length(st$stock) > 0) {
        card_id <- tail(st$stock, 1)
        st$stock <- head(st$stock, -1)
        st$waste <- c(st$waste, card_id)
        st$message <- paste("Carte tirée :", card_id)
      } else if (length(st$waste) > 0) {
        st$stock <- rev(st$waste)
        st$waste <- character(0)
        st$message <- "La défausse est remise dans la pioche."
      } else {
        st$message <- "Aucune carte à tirer."
      }
      state(st); return()
    }

    if (grepl("^foundation:", click)) {
      suit <- sub("^foundation:", "", click)
      if (is.null(st$selected)) {
        if (length(st$foundations[[suit]]) > 0) {
          st$selected <- list(type = "foundation", suit = suit)
          st$message <- paste("Fondation sélectionnée :", suit)
        } else st$message <- paste("Fondation vide :", suit)
        state(st); return()
      }
      st <- apply_drag_move(st, st$selected, list(target_type = "foundation", suit = suit))
      st$selected <- NULL
      state(st); return()
    }

    if (identical(click, "waste")) {
      if (length(st$waste) == 0) {
        st$message <- "Défausse vide."
      } else if (is.null(st$selected)) {
        st$selected <- list(type = "waste")
        st$message <- paste("Carte sélectionnée :", top_card(st$waste))
      } else {
        st$selected <- NULL
        st$message <- "Sélection effacée."
      }
      state(st); return()
    }

    if (grepl("^tableau:", click)) {
      parts <- strsplit(click, ":")[[1]]
      col <- as.integer(parts[2])

      if (identical(parts[3], "empty")) {
        if (is.null(st$selected)) {
          st$message <- "Colonne vide."
        } else {
          st <- apply_drag_move(st, st$selected, list(target_type = "tableau", col = col))
          st$selected <- NULL
        }
        state(st); return()
      }

      idx <- as.integer(parts[3])
      pile <- st$tableau[[col]]

      if (!pile$up[idx]) {
        if (idx == length(pile$ids)) {
          pile$up[idx] <- TRUE
          st$tableau[[col]] <- pile
          st$message <- paste("Carte retournée dans la colonne", col)
        } else {
          st$message <- "Cette carte n'est pas accessible."
        }
        state(st); return()
      }

      if (is.null(st$selected)) {
        st$selected <- list(type = "tableau", col = col, idx = idx)
        st$message <- paste("Sélection :", pile$ids[idx], "dans colonne", col)
        state(st); return()
      }

      if (identical(st$selected$type, "tableau") && st$selected$col == col && st$selected$idx == idx) {
        st$selected <- NULL
        st$message <- "Sélection effacée."
        state(st); return()
      }

      st <- apply_drag_move(st, st$selected, list(target_type = "tableau", col = col))
      st$selected <- NULL
      state(st); return()
    }
  })
}

shinyApp(ui, server)
