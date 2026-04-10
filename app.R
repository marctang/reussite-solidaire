library(shiny)
library(htmltools)
library(jsonlite)

# =========================================================
# Solitaire Klondike (single-file Shiny app)
# - proche du Solitaire Windows classique
# - glisser-deposer + clic pour deplacer
# - images de cartes personnalisables via PNG/JPG
# - themes detectes automatiquement dans www/cards/
# =========================================================

# -----------------------------
# Deck helpers
# -----------------------------
suits <- c("S", "H", "D", "C")
ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
suit_symbol <- c(S = "♠", H = "♥", D = "♦", C = "♣")
suit_color <- function(s) if (s %in% c("H", "D")) "red" else "black"
rank_value <- function(r) match(r, ranks)

make_deck <- function() {
  deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)
  deck$id <- paste0(deck$rank, deck$suit)
  deck$color <- vapply(deck$suit, suit_color, character(1))
  deck
}

get_card <- function(id, deck) {
  deck[deck$id == id, , drop = FALSE]
}

top_card <- function(x) if (length(x) == 0) NULL else x[[length(x)]]

flip_last_if_needed <- function(pile) {
  if (length(pile$ids) > 0 && !any(pile$up)) {
    pile$up[length(pile$up)] <- TRUE
  }
  pile
}

# -----------------------------
# Game initialization
# -----------------------------
new_game_state <- function(draw_n = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  deck <- make_deck()
  shuffled <- sample(deck$id)

  tableau <- vector("list", 7)
  pos <- 1
  for (i in seq_len(7)) {
    ids <- shuffled[pos:(pos + i - 1)]
    tableau[[i]] <- list(ids = ids, up = c(rep(FALSE, i - 1), TRUE))
    pos <- pos + i
  }

  stock <- if (pos <= length(shuffled)) shuffled[pos:length(shuffled)] else character(0)

  list(
    deck = deck,
    draw_n = draw_n,
    stock = stock,
    waste = character(0),
    foundations = list(S = character(0), H = character(0), D = character(0), C = character(0)),
    tableau = tableau,
    selected = NULL,
    moves = 0,
    score = 0,
    started_at = Sys.time(),
    message = "Texte à ajouter."
  )
}

# -----------------------------
# Rules
# -----------------------------
can_place_on_tableau <- function(moving_id, target_id, deck) {
  moving <- get_card(moving_id, deck)
  target <- get_card(target_id, deck)

  nrow(moving) == 1 &&
    nrow(target) == 1 &&
    moving$color != target$color &&
    rank_value(moving$rank) == rank_value(target$rank) - 1
}

can_move_to_empty_tableau <- function(card_id, deck) {
  card <- get_card(card_id, deck)
  nrow(card) == 1 && identical(card$rank, "K")
}

can_place_on_foundation <- function(card_id, foundation_ids, deck, target_suit = NULL) {
  card <- get_card(card_id, deck)
  if (nrow(card) == 0) return(FALSE)
  if (!is.null(target_suit) && !identical(card$suit, target_suit)) return(FALSE)

  if (length(foundation_ids) == 0) return(identical(card$rank, "A"))

  topf <- get_card(top_card(foundation_ids), deck)
  identical(card$suit, topf$suit) && rank_value(card$rank) == rank_value(topf$rank) + 1
}

is_won <- function(state) {
  sum(vapply(state$foundations, length, integer(1))) == 52
}

# -----------------------------
# Theme / assets
# -----------------------------
normalize_theme_name <- function(x) {
  x <- trimws(x)
  x[nzchar(x)]
}

list_theme_dirs <- function(base_dir) {
  if (!dir.exists(base_dir)) return(character(0))
  all_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)
  normalize_theme_name(all_dirs)
}

available_themes <- function() {
  themes <- c("classic")
  themes <- c(themes, list_theme_dirs("www/cards"), list_theme_dirs("cards"))
  unique(themes)
}

ensure_resource_paths <- function() {
  if (dir.exists("cards")) addResourcePath("cards_local", normalizePath("cards", winslash = "/", mustWork = FALSE))
  if (dir.exists("www/cards")) addResourcePath("cards_www", normalizePath("www/cards", winslash = "/", mustWork = FALSE))
}

ensure_resource_paths()

find_asset <- function(card_id, theme = "classic", back = FALSE, session_theme_dir = NULL) {
  exts <- c("png", "jpg", "jpeg", "webp")
  fname <- if (back) "back" else card_id

  # 1) uploaded zip theme for current session
  if (!is.null(session_theme_dir) && dir.exists(session_theme_dir)) {
    for (ext in exts) {
      p <- file.path(session_theme_dir, paste0(fname, ".", ext))
      if (file.exists(p)) {
        addResourcePath("cards_session", normalizePath(session_theme_dir, winslash = "/", mustWork = FALSE))
        return(paste0("cards_session/", basename(p)))
      }
    }
  }

  # 2) theme folder in www/cards/<theme>
  if (!identical(theme, "classic")) {
    for (ext in exts) {
      rel <- file.path(theme, paste0(fname, ".", ext))
      p1 <- file.path("www/cards", rel)
      p2 <- file.path("cards", rel)
      if (file.exists(p1)) return(paste0("cards_www/", rel))
      if (file.exists(p2)) return(paste0("cards_local/", rel))
    }
  }

  # 3) direct flat folder fallback: www/cards/AS.png etc.
  for (ext in exts) {
    rel <- paste0(fname, ".", ext)
    p1 <- file.path("www/cards", rel)
    p2 <- file.path("cards", rel)
    if (file.exists(p1)) return(paste0("cards_www/", rel))
    if (file.exists(p2)) return(paste0("cards_local/", rel))
  }

  NULL
}

card_label <- function(card_id) {
  rank <- sub("([0-9JQKA]+)([SHDC])$", "\\1", card_id)
  suit <- sub("([0-9JQKA]+)([SHDC])$", "\\2", card_id)
  paste(rank, suit_symbol[[suit]])
}

# -----------------------------
# Movement helpers
# -----------------------------
move_tableau_sequence <- function(state, from_col, from_idx, to_col) {
  src <- state$tableau[[from_col]]
  dst <- state$tableau[[to_col]]

  moving_ids <- src$ids[from_idx:length(src$ids)]
  moving_up  <- src$up[from_idx:length(src$up)]

  if (from_idx > 1) {
    src$ids <- src$ids[1:(from_idx - 1)]
    src$up  <- src$up[1:(from_idx - 1)]
  } else {
    src$ids <- character(0)
    src$up  <- logical(0)
  }

  dst$ids <- c(dst$ids, moving_ids)
  dst$up  <- c(dst$up, moving_up)

  state$tableau[[from_col]] <- flip_last_if_needed(src)
  state$tableau[[to_col]] <- dst
  state$moves <- state$moves + 1
  state
}

remove_top_tableau_card <- function(state, col) {
  pile <- state$tableau[[col]]
  id <- top_card(pile$ids)
  if (is.null(id)) return(list(state = state, card = NULL))

  pile$ids <- head(pile$ids, -1)
  pile$up <- head(pile$up, -1)
  state$tableau[[col]] <- flip_last_if_needed(pile)
  list(state = state, card = id)
}

send_to_foundation <- function(state, from_type, from_col = NULL, from_suit = NULL) {
  if (identical(from_type, "waste")) {
    card_id <- top_card(state$waste)
    if (is.null(card_id)) {
      state$message <- "Aucune carte dans la défausse."
      return(state)
    }
    suit <- get_card(card_id, state$deck)$suit
    if (!can_place_on_foundation(card_id, state$foundations[[suit]], state$deck, target_suit = suit)) {
      state$message <- "Cette carte ne peut pas aller en fondation."
      return(state)
    }
    state$waste <- head(state$waste, -1)
    state$foundations[[suit]] <- c(state$foundations[[suit]], card_id)
    state$moves <- state$moves + 1
    state$score <- state$score + 10
    state$message <- paste("Carte envoyée en fondation :", card_label(card_id))
    return(state)
  }

  if (identical(from_type, "tableau")) {
    pile <- state$tableau[[from_col]]
    if (length(pile$ids) == 0 || !pile$up[length(pile$up)]) {
      state$message <- "Aucune carte visible à envoyer."
      return(state)
    }
    card_id <- top_card(pile$ids)
    suit <- get_card(card_id, state$deck)$suit
    if (!can_place_on_foundation(card_id, state$foundations[[suit]], state$deck, target_suit = suit)) {
      state$message <- "Cette carte ne peut pas aller en fondation."
      return(state)
    }
    rem <- remove_top_tableau_card(state, from_col)
    state <- rem$state
    state$foundations[[suit]] <- c(state$foundations[[suit]], card_id)
    state$moves <- state$moves + 1
    state$score <- state$score + 10
    state$message <- paste("Carte envoyée en fondation :", card_label(card_id))
    return(state)
  }

  if (identical(from_type, "foundation")) {
    state$message <- "La carte est déjà en fondation."
    return(state)
  }

  state
}

draw_from_stock <- function(state) {
  if (length(state$stock) == 0) {
    if (length(state$waste) == 0) {
      state$message <- "Le talon et la défausse sont vides."
      return(state)
    }
    state$stock <- rev(state$waste)
    state$waste <- character(0)
    state$message <- "Le talon a été reconstitué."
    return(state)
  }

  n <- min(state$draw_n, length(state$stock))
  drawn <- state$stock[seq_len(n)]
  state$stock <- state$stock[-seq_len(n)]
  state$waste <- c(state$waste, drawn)
  state$moves <- state$moves + 1
  state$message <- if (n == 1) "1 carte tirée." else paste(n, "cartes tirées.")
  state
}

try_move_to_tableau <- function(state, moving_id, target_col, source_type, source_col = NULL, source_idx = NULL, source_suit = NULL) {
  dst <- state$tableau[[target_col]]

  if (length(dst$ids) == 0) {
    if (!can_move_to_empty_tableau(moving_id, state$deck)) {
      state$message <- "Seul un Roi peut aller sur une colonne vide."
      return(state)
    }

    if (identical(source_type, "waste")) {
      state$waste <- head(state$waste, -1)
      state$tableau[[target_col]]$ids <- c(state$tableau[[target_col]]$ids, moving_id)
      state$tableau[[target_col]]$up <- c(state$tableau[[target_col]]$up, TRUE)
    } else if (identical(source_type, "foundation")) {
      state$foundations[[source_suit]] <- head(state$foundations[[source_suit]], -1)
      state$tableau[[target_col]]$ids <- c(state$tableau[[target_col]]$ids, moving_id)
      state$tableau[[target_col]]$up <- c(state$tableau[[target_col]]$up, TRUE)
    } else if (identical(source_type, "tableau")) {
      state <- move_tableau_sequence(state, source_col, source_idx, target_col)
      state$message <- paste("Pile déplacée vers la colonne", target_col)
      state$score <- state$score + 5
      return(state)
    }

    state$moves <- state$moves + 1
    state$score <- state$score + 5
    state$message <- paste("Carte déplacée vers la colonne", target_col)
    return(state)
  }

  target_id <- top_card(dst$ids)
  if (!can_place_on_tableau(moving_id, target_id, state$deck)) {
    state$message <- "Déplacement invalide."
    return(state)
  }

  if (identical(source_type, "waste")) {
    state$waste <- head(state$waste, -1)
    state$tableau[[target_col]]$ids <- c(state$tableau[[target_col]]$ids, moving_id)
    state$tableau[[target_col]]$up <- c(state$tableau[[target_col]]$up, TRUE)
  } else if (identical(source_type, "foundation")) {
    state$foundations[[source_suit]] <- head(state$foundations[[source_suit]], -1)
    state$tableau[[target_col]]$ids <- c(state$tableau[[target_col]]$ids, moving_id)
    state$tableau[[target_col]]$up <- c(state$tableau[[target_col]]$up, TRUE)
  } else if (identical(source_type, "tableau")) {
    if (identical(source_col, target_col)) return(state)
    state <- move_tableau_sequence(state, source_col, source_idx, target_col)
    state$message <- paste("Pile déplacée vers la colonne", target_col)
    state$score <- state$score + 5
    return(state)
  }

  state$moves <- state$moves + 1
  state$score <- state$score + 5
  state$message <- paste("Carte déplacée vers la colonne", target_col)
  state
}

try_move_to_foundation <- function(state, moving_id, foundation_suit, source_type, source_col = NULL, source_idx = NULL, source_suit = NULL) {
  if (!can_place_on_foundation(moving_id, state$foundations[[foundation_suit]], state$deck, target_suit = foundation_suit)) {
    state$message <- "Déplacement invalide vers la fondation."
    return(state)
  }

  if (identical(source_type, "waste")) {
    state$waste <- head(state$waste, -1)
  } else if (identical(source_type, "foundation")) {
    if (identical(source_suit, foundation_suit)) return(state)
    state$foundations[[source_suit]] <- head(state$foundations[[source_suit]], -1)
  } else if (identical(source_type, "tableau")) {
    pile <- state$tableau[[source_col]]
    if (!identical(source_idx, length(pile$ids))) {
      state$message <- "Seule la carte du dessus peut aller en fondation."
      return(state)
    }
    rem <- remove_top_tableau_card(state, source_col)
    state <- rem$state
  }

  state$foundations[[foundation_suit]] <- c(state$foundations[[foundation_suit]], moving_id)
  state$moves <- state$moves + 1
  state$score <- state$score + 10
  state$message <- paste("Carte envoyée en fondation :", card_label(moving_id))
  state
}

parse_click <- function(x) {
  parts <- strsplit(x, ":", fixed = TRUE)[[1]]
  list(raw = x, parts = parts)
}

find_auto_tableau_target <- function(state, moving_id, exclude_col = NULL) {
  for (col in seq_len(7)) {
    if (!is.null(exclude_col) && identical(col, exclude_col)) next
    pile <- state$tableau[[col]]
    if (length(pile$ids) == 0) {
      if (can_move_to_empty_tableau(moving_id, state$deck)) return(col)
    } else {
      if (can_place_on_tableau(moving_id, top_card(pile$ids), state$deck)) return(col)
    }
  }
  NULL
}

auto_move_selected <- function(state) {
  sel <- state$selected
  if (is.null(sel)) return(state)

  if (identical(sel$type, "waste")) {
    card_id <- top_card(state$waste)
    if (is.null(card_id)) return(state)
    suit <- get_card(card_id, state$deck)$suit
    if (can_place_on_foundation(card_id, state$foundations[[suit]], state$deck, suit)) {
      state <- send_to_foundation(state, "waste")
      state$selected <- NULL
      return(state)
    }
    tgt <- find_auto_tableau_target(state, card_id)
    if (!is.null(tgt)) {
      state <- try_move_to_tableau(state, card_id, tgt, "waste")
      state$selected <- NULL
      return(state)
    }
    state$message <- "Aucun déplacement automatique possible."
    return(state)
  }

  if (identical(sel$type, "tableau")) {
    pile <- state$tableau[[sel$col]]
    if (sel$idx > length(pile$ids) || !pile$up[sel$idx]) {
      state$selected <- NULL
      return(state)
    }
    card_id <- pile$ids[sel$idx]

    # single top card -> try foundation first
    if (identical(sel$idx, length(pile$ids))) {
      suit <- get_card(card_id, state$deck)$suit
      if (can_place_on_foundation(card_id, state$foundations[[suit]], state$deck, suit)) {
        state <- send_to_foundation(state, "tableau", from_col = sel$col)
        state$selected <- NULL
        return(state)
      }
    }

    tgt <- find_auto_tableau_target(state, card_id, exclude_col = sel$col)
    if (!is.null(tgt)) {
      state <- try_move_to_tableau(state, card_id, tgt, "tableau", source_col = sel$col, source_idx = sel$idx)
      state$selected <- NULL
      return(state)
    }

    state$message <- "Aucun déplacement automatique possible."
    return(state)
  }

  if (identical(sel$type, "foundation")) {
    card_id <- top_card(state$foundations[[sel$suit]])
    if (is.null(card_id)) return(state)
    tgt <- find_auto_tableau_target(state, card_id)
    if (!is.null(tgt)) {
      state <- try_move_to_tableau(state, card_id, tgt, "foundation", source_suit = sel$suit)
      state$selected <- NULL
      return(state)
    }
    state$message <- "Aucun déplacement automatique possible."
  }

  state
}

handle_click <- function(state, click) {
  info <- parse_click(click)
  parts <- info$parts

  if (identical(parts[[1]], "stock")) {
    state$selected <- NULL
    return(draw_from_stock(state))
  }

  # empty tableau as explicit target for selected card
  if (identical(parts[[1]], "tableau") && length(parts) == 3 && identical(parts[[3]], "empty")) {
    target_col <- as.integer(parts[[2]])
    sel <- state$selected
    if (is.null(sel)) {
      state$message <- "Sélectionnez d'abord une carte."
      return(state)
    }
    if (identical(sel$type, "waste")) {
      card_id <- top_card(state$waste)
      state <- try_move_to_tableau(state, card_id, target_col, "waste")
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "foundation")) {
      card_id <- top_card(state$foundations[[sel$suit]])
      state <- try_move_to_tableau(state, card_id, target_col, "foundation", source_suit = sel$suit)
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "tableau")) {
      pile <- state$tableau[[sel$col]]
      card_id <- pile$ids[sel$idx]
      state <- try_move_to_tableau(state, card_id, target_col, "tableau", source_col = sel$col, source_idx = sel$idx)
      state$selected <- NULL
      return(state)
    }
  }

  if (identical(parts[[1]], "waste")) {
    if (length(state$waste) == 0) return(state)
    if (!is.null(state$selected) && identical(state$selected$type, "waste")) {
      state$selected <- NULL
      state$message <- "Sélection annulée."
      return(state)
    }
    state$selected <- list(type = "waste")
    state$message <- "Carte de la défausse sélectionnée."
    return(state)
  }

  if (identical(parts[[1]], "foundation")) {
    suit <- parts[[2]]

    if (is.null(state$selected)) {
      if (length(state$foundations[[suit]]) == 0) {
        state$message <- "Fondation vide."
        return(state)
      }
      state$selected <- list(type = "foundation", suit = suit)
      state$message <- paste("Fondation", suit_symbol[[suit]], "sélectionnée.")
      return(state)
    }

    sel <- state$selected
    if (identical(sel$type, "waste")) {
      card_id <- top_card(state$waste)
      state <- try_move_to_foundation(state, card_id, suit, "waste")
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "tableau")) {
      pile <- state$tableau[[sel$col]]
      card_id <- pile$ids[sel$idx]
      state <- try_move_to_foundation(state, card_id, suit, "tableau", source_col = sel$col, source_idx = sel$idx)
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "foundation")) {
      if (identical(sel$suit, suit)) {
        state$selected <- NULL
        state$message <- "Sélection annulée."
        return(state)
      }
      card_id <- top_card(state$foundations[[sel$suit]])
      state <- try_move_to_foundation(state, card_id, suit, "foundation", source_suit = sel$suit)
      state$selected <- NULL
      return(state)
    }
  }

  if (identical(parts[[1]], "tableau")) {
    col <- as.integer(parts[[2]])
    idx <- as.integer(parts[[3]])
    pile <- state$tableau[[col]]
    if (idx > length(pile$ids)) return(state)

    if (!pile$up[idx]) {
      state$message <- "Cette carte est retournée."
      return(state)
    }

    if (is.null(state$selected)) {
      state$selected <- list(type = "tableau", col = col, idx = idx)
      state$message <- paste("Sélection dans la colonne", col)
      return(state)
    }

    sel <- state$selected
    if (identical(sel$type, "tableau") && identical(sel$col, col) && identical(sel$idx, idx)) {
      state <- auto_move_selected(state)
      if (!identical(state$message, "Aucun déplacement automatique possible.")) state$selected <- NULL
      return(state)
    }

    # target tableau
    if (identical(sel$type, "waste")) {
      card_id <- top_card(state$waste)
      state <- try_move_to_tableau(state, card_id, col, "waste")
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "foundation")) {
      card_id <- top_card(state$foundations[[sel$suit]])
      state <- try_move_to_tableau(state, card_id, col, "foundation", source_suit = sel$suit)
      state$selected <- NULL
      return(state)
    }
    if (identical(sel$type, "tableau")) {
      moving_id <- state$tableau[[sel$col]]$ids[sel$idx]
      state <- try_move_to_tableau(state, moving_id, col, "tableau", source_col = sel$col, source_idx = sel$idx)
      state$selected <- NULL
      return(state)
    }
  }

  state
}

apply_drag_move <- function(state, source, target) {
  if (is.null(source) || is.null(target)) return(state)

  if (identical(target$target_type, "tableau")) {
    target_col <- as.integer(target$col)

    if (identical(source$type, "waste")) {
      moving_id <- top_card(state$waste)
      return(try_move_to_tableau(state, moving_id, target_col, "waste"))
    }
    if (identical(source$type, "foundation")) {
      moving_id <- top_card(state$foundations[[source$suit]])
      return(try_move_to_tableau(state, moving_id, target_col, "foundation", source_suit = source$suit))
    }
    if (identical(source$type, "tableau")) {
      moving_id <- state$tableau[[source$col]]$ids[source$idx]
      return(try_move_to_tableau(state, moving_id, target_col, "tableau", source_col = source$col, source_idx = source$idx))
    }
  }

  if (identical(target$target_type, "foundation")) {
    suit <- target$suit
    if (identical(source$type, "waste")) {
      moving_id <- top_card(state$waste)
      return(try_move_to_foundation(state, moving_id, suit, "waste"))
    }
    if (identical(source$type, "foundation")) {
      moving_id <- top_card(state$foundations[[source$suit]])
      return(try_move_to_foundation(state, moving_id, suit, "foundation", source_suit = source$suit))
    }
    if (identical(source$type, "tableau")) {
      moving_id <- state$tableau[[source$col]]$ids[source$idx]
      return(try_move_to_foundation(state, moving_id, suit, "tableau", source_col = source$col, source_idx = source$idx))
    }
  }

  state$message <- "Déplacement impossible."
  state
}

# -----------------------------
# UI fragments
# -----------------------------
source_payload <- function(type, col = NULL, idx = NULL, suit = NULL) {
  payload <- list(type = type)
  if (!is.null(col)) payload$col <- col
  if (!is.null(idx)) payload$idx <- idx
  if (!is.null(suit)) payload$suit <- suit
  jsonlite::toJSON(payload, auto_unbox = TRUE)
}

drop_payload <- function(target_type, col = NULL, suit = NULL) {
  payload <- list(target_type = target_type)
  if (!is.null(col)) payload$col <- col
  if (!is.null(suit)) payload$suit <- suit
  jsonlite::toJSON(payload, auto_unbox = TRUE)
}

card_tag <- function(card_id = NULL,
                     face_up = TRUE,
                     clickable = FALSE,
                     click_value = NULL,
                     selected = FALSE,
                     deck = NULL,
                     theme = "classic",
                     session_theme_dir = NULL,
                     placeholder = FALSE,
                     draggable = FALSE,
                     drag_payload = NULL,
                     drop_payload_json = NULL,
                     extra_classes = character()) {

  classes <- c("sol-card", extra_classes)
  if (!face_up) classes <- c(classes, "card-back")
  if (selected) classes <- c(classes, "selected")
  if (placeholder) classes <- c(classes, "placeholder")
  if (draggable && face_up && !placeholder) classes <- c(classes, "draggable-card")
  if (!is.null(drop_payload_json)) classes <- c(classes, "dropzone")

  attrs <- list(class = paste(classes, collapse = " "))
  if (clickable && !is.null(click_value)) {
    attrs$onclick <- sprintf("Shiny.setInputValue('card_click', '%s', {priority: 'event'})", click_value)
  }
  if (draggable && face_up && !placeholder && !is.null(drag_payload)) {
    attrs$draggable <- "true"
    attrs[["data-drag"]] <- drag_payload
  }
  if (!is.null(drop_payload_json)) attrs[["data-drop"]] <- drop_payload_json

  if (placeholder) {
    return(do.call(div, c(attrs, list())))
  }

  if (!face_up) {
    back_asset <- find_asset(back = TRUE, card_id = NULL, theme = theme, session_theme_dir = session_theme_dir)
    if (!is.null(back_asset)) {
      return(do.call(div, c(attrs, list(img(src = back_asset, class = "card-img", draggable = "false")))))
    }
    return(do.call(div, c(attrs, list(div(class = "fallback-back", "✶")))))
  }

  front_asset <- find_asset(card_id = card_id, back = FALSE, theme = theme, session_theme_dir = session_theme_dir)
  if (!is.null(front_asset)) {
    return(do.call(div, c(attrs, list(img(src = front_asset, class = "card-img", draggable = "false")))))
  }

  card <- get_card(card_id, deck)
  col <- if (nrow(card) == 1 && identical(card$color, "red")) "#a52b2b" else "#111827"
  do.call(div, c(attrs, list(
    style = sprintf("color:%s;", col),
    div(class = "fallback-rank tl", card_label(card_id)),
    div(class = "fallback-center", card_label(card_id)),
    div(class = "fallback-rank br", card_label(card_id))
  )))
}

render_stock <- function(state, theme, session_theme_dir) {
  clickable <- !(length(state$stock) == 0 && length(state$waste) == 0)
  if (length(state$stock) > 0) {
    return(card_tag(face_up = FALSE, clickable = clickable, click_value = "stock", theme = theme, session_theme_dir = session_theme_dir))
  }
  div(
    class = "recycle-wrap",
    div(
      class = "sol-card recycle",
      onclick = if (clickable) "Shiny.setInputValue('card_click','stock',{priority:'event'})" else NULL,
      "↺"
    )
  )
}

render_waste <- function(state, theme, session_theme_dir) {
  if (length(state$waste) == 0) return(card_tag(placeholder = TRUE))
  selected <- !is.null(state$selected) && identical(state$selected$type, "waste")
  card_id <- top_card(state$waste)
  card_tag(
    card_id = card_id,
    face_up = TRUE,
    clickable = TRUE,
    click_value = "waste",
    selected = selected,
    deck = state$deck,
    theme = theme,
    session_theme_dir = session_theme_dir,
    draggable = TRUE,
    drag_payload = source_payload("waste")
  )
}

render_foundation <- function(state, suit, theme, session_theme_dir) {
  pile <- state$foundations[[suit]]
  sel <- !is.null(state$selected) && identical(state$selected$type, "foundation") && identical(state$selected$suit, suit)
  dp <- drop_payload("foundation", suit = suit)

  if (length(pile) == 0) {
    return(div(
      class = paste("sol-card foundation-empty dropzone", if (sel) "selected" else ""),
      `data-drop` = dp,
      onclick = sprintf("Shiny.setInputValue('card_click', 'foundation:%s', {priority:'event'})", suit),
      suit_symbol[[suit]]
    ))
  }

  card_id <- top_card(pile)
  card_tag(
    card_id = card_id,
    face_up = TRUE,
    clickable = TRUE,
    click_value = paste0("foundation:", suit),
    selected = sel,
    deck = state$deck,
    theme = theme,
    session_theme_dir = session_theme_dir,
    draggable = TRUE,
    drag_payload = source_payload("foundation", suit = suit),
    drop_payload_json = dp
  )
}

render_tableau <- function(state, col, theme, session_theme_dir) {
  pile <- state$tableau[[col]]
  dp_col <- drop_payload("tableau", col = col)

  if (length(pile$ids) == 0) {
    return(div(
      class = "tableau-col",
      div(
        class = "sol-card tableau-empty dropzone",
        `data-drop` = dp_col,
        onclick = sprintf("Shiny.setInputValue('card_click', 'tableau:%s:empty', {priority:'event'})", col),
        "K"
      )
    ))
  }

  pieces <- lapply(seq_along(pile$ids), function(i) {
    id <- pile$ids[[i]]
    up <- pile$up[[i]]
    sel <- !is.null(state$selected) && identical(state$selected$type, "tableau") && identical(state$selected$col, col) && i >= state$selected$idx
    overlap_class <- if (i == 1) {
      "first-card"
    } else if (up) {
      "face-up-card"
    } else {
      "face-down-card"
    }

    div(
      class = paste("tableau-card-wrap", overlap_class),
      card_tag(
        card_id = id,
        face_up = up,
        clickable = TRUE,
        click_value = paste0("tableau:", col, ":", i),
        selected = sel,
        deck = state$deck,
        theme = theme,
        session_theme_dir = session_theme_dir,
        draggable = up,
        drag_payload = if (up) source_payload("tableau", col = col, idx = i) else NULL,
        drop_payload_json = if (i == length(pile$ids) && up) dp_col else NULL,
        extra_classes = if (i == length(pile$ids) && up) "top-drop-target" else character(0)
      )
    )
  })

  div(class = "tableau-col", pieces)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$style(HTML(" 
      :root {
        --bg1: #0d6b38;
        --bg2: #0a4e2b;
        --panel: rgba(255,255,255,0.10);
        --panel-border: rgba(255,255,255,0.18);
        --ink: #f5f7fb;
        --muted: rgba(255,255,255,0.78);
        --card-width: 100px;
        --card-height: 140px;
        --overlap-down: 126px;
        --overlap-up: 126px;
        --shadow: rgba(0,0,0,0.28);
      }
      * { box-sizing: border-box; }
      html, body { min-height: 100%; }
      body {
        background:
          radial-gradient(circle at top left, rgba(255,255,255,0.08), transparent 25%),
          radial-gradient(circle at bottom right, rgba(255,255,255,0.06), transparent 28%),
          linear-gradient(180deg, var(--bg1) 0%, var(--bg2) 100%);
        color: var(--ink);
        font-family: 'Segoe UI', Arial, sans-serif;
      }
      .container-fluid {
        max-width: 1440px;
        padding: 14px 12px 28px;
      }
      .shell {
        border-radius: 24px;
        background: rgba(0,0,0,0.12);
        border: 1px solid rgba(255,255,255,0.10);
        box-shadow: 0 20px 45px rgba(0,0,0,0.18);
        padding: 14px;
      }
      .topbar {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        align-items: stretch;
        justify-content: space-between;
        margin-bottom: 14px;
      }
      .titlebox, .controlbox, .statusbox {
        background: var(--panel);
        border: 1px solid var(--panel-border);
        border-radius: 18px;
        padding: 12px 14px;
        backdrop-filter: blur(5px);
      }
      .titlebox { min-width: 280px; flex: 1 1 320px; }
      .controlbox { flex: 2 1 560px; }
      .statusbox { min-width: 280px; flex: 1 1 320px; }
      .app-title {
        font-size: clamp(1.4rem, 2.4vw, 2rem);
        font-weight: 700;
        margin: 0 0 4px 0;
      }
      .subtitle { margin: 0; color: var(--muted); }
      .controls-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 10px;
        align-items: end;
      }
      .form-group { margin-bottom: 0; }
      .btn, .btn-default, .btn-primary {
        border-radius: 999px !important;
        border: 1px solid rgba(255,255,255,0.25) !important;
        background: rgba(255,255,255,0.14) !important;
        color: white !important;
      }
      .btn:hover { background: rgba(255,255,255,0.22) !important; }
      .help-text { color: var(--muted); font-size: 0.92rem; line-height: 1.35; }
      .status-row {
        display: grid;
        grid-template-columns: repeat(3, minmax(70px, auto));
        gap: 10px;
        margin-bottom: 8px;
      }
      .metric {
        background: rgba(255,255,255,0.08);
        border-radius: 12px;
        padding: 8px 10px;
        text-align: center;
      }
      .metric .label { font-size: 0.78rem; color: var(--muted); }
      .metric .value { font-size: 1.15rem; font-weight: 700; }
      .message-line {
        min-height: 24px;
        font-size: 0.95rem;
      }
      .board-top {
        display: grid;
        grid-template-columns: 1fr auto;
        gap: 26px;
        align-items: start;
        margin-bottom: 18px;
      }
      .stock-waste {
        display: flex;
        gap: 14px;
        align-items: flex-start;
      }
      .foundations {
        display: flex;
        gap: 14px;
        justify-content: flex-end;
        flex-wrap: wrap;
      }
      .tableau-row {
        display: grid;
        grid-template-columns: repeat(7, minmax(110px, 1fr));
        gap: 12px;
        align-items: start;
      }
      .tableau-col {
        min-height: calc(var(--card-height) + 180px);
        position: relative;
      }
      .tableau-card-wrap.first-card { margin-top: calc(var(--overlap-down) * -1); }
      .tableau-card-wrap.face-down-card { margin-top: calc(var(--overlap-down) * -1); }
      .tableau-card-wrap.face-up-card { margin-top: calc(var(--overlap-up) * -1); }
      .tableau-card-wrap:first-child { margin-top: 0 !important;  }
      .sol-card {
        width: var(--card-width);
        height: var(--card-height);
        border-radius: 10px;
        background: #ffffff;
        border: 1px solid rgba(17,24,39,0.20);
        box-shadow: 0 5px 12px var(--shadow);
        overflow: hidden;
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        user-select: none;
        transition: transform 0.08s ease, box-shadow 0.08s ease, outline 0.08s ease;
      }
      .sol-card:hover { transform: translateY(-1px); }
      .sol-card.selected {
        outline: 3px solid #facc15;
        outline-offset: 2px;
        box-shadow: 0 0 0 4px rgba(250, 204, 21, 0.15), 0 6px 16px var(--shadow);
      }
      .sol-card.placeholder,
      .sol-card.foundation-empty,
      .sol-card.tableau-empty,
      .sol-card.recycle {
        background: rgba(255,255,255,0.08);
        border: 2px dashed rgba(255,255,255,0.28);
        color: rgba(255,255,255,0.80);
      }
      .sol-card.recycle { font-size: 2rem; }
      .card-back {
        background: linear-gradient(135deg, #174ea6, #0b2f6a);
      }
      .card-img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        display: block;
        pointer-events: none;
      }
      .fallback-back {
        color: white;
        font-size: 2rem;
      }
      .fallback-rank {
        position: absolute;
        font-size: 0.95rem;
        font-weight: 700;
        line-height: 1.05;
      }
      .fallback-rank.tl { top: 8px; left: 8px; }
      .fallback-rank.br { bottom: 8px; right: 8px; transform: rotate(180deg); }
      .fallback-center {
        font-size: 1.7rem;
        font-weight: 700;
      }
      .dropzone.can-drop {
        box-shadow: 0 0 0 3px rgba(59,130,246,0.45), 0 6px 18px var(--shadow);
      }
      .dropzone.drop-hover {
        box-shadow: 0 0 0 4px rgba(250,204,21,0.65), 0 6px 18px var(--shadow);
      }
      .footer-help {
        margin-top: 16px;
        background: rgba(255,255,255,0.08);
        border: 1px solid rgba(255,255,255,0.15);
        border-radius: 16px;
        padding: 12px 14px;
        color: var(--muted);
      }
      @media (max-width: 1100px) {
        :root {
          --card-width: 86px;
          --card-height: 120px;
          --overlap-down: 108px;
          --overlap-up: 24px;
        }
        .tableau-row { grid-template-columns: repeat(7, minmax(92px, 1fr)); }
      }
      @media (max-width: 860px) {
        .board-top { grid-template-columns: 1fr; }
        .foundations { justify-content: flex-start; }
        .tableau-row {
          overflow-x: auto;
          display: flex;
          gap: 10px;
          padding-bottom: 6px;
        }
        .tableau-col { min-width: 90px; }
      }
    ")),
    tags$script(HTML(" 
      document.addEventListener('DOMContentLoaded', function() {
        let dragPayload = null;

        function safeParse(txt) {
          try { return JSON.parse(txt); } catch(e) { return null; }
        }

        function clearDropHints() {
          document.querySelectorAll('.dropzone').forEach(el => {
            el.classList.remove('can-drop');
            el.classList.remove('drop-hover');
          });
        }

        document.addEventListener('dragstart', function(e) {
          const el = e.target.closest('[data-drag]');
          if (!el) return;
          dragPayload = safeParse(el.dataset.drag);
          if (e.dataTransfer) {
            e.dataTransfer.setData('text/plain', el.dataset.drag);
            e.dataTransfer.effectAllowed = 'move';
          }
          document.querySelectorAll('.dropzone').forEach(z => z.classList.add('can-drop'));
        });

        document.addEventListener('dragend', function() {
          dragPayload = null;
          clearDropHints();
        });

        document.addEventListener('dragover', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone) return;
          e.preventDefault();
          zone.classList.add('drop-hover');
        });

        document.addEventListener('dragleave', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone) return;
          zone.classList.remove('drop-hover');
        });

        document.addEventListener('drop', function(e) {
          const zone = e.target.closest('.dropzone');
          if (!zone) return;
          e.preventDefault();
          const target = safeParse(zone.dataset.drop || '');
          const source = dragPayload || safeParse(e.dataTransfer ? e.dataTransfer.getData('text/plain') : '');
          clearDropHints();
          dragPayload = null;
          if (!source || !target) return;
          if (window.Shiny) {
            Shiny.setInputValue('drag_move', { source: source, target: target, nonce: Date.now() }, {priority: 'event'});
          }
        });
      });
    "))
  ),
  div(
    class = "shell",
    div(
      class = "topbar",
      div(
        class = "titlebox",
        div(class = "app-title", "Solitaire Klondike"),
        p(class = "subtitle", "Version Shiny réécrite, proche du Solitaire Windows classique, avec thèmes de cartes personnalisables via dossiers PNG.")
      ),
      div(
        class = "controlbox",
        div(
          class = "controls-grid",
          div(actionButton("new_game", "Nouvelle partie", width = "100%")),
          div(selectInput("draw_n", "Tirage", choices = c("1 carte" = 1, "3 cartes" = 3), selected = 3, width = "100%")),
          div(uiOutput("theme_ui"))
        ),
        div(class = "help-text", HTML("Texte à ajouter."))
      ),
      div(
        class = "statusbox",
        div(class = "status-row",
            #div(class = "metric", div(class = "label", "Score"), textOutput("score", inline = TRUE)),
            #div(class = "metric", div(class = "label", "Coups"), textOutput("moves", inline = TRUE)),
            #div(class = "metric", div(class = "label", "Temps"), textOutput("elapsed", inline = TRUE))
        ),
        div(class = "message-line", textOutput("message", inline = TRUE))
      )
    ),
    div(
      class = "board-top",
      div(class = "stock-waste", uiOutput("stock_ui"), uiOutput("waste_ui")),
      div(class = "foundations", lapply(suits, function(s) uiOutput(paste0("foundation_", s))))
    ),
    div(class = "tableau-row", lapply(1:7, function(i) uiOutput(paste0("tableau_", i)))),
    div(
      class = "footer-help",
      HTML("<strong>Utilisation :</strong> cliquez sur le talon pour tirer. Cliquez sur une carte pour la sélectionner puis sur une destination, ou cliquez une deuxième fois sur la même carte pour tenter un déplacement automatique. Le glisser-déposer fonctionne aussi. <br><strong>Limite honnête :</strong> cette version cherche à reproduire le Solitaire Windows de très près, mais pas pixel par pixel ni avec toutes les animations natives de Microsoft.")
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(
    state = new_game_state(draw_n = 3)
  )

  timer <- reactiveTimer(1000)

  theme_choices <- reactive({
    unique(c("classic", available_themes()))
  })

  output$theme_ui <- renderUI({
    selectInput(
      "theme_name",
      "Thème de cartes",
      choices = setNames(theme_choices(), theme_choices()),
      selected = if (!is.null(input$theme_name) && input$theme_name %in% theme_choices()) input$theme_name else "classic",
      width = "100%"
    )
  })

  observeEvent(input$new_game, {
    rv$state <- new_game_state(draw_n = as.integer(input$draw_n))
  })

  observeEvent(input$draw_n, {
    current <- rv$state
    rv$state <- new_game_state(draw_n = as.integer(input$draw_n))
    rv$state$message <- paste("Nouvelle partie en mode tirage", input$draw_n)
  }, ignoreInit = TRUE)


  current_theme <- reactive({
    if (!is.null(input$theme_name)) return(input$theme_name)
    "classic"
  })

  current_session_theme_dir <- reactive({
    NULL
  })

  output$score <- renderText(rv$state$score)
  output$moves <- renderText(rv$state$moves)
  output$message <- renderText(rv$state$message)
  output$elapsed <- renderText({
    timer()
    secs <- max(0, as.integer(difftime(Sys.time(), rv$state$started_at, units = "secs")))
    sprintf("%02d:%02d", secs %/% 60, secs %% 60)
  })

  output$stock_ui <- renderUI({
    render_stock(rv$state, current_theme(), current_session_theme_dir())
  })
  output$waste_ui <- renderUI({
    render_waste(rv$state, current_theme(), current_session_theme_dir())
  })

  lapply(suits, function(s) {
    output[[paste0("foundation_", s)]] <- renderUI({
      render_foundation(rv$state, s, current_theme(), current_session_theme_dir())
    })
  })

  lapply(1:7, function(i) {
    output[[paste0("tableau_", i)]] <- renderUI({
      render_tableau(rv$state, i, current_theme(), current_session_theme_dir())
    })
  })

  observeEvent(input$card_click, {
    rv$state <- handle_click(rv$state, input$card_click)
    if (is_won(rv$state)) {
      rv$state$message <- "Bravo, partie gagnée !"
      rv$state$score <- rv$state$score + 100
    }
  })

  observeEvent(input$drag_move, {
    req(input$drag_move$source, input$drag_move$target)
    rv$state <- apply_drag_move(rv$state, input$drag_move$source, input$drag_move$target)
    rv$state$selected <- NULL
    if (is_won(rv$state)) {
      rv$state$message <- "Bravo, partie gagnée !"
      rv$state$score <- rv$state$score + 100
    }
  })
}

shinyApp(ui, server)
