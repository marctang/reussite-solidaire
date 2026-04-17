# Version JavaScript statique du solitaire

Ce dossier contient une version légère du jeu, pensée pour GitHub Pages.

## Fichiers

- `docs/index.html` : page principale
- `docs/style.css` : styles
- `docs/game.js` : logique du jeu
- `docs/.nojekyll` : évite certains problèmes de publication GitHub Pages

## Mise en ligne sur GitHub Pages

### Méthode simple

1. Crée un dépôt GitHub ou utilise ton dépôt existant.
2. Place le contenu du dossier `docs/` dans le dossier `docs/` du dépôt.
3. Push sur GitHub.
4. Va dans **Settings > Pages**.
5. Dans **Build and deployment**, choisis **Deploy from a branch**.
6. Sélectionne la branche `main` puis le dossier `/docs`.
7. Enregistre.
8. GitHub publiera le site à l’adresse de ton dépôt Pages.

## Ajouter tes propres images de cartes

Par défaut, le jeu fonctionne sans images : il affiche des cartes dessinées en HTML/CSS.

Mais tu peux aussi ajouter des PNG personnalisés dans :

`docs/assets/cards/default/`

Noms attendus :

- `AS.png`, `2S.png`, ..., `KS.png`
- `AH.png`, ..., `KH.png`
- `AD.png`, ..., `KD.png`
- `AC.png`, ..., `KC.png`
- `back.png`

Si une image existe, elle sera utilisée automatiquement. Sinon, le jeu garde son rendu texte.

## Différence avec la version Shiny

Cette version ne dépend pas de Shiny, WebR ni Shinylive. Elle démarre donc beaucoup plus vite sur GitHub Pages.
