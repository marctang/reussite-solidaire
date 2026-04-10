# Solitaire Shiny — version packagée

## Contenu

- `app.R` : l'application principale
- `www/cards/demo_classic/` : un thème de cartes PNG prêt à l'emploi
- `www/cards/demo_nuees/` : un second thème d'exemple
- `run_app.R` : script de lancement rapide
- `install_packages.R` : installation des packages nécessaires

## Lancer l'application

Dans R ou RStudio :

```r
setwd("CHEMIN/VERS/solitaire_shiny_windows_style")
source("install_packages.R")   # une seule fois si besoin
source("run_app.R")
```

Ou simplement :

```r
shiny::runApp()
```

### Option 1 — Dossier de thème

Créer un dossier comme :

```text
www/cards/mon_theme/
```

Puis y mettre les fichiers :

```text
AS.png  2S.png  3S.png ... KS.png
AH.png  ... KH.png
AD.png  ... KD.png
AC.png  ... KC.png
back.png
```

Extensions acceptées : `png`, `jpg`, `jpeg`, `webp`.

Ensuite le thème apparaîtra automatiquement dans la liste déroulante de l'application.


sans sous-dossier obligatoire.

## Commandes du jeu

- clic sur le **talon** : tirer 1 ou 3 cartes
- clic sur une carte : sélectionner
- clic sur une destination : déplacer
- glisser-déposer : déplacer une carte ou une pile
- double-clic logique via clic répété : tentative de déplacement automatique
- bouton **Nouvelle partie** : recommencer
- bouton **Mouvement auto** : essaie d'envoyer la sélection au meilleur endroit

Comment le déployer sur GitHub Pages

- Dans R : source("build_site.R")
- Push sur GitHub.
- Dans GitHub > Settings > Pages > source = Deploy from a branch > branch = main > folder = /docs



