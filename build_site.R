# install.packages("shinylive")
# install.packages("shiny")
# install.packages("htmltools")

shinylive::export(
  appdir = ".",
  destdir = "docs"
)

file.create("docs/.nojekyll")
message("Export terminé dans le dossier docs/")