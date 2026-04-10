if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Le package 'shiny' n'est pas installé. Lance d'abord source('install_packages.R').")
}
shiny::runApp(appDir = ".", launch.browser = TRUE)
