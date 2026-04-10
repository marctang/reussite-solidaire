packages <- c("shiny", "htmltools", "jsonlite")
to_install <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
cat("Packages OK\n")
