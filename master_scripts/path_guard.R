root <- normalizePath(getwd())

if (grepl("Orig", root, ignore.case = TRUE)) {
  stop("You are running inside the Orig directory. Analysis must be run inside the Analysis folder.")
}