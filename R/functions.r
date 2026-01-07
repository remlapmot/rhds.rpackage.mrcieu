my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}

extract.participant <- function(id) {
  sub("TCGA-[^-]+-([^-]+)-.*", "\\1", id)
}

extract.tissue <- function(id) {
  sub("TCGA-[^-]+-[^-]+-([0-9]+)[^-]+-.*", "\\1", id)
}

extract.file <- function(tar.file, extract.file, new.file) {
  # get file path to extracted file
  x.file <-
    grep(extract.file,
      untar(tar.file, list = T),
      value = T
    )
    
  # extract the tar file
  cat("Extracting", tar.file, "to", new.file, "\n")
  untar(tar.file, exdir=resultsdir, extras="--no-same-owner")
  x.file = file.path(resultsdir,x.file)

  # move the data to named output
  file.copy(x.file, new.file)

  # remove untared directory
  unlink(dirname(x.file), recursive = TRUE)
}

my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}
