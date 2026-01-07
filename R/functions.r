#' Write table
#' @param x data frame to write
#' @param filename output file name
#' @return none
#' @export
my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}

#' Extract participant ID from TCGA barcode
#' @param id TCGA barcode
#' @return participant ID
#' @export
extract.participant <- function(id) {
  sub("TCGA-[^-]+-([^-]+)-.*", "\\1", id)
}

#' Extract tissue code from TCGA barcode
#' @param id TCGA barcode
#' @return tissue code
#' @export
extract.tissue <- function(id) {
  sub("TCGA-[^-]+-[^-]+-([0-9]+)[^-]+-.*", "\\1", id)
}

#' Extract a specific file from a tar archive
#' @param tar.file path to tar file
#' @param extract.file file to extract from tar archive
#' @param new.file output file name
#' @param resultsdir directory to extract files to
#' @return none
#' @export
extract.file <- function(tar.file, extract.file, new.file, resultsdir) {
  # get file path to extracted file
  x.file <-
    grep(extract.file,
      utils::untar(tar.file, list = T),
      value = T
    )
    
  # extract the tar file
  cat("Extracting", tar.file, "to", new.file, "\n")
  utils::untar(tar.file, exdir=resultsdir, extras="--no-same-owner")
  x.file = file.path(resultsdir,x.file)

  # move the data to named output
  file.copy(x.file, new.file)

  # remove untared directory
  unlink(dirname(x.file), recursive = TRUE)
}
