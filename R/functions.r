#' Write table
#' @param x data frame to write
#' @param filename output file name
#' @return none
#' @export
my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  utils::write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}

#' Extract participant ID from TCGA barcode
#' 
#' The format of sample identifiers/barcodes is described here:
#' https://docs.gdc.cancer.gov/Encyclopedia/pages/TCGA_Barcode/
#'
#' Here is a summary:
#' e.g. TCGA-3C-AAAU-01A-11D-A41Q-05
#'   project TCGA
#'   tissue source site 3C
#'   participant AAAU
#'   sample 01 (01-09 tumor, 10-19 normal, 20-29 controls)
#'   vial A
#'   portion 11
#'   analyte D (as in DNA)
#'   plate A41Q
#'   analysis center 05
#'
#' The following function extracts the participant identifier
#' from a sample id/barcode.
#'
#' @param id TCGA barcode
#' @return participant ID
#' @export
extract.participant <- function(id) {
  sub("TCGA-[^-]+-([^-]+)-.*", "\\1", id)
}

#' Extract tissue code from TCGA barcode
#' 
#' The format of sample identifiers/barcodes is described here:
#' https://docs.gdc.cancer.gov/Encyclopedia/pages/TCGA_Barcode/
#'
#' Here is a summary:
#' e.g. TCGA-3C-AAAU-01A-11D-A41Q-05
#'   project TCGA
#'   tissue source site 3C
#'   participant AAAU
#'   sample 01 (01-09 tumor, 10-19 normal, 20-29 controls)
#'   vial A
#'   portion 11
#'   analyte D (as in DNA)
#'   plate A41Q
#'   analysis center 05
#'
#' The following function extracts the participant identifier
#' from a sample id/barcode.

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
