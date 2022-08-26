#' The main function for parsing a fasta file
#'
#' @description
#' `parse()` get the current regex
#'
#' @param fasta_path a character string of the path to the fasta formatted file
#' @param patterns a list, if not provided the default from `regex()` will be used.
#' *Note*: the first element in the regex list will define the list reference name, such
#' that with the list output, each protein can be accessed with that designation.
#' *Note*: if the patterns list is missing an explicit "sequence" element, no sequence will
#' be returned. This might be beneficial if only a few meta elements are sought.
#' @param as a character designating the output format
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' library(rfasta)
#' proteins <- parse("~/Local/fasta/ecoli_UniProt.fasta")
#'
#' # using a custom supplied regex list
#' proteins <- parse(fasta_path = "~/Local/fasta/ecoli_UniProt.fasta",
#'                   pattern = list(
#'                       "accession" = "sp\\|[A-Z]",
#'                       "gene_name" = "(?<=GN\\=).*?(?=\\s..\\=)"
#'                   ))
#' }
#'
parse <- function(fasta_path = NULL,
                  patterns = NULL,
                  as = c('list','data.frame')
){

  as <- rlang::arg_match(as)
  if(is.null(fasta_path)) {cli::cli_abort(c("x" = "fasta_path is empty"))}
  if(is.null(patterns)) { patterns <- regex() }
  if(mode(patterns) != 'list') {cli::cli_abort(c("x" = "patterns is `{mode(patterns)}`, should be a list"))}

  cli::cli_process_start("Parsing FASTA file {basename(fasta_path)}")

  tryCatch({

    # read in fasta file
    l_fasta <- unlist(base::strsplit(readr::read_file(fasta_path), ">"))
    l_fasta <- l_fasta[-1] # first in list is blank

    names(l_fasta) <- unlist(parallel::mclapply(l_fasta, extract, patterns[1]))
    l_fasta <- parallel::mclapply(l_fasta, extract, patterns)

  }, error = function(err) {
    err = as.character(as.vector(err))
    cli::cli_process_failed()
    cli::cli_abort(err)
  })
  cli::cli_process_done()

  if(as == "list") {
    return(l_fasta)
  } else {
    l_fasta <- parallel::mclapply(l_fasta, as.data.frame)
    return(Reduce(function(x, y) merge(x, y, all=TRUE), l_fasta))
  }
}
