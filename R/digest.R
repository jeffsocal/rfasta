#' Proteolytic digest a parsed fasta list
#'
#' @description
#' `digest()` Generates peptide sequences based on *enzyme* and *partial* inputs.
#' Only works with the "list" output of the `parse()` function
#'
#' @param protein as character string
#' @param ... parameters for `peptides()`
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' library(rfasta)
#' proteins <- parse("~/Local/fasta/ecoli_UniProt.fasta")
#' proteins <- digest(proteins, enzyme = "[K]", partial = 2)
#' }
#'
digest <- function(
    protein = NULL,
    ...
){
  if(mode(protein) != "list") {cli::cli_abort(c("x" = "protein is `{mode(protein)}`, should be an list"))}
  if(!"sequence" %in% names(protein)) { return("wtf") }
  protein[['peptides']] <- peptides(protein$sequence, ...)
  return(protein)
}
