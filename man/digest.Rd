% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/digest.R
\name{digest}
\alias{digest}
\title{Proteolytic digest a parsed fasta list}
\usage{
digest(protein = NULL, ...)
}
\arguments{
\item{protein}{as character string}

\item{...}{parameters for \code{peptides()}}
}
\value{
a list
}
\description{
\code{digest()} Generates peptide sequences based on \emph{enzyme} and \emph{partial} inputs.
Only works with the "list" output of the \code{parse()} function
}
\examples{
\dontrun{
library(rfasta)
proteins <- parse("~/Local/fasta/ecoli_UniProt.fasta")
proteins <- digest(proteins, enzyme = "[K]", partial = 2)
}

}
