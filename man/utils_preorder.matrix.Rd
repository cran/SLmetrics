% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{preorder.matrix}
\alias{preorder.matrix}
\title{Preorder Matrices}
\usage{
\method{preorder}{matrix}(x, decreasing = FALSE, ...)
}
\arguments{
\item{x}{A <\link{matrix}> to be sorted}

\item{decreasing}{A <\link{logical}>}

\item{...}{Arguments passed into other methods}
}
\value{
A <\link{matrix}> of same dimensions as the input <\link{matrix}>
}
\description{
A generic S3 function for somehting long. This function dispatches to S3 methods in \code{\link[=preorder]{preorder()}} and performs no input validation. If you supply \link{NA} values or vectors of unequal \link{length} (e.g. \code{length(x) != length(y)}), the underlying \code{C++} code may trigger undefined behavior and crash your \code{R} session.
\subsection{Defensive measures}{

Because \code{\link[=preorder]{preorder()}} operates on raw pointers, pointer-level faults (e.g. from \link{NA} or mismatched \link{length}) occur before any \code{R}-level error handling.  Wrapping calls in \code{\link[=try]{try()}} or \code{\link[=tryCatch]{tryCatch()}} will \emph{not} prevent \code{R}-session crashes.

To guard against this, wrap \code{\link[=preorder]{preorder()}} in a “safe” validator that checks for \link{NA} values and matching \link{length}, for example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{safe_preorder <- function(x, y, ...) \{
  stopifnot(
    !anyNA(x), !anyNA(y),
    length(x) == length(y)
  )
  preorder(x, y, ...)
\}
}\if{html}{\out{</div>}}
}
}
\seealso{
Other Utilities: 
\code{\link{presort}()}
}
