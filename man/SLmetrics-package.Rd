% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SLmetrics-package.R
\docType{package}
\name{SLmetrics-package}
\alias{SLmetrics}
\alias{SLmetrics-package}
\title{{SLmetrics}: Machine Learning Performance Evaluation on Steroids}
\description{
\{SLmetrics\} is a lightweight package written in C++ for supervised and unsupervised Machine Learning applications. The package has been
developed with two primary goals in mind: memory management and execution speed. All functions are designed with internal pointers and references,
ensuring that passed objects are not copied into memory, resulting in optimized performance.
}
\section{Handling of Missing Values}{


\{SLmetrics\} does not provide explicit handling for missing values in either regression or classification applications. Users are advised
to ensure that their input data is preprocessed to remove or impute missing values before passing them to any functions.

Since the package heavily relies on pointers and references for performance, passing data with missing values may lead to undefined behavior,
including potential crashes of the R session.

For classification metrics that support micro and macro averages, \{SLmetrics\} does handle invalid values such as divisions by zero,
ensuring robust computation and accurate results.
}

\seealso{
Useful links:
\itemize{
  \item \url{https://slmetrics-docs.gitbook.io/v1}
  \item \url{https://github.com/serkor1/SLmetrics}
  \item Report bugs at \url{https://github.com/serkor1/SLmetrics/issues}
}

}
\author{
\strong{Maintainer}: Serkan Korkmaz \email{serkor1@duck.com} (\href{https://orcid.org/0000-0002-5052-0982}{ORCID}) [copyright holder]

}
\keyword{internal}
