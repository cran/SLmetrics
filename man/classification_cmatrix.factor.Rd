% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{cmatrix.factor}
\alias{cmatrix.factor}
\title{Confusion Matrix}
\usage{
\method{cmatrix}{factor}(actual, predicted, ...)
}
\arguments{
\item{actual, predicted}{A pair of <\link{integer}> or <\link{factor}> vectors of \link{length} \eqn{n}, and \eqn{k} levels.}

\item{...}{Arguments passed into other methods.}
}
\value{
A named \eqn{k} x \eqn{k} <\link{matrix}>
}
\description{
A generic S3 function to compute the \emph{confusion matrix}  for a classification model. This function dispatches to S3 methods in \code{\link[=cmatrix]{cmatrix()}} and performs no input validation. If you supply \link{NA} values or vectors of unequal \link{length} (e.g. \code{length(x) != length(y)}), the underlying \code{C++} code may trigger undefined behavior and crash your \code{R} session.
\subsection{Defensive measures}{

Because \code{\link[=cmatrix]{cmatrix()}} operates on raw pointers, pointer-level faults (e.g. from \link{NA} or mismatched \link{length}) occur before any \code{R}-level error handling.  Wrapping calls in \code{\link[=try]{try()}} or \code{\link[=tryCatch]{tryCatch()}} will \emph{not} prevent \code{R}-session crashes.

To guard against this, wrap \code{\link[=cmatrix]{cmatrix()}} in a "safe" validator that checks for \link{NA} values and matching \link{length}, for example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{safe_cmatrix <- function(x, y, ...) \{
  stopifnot(
    !anyNA(x), !anyNA(y),
    length(x) == length(y)
  )
  cmatrix(x, y, ...)
\}
}\if{html}{\out{</div>}}

Apply the same pattern to any custom metric functions to ensure input sanity before calling the underlying \code{C++} code.
}

\subsection{The workhorse}{

\code{\link[=cmatrix]{cmatrix()}} is the main function for classification metrics with cmatrix S3 dispatch. These functions internally calls \code{\link[=cmatrix]{cmatrix()}}, so there is a signficant gain in computing the confusion matrix first, and then pass it onto the metrics.
For example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{## Compute confusion matrix
confusion_matrix <- cmatrix(actual, predicted)

## Evaluate accuracy
## via S3 dispatching
accuracy(confusion_matrix)

## Evaluate recall
## via S3 dispatching
recall(confusion_matrix)
}\if{html}{\out{</div>}}
}
}
\section{Dimensions}{

There is no robust defensive measure against misspecifying the confusion matrix. If the arguments are passed correctly, the resulting
confusion matrix is on the form:\tabular{lcr}{
    \tab A (Predicted) \tab B (Predicted) \cr
   A (Actual) \tab Value \tab Value \cr
   B (Actual) \tab Value \tab Value \cr
}
}

\examples{
## Classes and
## seed
set.seed(1903)
classes <- c("Kebab", "Falafel")

## Generate actual
## and predicted classes
actual_classes <- factor(
    x = sample(x = classes, size = 1e3, replace = TRUE),
    levels = c("Kebab", "Falafel")
)

predicted_classes <- factor(
    x = sample(x = classes, size = 1e3, replace = TRUE),
    levels = c("Kebab", "Falafel")
)

## Compute confusion matrix
SLmetrics::cmatrix(
   actual    = actual_classes, 
   predicted = predicted_classes
)



}
\references{
James, Gareth, et al. An introduction to statistical learning. Vol. 112. No. 1. New York: springer, 2013.

Hastie, Trevor. "The elements of statistical learning: data mining, inference, and prediction." (2009).

Pedregosa, Fabian, et al. "Scikit-learn: Machine learning in Python." the Journal of machine Learning research 12 (2011): 2825-2830.
}
\seealso{
Other Classification: 
\code{\link{accuracy}()},
\code{\link{auc.pr.curve}()},
\code{\link{auc.roc.curve}()},
\code{\link{baccuracy}()},
\code{\link{brier.score}()},
\code{\link{ckappa}()},
\code{\link{cross.entropy}()},
\code{\link{dor}()},
\code{\link{fbeta}()},
\code{\link{fdr}()},
\code{\link{fer}()},
\code{\link{fmi}()},
\code{\link{fpr}()},
\code{\link{hammingloss}()},
\code{\link{jaccard}()},
\code{\link{logloss}()},
\code{\link{mcc}()},
\code{\link{nlr}()},
\code{\link{npv}()},
\code{\link{plr}()},
\code{\link{pr.curve}()},
\code{\link{precision}()},
\code{\link{recall}()},
\code{\link{relative.entropy}()},
\code{\link{roc.curve}()},
\code{\link{shannon.entropy}()},
\code{\link{specificity}()},
\code{\link{zerooneloss}()}

Other Supervised Learning: 
\code{\link{accuracy}()},
\code{\link{auc.pr.curve}()},
\code{\link{auc.roc.curve}()},
\code{\link{baccuracy}()},
\code{\link{brier.score}()},
\code{\link{ccc}()},
\code{\link{ckappa}()},
\code{\link{cross.entropy}()},
\code{\link{deviance.gamma}()},
\code{\link{deviance.poisson}()},
\code{\link{deviance.tweedie}()},
\code{\link{dor}()},
\code{\link{fbeta}()},
\code{\link{fdr}()},
\code{\link{fer}()},
\code{\link{fmi}()},
\code{\link{fpr}()},
\code{\link{gmse}()},
\code{\link{hammingloss}()},
\code{\link{huberloss}()},
\code{\link{jaccard}()},
\code{\link{logloss}()},
\code{\link{maape}()},
\code{\link{mae}()},
\code{\link{mape}()},
\code{\link{mcc}()},
\code{\link{mpe}()},
\code{\link{mse}()},
\code{\link{nlr}()},
\code{\link{npv}()},
\code{\link{pinball}()},
\code{\link{plr}()},
\code{\link{pr.curve}()},
\code{\link{precision}()},
\code{\link{rae}()},
\code{\link{recall}()},
\code{\link{relative.entropy}()},
\code{\link{rmse}()},
\code{\link{rmsle}()},
\code{\link{roc.curve}()},
\code{\link{rrmse}()},
\code{\link{rrse}()},
\code{\link{rsq}()},
\code{\link{shannon.entropy}()},
\code{\link{smape}()},
\code{\link{specificity}()},
\code{\link{zerooneloss}()}
}
\concept{Machine learning performance evaluation}
\keyword{classification}
\keyword{evaluation}
