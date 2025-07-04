% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3_Accuracy.R
\name{accuracy}
\alias{accuracy}
\alias{weighted.accuracy}
\title{Accuracy}
\usage{
## Generic S3 method
## for Accuracy
accuracy(...)

## Generic S3 method
## for weighted Accuracy
weighted.accuracy(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=accuracy.factor]{accuracy.factor}}, \code{\link[=weighted.accuracy.factor]{weighted.accuracy.factor}}, \code{\link[=accuracy.cmatrix]{accuracy.cmatrix}}
  \describe{
    \item{\code{actual,predicted}}{A pair of <\link{integer}> or <\link{factor}> vectors of \link{length} \eqn{n}, and \eqn{k} levels.}
    \item{\code{w}}{A <\link{double}> vector of sample weights.}
    \item{\code{x}}{A confusion matrix created \code{\link[=cmatrix]{cmatrix()}}.}
  }}
}
\value{
A <\link{double}>-value
}
\description{
A generic S3 function to compute the \emph{accuracy} score for a classification model. This function dispatches to S3 methods in \code{\link[=accuracy]{accuracy()}} and performs no input validation. If you supply \link{NA} values or vectors of unequal \link{length} (e.g. \code{length(x) != length(y)}), the underlying \code{C++} code may trigger undefined behavior and crash your \code{R} session.
\subsection{Defensive measures}{

Because \code{\link[=accuracy]{accuracy()}} operates on raw pointers, pointer-level faults (e.g. from \link{NA} or mismatched \link{length}) occur before any \code{R}-level error handling.  Wrapping calls in \code{\link[=try]{try()}} or \code{\link[=tryCatch]{tryCatch()}} will \emph{not} prevent \code{R}-session crashes.

To guard against this, wrap \code{\link[=accuracy]{accuracy()}} in a "safe" validator that checks for \link{NA} values and matching \link{length}, for example:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{safe_accuracy <- function(x, y, ...) \{
  stopifnot(
    !anyNA(x), !anyNA(y),
    length(x) == length(y)
  )
  accuracy(x, y, ...)
\}
}\if{html}{\out{</div>}}

Apply the same pattern to any custom metric functions to ensure input sanity before calling the underlying \code{C++} code.
}

\subsection{Efficient multi-metric evaluation}{

For multiple performance evaluations of a classification model, first compute the confusion matrix once via \code{\link[=cmatrix]{cmatrix()}}. All other performance metrics can then be derived from this one object via S3 dispatching:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{## compute confusion matrix
confusion_matrix <- cmatrix(actual, predicted)

## evaluate accuracy
## via S3 dispatching
accuracy(confusion_matrix)

## additional performance metrics
## below
}\if{html}{\out{</div>}}

The \code{\link[=accuracy.factor]{accuracy.factor()}} method calls \code{\link[=cmatrix]{cmatrix()}} internally, so explicitly invoking \code{\link[=accuracy.cmatrix]{accuracy.cmatrix()}} yourself avoids duplicate computation, yielding significant speed and memory effciency gains when you need multiple evaluation metrics.
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

## Evaluate performance
SLmetrics::accuracy(
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
\code{\link{auc.pr.curve}()},
\code{\link{auc.roc.curve}()},
\code{\link{baccuracy}()},
\code{\link{brier.score}()},
\code{\link{ckappa}()},
\code{\link{cmatrix}()},
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
\code{\link{auc.pr.curve}()},
\code{\link{auc.roc.curve}()},
\code{\link{baccuracy}()},
\code{\link{brier.score}()},
\code{\link{ccc}()},
\code{\link{ckappa}()},
\code{\link{cmatrix}()},
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
\concept{Classification}
\concept{Machine learning}
\concept{Performance evaluation}
\concept{Statistical learning}
\concept{Supervised Learning}
\keyword{classification}
\keyword{evaluation}
\keyword{regression}
