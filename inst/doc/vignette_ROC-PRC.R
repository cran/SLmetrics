## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup--------------------------------------------------------------------
library(SLmetrics)

## -----------------------------------------------------------------------------
# 1) load data into namespace
data(
    banknote,
    package = "SLmetrics"
)

## -----------------------------------------------------------------------------
# 1) set seed
set.seed(1903)

# 2) extract indices
# for shuffling
noise <- sample(
    x = 1:nrow(banknote$features),
    size = nrow(banknote$features) * 0.50
)

# 3) reshuffle
# features and target
noise <- cbind(
    banknote$features[sample(noise),],
    target = banknote$target[sample(noise)]
)

## -----------------------------------------------------------------------------
# 1) convert to data.frame
# and head
head(
    banknote <- cbind(
        banknote$features,
        target = banknote$target
        )
)

# 2) introduce random
# noise to the data
# NOTE: wrapped in `try()` in case 
# noise is removed
try(
    expr = {
        banknote <- rbind(
        banknote,
        noise
    )
    },
    silent = TRUE
)

# 3) convert target to binary
# value
banknote$target <- as.numeric(
    banknote$target == "inauthentic"
)

## -----------------------------------------------------------------------------
# 1) set seed
set.seed(1903)

# 2) generate indices
index <- sample(
    x = 1:nrow(banknote),
    size = nrow(banknote) * 0.80
)

# 3) split data
# 3.1) training
train <- banknote[index,]
test  <- banknote[-index,]

## -----------------------------------------------------------------------------
# 1) train the logistic
# regression
model <- glm(
    formula = target ~ .,
    data    = train,
    family  = binomial(
        link = "logit"
    ) 
)

## -----------------------------------------------------------------------------
# 1) extract class
# probabilites
class_probabilities <- predict(
    object  = model,
    newdata = subset(test, select = -target),
    type    = "response"
)

# 2) calculate class
class_probabilities <- as.matrix(
    cbind(
        class_probabilities,
        1 - class_probabilities
    )
)

## -----------------------------------------------------------------------------
# 1) create actual
# value
actual <- factor(
    x = test$target,
    levels = c(1, 0),
    labels = c("inauthentic", "authentic")
)

## -----------------------------------------------------------------------------
# 1) construct precision-recall 
# object
print(
    precision_recall <- prROC(
        actual   = actual,
        response = class_probabilities
    )
)

## -----------------------------------------------------------------------------
plot(
    precision_recall
)

## -----------------------------------------------------------------------------
pr.auc(
    actual   = actual,
    response = class_probabilities
)

## -----------------------------------------------------------------------------
# 1) construct Receiver Operator Characteristics 
# object
print(
    receiver_operator_characteristics <- ROC(
        actual   = actual,
        response = class_probabilities
    )
)

## -----------------------------------------------------------------------------
plot(
    receiver_operator_characteristics
)

## -----------------------------------------------------------------------------
roc.auc(
    actual   = actual,
    response = class_probabilities
)

