## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE,
    warning = FALSE,
    cache = TRUE
)
library(tidyverse)
ggplot2::theme_set(cowplot::theme_half_open())
# ggplot2::theme_set(ggplot2::theme_minimal())


## -----------------------------------------------------------------------------------------------------------
library(readxl)
loyn <- read_xlsx("assets/mlr.xlsx", "Loyn")


## -----------------------------------------------------------------------------------------------------------
set.seed(100)
indexes <- sample(1:nrow(loyn), size = 0.2 * nrow(loyn))
loyn_train <- loyn[-indexes, ] # use this to create model
loyn_test <- loyn[indexes, ] # use this to validate model


## -----------------------------------------------------------------------------------------------------------
#| message: false
library(tidyverse)
glimpse(loyn_train)
glimpse(loyn_test)


## -----------------------------------------------------------------------------------------------------------
library(Hmisc)
hist(loyn_train, nclass = 20)


## -----------------------------------------------------------------------------------------------------------
loyn_train <- loyn_train %>%
    mutate(
        AREA_L10 = log10(AREA),
        LDIST_L10 = log10(LDIST),
        DIST_L10 = log10(DIST)
    )


## -----------------------------------------------------------------------------------------------------------
loyn_train <- loyn_train %>%
    select(-AREA, -LDIST, -DIST)
glimpse(loyn)


## -----------------------------------------------------------------------------------------------------------
hist(loyn_train, nclass = 20)


## -----------------------------------------------------------------------------------------------------------
full_fit <- lm(ABUND ~ ., data = loyn_train)
summary(full_fit)


## -----------------------------------------------------------------------------------------------------------
library(performance)
check_model(full_fit)


## -----------------------------------------------------------------------------------------------------------
step_fit <- step(full_fit, direction = "backward")


## -----------------------------------------------------------------------------------------------------------
summary(step_fit)


## -----------------------------------------------------------------------------------------------------------
check_model(step_fit)


## -----------------------------------------------------------------------------------------------------------
library(caret)
library(epiR)


## -----------------------------------------------------------------------------------------------------------
loyn_test <- loyn_test %>%
    mutate(
        AREA_L10 = log10(AREA),
        LDIST_L10 = log10(LDIST),
        DIST_L10 = log10(DIST)
    ) %>%
    select(-AREA, -LDIST, -DIST)


## -----------------------------------------------------------------------------------------------------------
RMSE(loyn_train$ABUND, predict(step_fit))
RMSE(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))


## -----------------------------------------------------------------------------------------------------------
mean(loyn_train$ABUND - predict(step_fit))
mean(loyn_test$ABUND - predict(step_fit, newdata = loyn_test))


## -----------------------------------------------------------------------------------------------------------
cor(loyn_train$ABUND, predict(step_fit))
cor(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))


## -----------------------------------------------------------------------------------------------------------
epi.ccc(loyn_train$ABUND, predict(step_fit))$rho.c
epi.ccc(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))$rho.c


## -----------------------------------------------------------------------------------------------------------
#| code-fold: true
# put all data into a tible and kable it
tibble(
    Dataset = c("Training", "Test"),
    RMSE = c(
        RMSE(loyn_train$ABUND, predict(step_fit)),
        RMSE(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))
    ),
    ME = c(
        mean(loyn_train$ABUND - predict(step_fit)),
        mean(loyn_test$ABUND - predict(step_fit, newdata = loyn_test))
    ),
    COR = c(
        cor(loyn_train$ABUND, predict(step_fit)),
        cor(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))
    ),
    CCC = c(
        epi.ccc(loyn_train$ABUND, predict(step_fit))$rho.c[[1]],
        epi.ccc(loyn_test$ABUND, predict(step_fit, newdata = loyn_test))$rho.c[[1]]
    )
) %>%
    knitr::kable()

