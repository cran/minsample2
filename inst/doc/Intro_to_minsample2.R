## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(minsample2)

## -----------------------------------------------------------------------------
#example-1: minimum sample size required for epsilon = 0.5 and mean=0, sd=1(default)
l_norm(1:10,0.5,0,1)

#example-2: minimum sample sizes required for epsilons = 0.7 and 0.5 for the default mean and variance.
l_norm(1:10,c(0.7,0.5))

## -----------------------------------------------------------------------------
#example-3: minimum sample size required for epsilon = 0.1 and mean=1, sd=3
l_norm(1:10,0.5,1,3)

