## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#")

## ---- warning=FALSE-----------------------------------------------------------
library(susieR)
library(L0Learn)

## -----------------------------------------------------------------------------
set.seed(1)
n = 1000
p = 1000
beta = rep(0,p)
beta[c(1,2,300,400)] = 1
X = matrix(rnorm(n*p),nrow=n,ncol=p)
y = X %*% beta + rnorm(n)

## -----------------------------------------------------------------------------
set.seed(1)
L0fit = L0Learn.cvfit(X, y, penalty = "L0")

## -----------------------------------------------------------------------------
lambdaIndex = which.min(L0fit$cvMeans[[1]]) 
L0coef = as.numeric(coef(L0fit$fit, lambda = L0fit$fit$lambda[[1]][lambdaIndex]))
effect.beta = L0coef[which(L0coef!=0)][-1]
effect.index = (which(L0coef!=0)-1)[-1] 
length(effect.beta)
effect.beta[1:10]
effect.index[1:10]

## -----------------------------------------------------------------------------
set.seed(1)
s.init = susie_init_coef(effect.index, effect.beta, p)

## -----------------------------------------------------------------------------
susieL0.fit = susie(X,y,s_init=s.init)
susieL0.fit$sets$cs

