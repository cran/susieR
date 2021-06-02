## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 4.5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
set.seed(1)
n    <- 1000
p    <- 1000
beta <- rep(0,p)
beta[c(1,2,300,400)] <- 1
X   <- matrix(rnorm(n*p),nrow=n,ncol=p)
y   <- X %*% beta + rnorm(n)
res <- susie(X,y,L=10)
plot(coef(res)[-1],pch = 20)

## ----fig.height=3.5, fig.width=3.5--------------------------------------------
plot(y,predict(res),pch = 20)

## -----------------------------------------------------------------------------
sessionInfo()

