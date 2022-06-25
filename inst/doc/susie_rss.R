## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      dpi = 120)

## ----load-pkgs----------------------------------------------------------------
library(susieR)

## ----simdata------------------------------------------------------------------
set.seed(1)
n <- 200
p <- 1000
beta <- rep(0,p)
beta[1:4] <- 1
X <- matrix(rnorm(n*p),nrow = n,ncol = p)
X <- scale(X,center = TRUE,scale = FALSE)
y <- drop(X %*% beta + rnorm(n))

## ----sumstats-no-standardize--------------------------------------------------
ss  <- univariate_regression(X,y)
dat <- compute_suff_stat(X,y,standardize = FALSE)
R   <- cov2cor(dat$XtX)

## ----first-comparison, fig.height=3.5, fig.width=3----------------------------
res1 <- susie(X,y,L = 10)
res2 <- susie_rss(bhat = ss$betahat,shat = ss$sebetahat,R = R,n = n,
                  var_y = var(y),L = 10,estimate_residual_variance = TRUE)
plot(coef(res1),coef(res2),pch = 20,xlab = "susie",ylab = "susie_rss")
abline(a = 0,b = 1,col = "skyblue",lty = "dashed")

## ----sumstats-standardize-1---------------------------------------------------
ss  <- univariate_regression(scale(X),scale(y))
dat <- compute_suff_stat(X,y,standardize = TRUE)
R   <- cov2cor(dat$XtX)

## ----sumstats-standardize-2---------------------------------------------------
zhat <- ss$betahat/ss$sebetahat

## ----second-comparison, fig.height=3.5, fig.width=6, message=FALSE------------
res1 <- susie(scale(X),scale(y),L = 10)
res2 <- susie_rss(bhat = ss$betahat,shat = ss$sebetahat,R = R,n = n,
                  L = 10,estimate_residual_variance = TRUE)
res3 <- susie_rss(zhat,R,n = n,L = 10,estimate_residual_variance = TRUE)
layout(matrix(1:2,1,2))
plot(coef(res1),coef(res2),pch = 20,xlab = "susie",
          ylab = "susie_rss(bhat,shat)")
abline(a = 0,b = 1,col = "skyblue",lty = "dashed")
plot(coef(res1),coef(res3),pch = 20,xlab = "susie",ylab = "susie_rss(z)")
abline(a = 0,b = 1,col = "skyblue",lty = "dashed")

## ----third-comparison, fig.height=3.5, fig.width=3, message=FALSE-------------
res4 <- susie_rss(zhat,R,n = n,L = 10)
plot(coef(res1),coef(res4),pch = 20,xlab = "susie",ylab = "susie_rss")
abline(a = 0,b = 1,col = "skyblue",lty = "dashed")

## ----fourth-comparison, fig.height=3.5, fig.width=3, message=FALSE, warning=FALSE----
res5 <- susie_rss(zhat,R,L = 10)
plot(coef(res1),coef(res5)/sqrt(n),pch = 20,xlab = "susie",
ylab = "susie_rss/sqrt(n)")
abline(a = 0,b = 1,col = "skyblue",lty = "dashed")

