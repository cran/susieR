## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
set.seed(1)

## -----------------------------------------------------------------------------
data(N3finemapping)
attach(N3finemapping)
n = nrow(X)

## -----------------------------------------------------------------------------
dim(Y)

## -----------------------------------------------------------------------------
b <- true_coef[,1]
plot(b, pch=16, ylab='effect size')

## -----------------------------------------------------------------------------
which(b != 0)

## -----------------------------------------------------------------------------
sumstats <- univariate_regression(X, Y[,1])
z_scores <- sumstats$betahat / sumstats$sebetahat
susie_plot(z_scores, y = "z", b=b)

## -----------------------------------------------------------------------------
R <- cor(X)

## -----------------------------------------------------------------------------
fitted_rss1 <- susie_rss(bhat = sumstats$betahat, shat = sumstats$sebetahat, n = n, R = R, var_y = var(Y[,1]), L = 10,
                         estimate_residual_variance = TRUE)

## -----------------------------------------------------------------------------
summary(fitted_rss1)$cs

## -----------------------------------------------------------------------------
susie_plot(fitted_rss1, y="PIP", b=b)

## -----------------------------------------------------------------------------
fitted = susie(X, Y[,1], L = 10)
all.equal(fitted$pip, fitted_rss1$pip)
all.equal(coef(fitted)[-1], coef(fitted_rss1)[-1])

## -----------------------------------------------------------------------------
fitted_rss2 = susie_rss(z = z_scores, R = R, n = n, L = 10,
                        estimate_residual_variance = TRUE)

## ----fig.height=4, fig.width=3.5----------------------------------------------
all.equal(fitted$pip, fitted_rss2$pip)
plot(coef(fitted)[-1], coef(fitted_rss2)[-1], xlab = 'effects from SuSiE', ylab = 'effects from SuSiE-RSS', xlim=c(-1,1), ylim=c(-0.3,0.3))

## -----------------------------------------------------------------------------
fitted_standardize = susie(scale(X), scale(Y[,1]), L = 10)
all.equal(coef(fitted_standardize)[-1], coef(fitted_rss2)[-1])

## ----echo=F-------------------------------------------------------------------
set.seed(1)
tmp = matrix(rnorm(500*1001), 500, 1001)
eigenR = eigen(R)
eigenR$values[eigenR$values < 1e-10] = 0
X_ref = tmp %*% (sqrt(eigenR$values) * t(eigenR$vectors))
R_ref = cor(X_ref)

## -----------------------------------------------------------------------------
fitted_rss3 <- susie_rss(z_scores, R_ref, n=n, L = 10)

## -----------------------------------------------------------------------------
susie_plot(fitted_rss3, y="PIP", b=b)

## ----fig.width=3.5,fig.height=4-----------------------------------------------
plot(fitted_rss1$pip, fitted_rss3$pip, ylim=c(0,1), xlab='SuSiE PIP', ylab='SuSiE-RSS PIP')

## -----------------------------------------------------------------------------
fitted_rss4 = susie_rss(z_scores, R_ref, L = 10)
susie_plot(fitted_rss4, y="PIP", b=b)

## -----------------------------------------------------------------------------
sessionInfo()

