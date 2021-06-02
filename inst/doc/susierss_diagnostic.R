## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)

## -----------------------------------------------------------------------------
data("N3finemapping")
b = N3finemapping$true_coef[,1]
sumstats <- univariate_regression(N3finemapping$X, N3finemapping$Y[,1])
z_scores <- sumstats$betahat / sumstats$sebetahat
Rin = cor(N3finemapping$X)
attr(Rin, 'eigen') = eigen(Rin, symmetric = T)
susie_plot(z_scores, y = "z", b=b)

## -----------------------------------------------------------------------------
s = estimate_s_rss(z_scores, Rin)
s

## -----------------------------------------------------------------------------
condz_in = kriging_rss(z_scores, Rin)
condz_in$plot

## -----------------------------------------------------------------------------
data("SummaryConsistency")
zflip = SummaryConsistency$z
ld = SummaryConsistency$ldref
plot(zflip, pch = 16, col = '#767676', main = 'Marginal Associations', 
     xlab='SNP', ylab = 'z scores')
points(SummaryConsistency$signal_id, zflip[SummaryConsistency$signal_id], col=2, pch=16)
points(SummaryConsistency$flip_id, zflip[SummaryConsistency$flip_id], col=7, pch=16)

## -----------------------------------------------------------------------------
s = estimate_s_rss(zflip, ld)
s

## -----------------------------------------------------------------------------
condz = kriging_rss(zflip, ld)
condz$plot

## -----------------------------------------------------------------------------
sessionInfo()

