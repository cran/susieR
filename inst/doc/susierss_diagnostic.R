## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
library(curl)

## -----------------------------------------------------------------------------
data("N3finemapping")
b = N3finemapping$true_coef[,1]
sumstats <- univariate_regression(N3finemapping$X, N3finemapping$Y[,1])
z_scores <- sumstats$betahat / sumstats$sebetahat
Rin = cor(N3finemapping$X)
attr(Rin, "eigen") = eigen(Rin, symmetric = T)
susie_plot(z_scores, y = "z", b=b)

## -----------------------------------------------------------------------------
s = estimate_s_rss(z_scores, Rin)
s

## -----------------------------------------------------------------------------
condz_in = kriging_rss(z_scores, Rin)
condz_in$plot

## -----------------------------------------------------------------------------
fit <- susie_rss(z_scores, Rin)
susie_plot(fit,y = "PIP", b=b)

## -----------------------------------------------------------------------------
data_file <- tempfile(fileext = ".RData")
data_url <- paste0("https://raw.githubusercontent.com/stephenslab/susieR/",
                   "master/inst/datafiles/SummaryConsistency1k.RData")
curl_download(data_url,data_file)
load(data_file)
zflip = SummaryConsistency$z
ld = SummaryConsistency$ldref
b = numeric(length(zflip))
b[SummaryConsistency$signal_id] = zflip[SummaryConsistency$signal_id]
plot(zflip, pch = 16, col = "#767676", main = "Marginal Associations", 
     xlab="SNP", ylab = "z scores")
points(SummaryConsistency$signal_id, zflip[SummaryConsistency$signal_id], col=2, pch=16)
points(SummaryConsistency$flip_id, zflip[SummaryConsistency$flip_id], col=7, pch=16)

## -----------------------------------------------------------------------------
fit = susie_rss(zflip, ld)
susie_plot(fit, y='PIP', b=b)
points(SummaryConsistency$flip_id, fit$pip[SummaryConsistency$flip_id], col=7, pch=16)

## -----------------------------------------------------------------------------
s = estimate_s_rss(zflip, ld)
s

## -----------------------------------------------------------------------------
condz = kriging_rss(zflip, ld)
condz$plot

## -----------------------------------------------------------------------------
z = zflip
z[SummaryConsistency$flip_id] = -z[SummaryConsistency$flip_id]
fit = susie_rss(z, ld)
susie_plot(fit, y='PIP', b=b)

## -----------------------------------------------------------------------------
sessionInfo()

