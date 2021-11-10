## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#",fig.width = 5,
                      fig.height = 3,fig.align = "center",
                      fig.cap = "&nbsp;",dpi = 120)

## -----------------------------------------------------------------------------
library(susieR)
library(curl)
data_file <- tempfile(fileext = ".RData")
data_url <- paste0("https://raw.githubusercontent.com/stephenslab/susieR/",
                   "master/inst/datafiles/FinemappingConvergence1k.RData")
curl_download(data_url,data_file)
load(data_file)
b <- FinemappingConvergence$true_coef
susie_plot(FinemappingConvergence$z, y = "z", b=b)

## -----------------------------------------------------------------------------
fitted <- with(FinemappingConvergence,
               susie_suff_stat(XtX = XtX, Xty = Xty, yty = yty, n = n))
susie_plot(fitted, y="PIP", b=b, main=paste0("ELBO = ", round(susie_get_objective(fitted),2)))

## -----------------------------------------------------------------------------
fitted_refine <- with(FinemappingConvergence,
                      susie_suff_stat(XtX = XtX, Xty = Xty, yty = yty,
					                  n = n, refine=TRUE))
susie_plot(fitted_refine, y="PIP", b=b, main=paste0("ELBO = ", round(susie_get_objective(fitted_refine),2)))

## -----------------------------------------------------------------------------
sessionInfo()

