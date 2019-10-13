#----------------------------------------------------------------------
# getbiasadjusteddata.R
# Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetBiasAdjustedData <- function(# Get bias-adjusted data based on posterior samples of \code{beta.csr}.
  data.hivremoved, ##<<  \code{mcmc.meta$data.hivremoved} from \code{\link{RunMCMC}}.
  mcmc.array, ##<< \code{mcmc.array} from \code{\link{ReadMCMCOutput}}.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles.
) {
  nsim <- dim(mcmc.array)[1]*dim(mcmc.array)[2]
  ubiasadj.Lcs.j <- ubiasadjlwr.Lcs.j <- ubiasadjupr.Lcs.j <- 
    ub1adj.Lcs.j <- ub1adjlwr.Lcs.j <- ub1adjupr.Lcs.j <- list()
  for (c in (1:data.hivremoved$C)[data.hivremoved$nnonvr.c[1:data.hivremoved$C] > 0]) {
    ubiasadj.Lcs.j[[c]] <- ubiasadjlwr.Lcs.j[[c]] <- ubiasadjupr.Lcs.j[[c]] <- 
      ub1adj.Lcs.j[[c]] <- ub1adjlwr.Lcs.j[[c]] <- ub1adjupr.Lcs.j[[c]] <- list()
    for(s in 1:data.hivremoved$nseriesnonvr.c[c]) {
      year.i <- data.hivremoved$year.Lcs.j[[c]][[s]]
      recallnotcentred.i <- data.hivremoved$surveyyear.Lc.s[[c]][s] - year.i
      recall.i <- data.hivremoved$surveyyear.Lc.s[[c]][s] - year.i - data.hivremoved$recall.mid
      fit.ij <- fit.b1adj.ij <- matrix(NA, length(recall.i), nsim)
      if (length(recall.i) > 1) {
        b2adj.ij <- sapply(c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",2]")]), "*", recall.i)
      } else {
        b2adj.ij <- matrix(sapply(c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",2]")]), "*", recall.i), 
                           length(recall.i), nsim)
      }
      fit.ij <- apply(1/exp(apply(b2adj.ij, 1, 
                                  "+", c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",1]")]))), 1, "*", 
                      data.hivremoved$u.Lcs.j[[c]][[s]])
      fit.b1adj.ij <- sapply(1/exp(c(mcmc.array[, , paste0("beta.csr[", c, ",", s, ",1]")])), 
                             "*", data.hivremoved$u.Lcs.j[[c]][[s]])
      if (length(recall.i) == 1) {
        fit.ij <- t(fit.ij)
        fit.b1adj.ij <- t(fit.b1adj.ij)      
      }
      fit.ri <- apply(fit.ij, 1, quantile, probs = percentiles)
      fit.b1adj.ri <- apply(fit.b1adj.ij, 1, quantile, probs = percentiles)
      # observations adjusted for biases in level and trend
      ubiasadj.Lcs.j[[c]][[s]] <- fit.ri[2, ]
      ubiasadjlwr.Lcs.j[[c]][[s]] <- fit.ri[1, ] 
      ubiasadjupr.Lcs.j[[c]][[s]] <- fit.ri[3, ]
      # observations adjusted for biases in level only
      ub1adj.Lcs.j[[c]][[s]] <- fit.b1adj.ri[2, ]
      ub1adjlwr.Lcs.j[[c]][[s]] <- fit.b1adj.ri[1, ]
      ub1adjupr.Lcs.j[[c]][[s]] <- fit.b1adj.ri[3, ]
    }
  }
  data.hivremoved.biasadjusted <- list(ubiasadj.Lcs.j = ubiasadj.Lcs.j, 
                                       ubiasadjlwr.Lcs.j = ubiasadjlwr.Lcs.j,
                                       ubiasadjupr.Lcs.j = ubiasadjupr.Lcs.j, 
                                       ub1adj.Lcs.j = ub1adj.Lcs.j,
                                       ub1adjlwr.Lcs.j = ub1adjlwr.Lcs.j,
                                       ub1adjupr.Lcs.j = ub1adjupr.Lcs.j)
  ##value<< List with bias-adjusted data.
  return(data.hivremoved.biasadjusted)
}
