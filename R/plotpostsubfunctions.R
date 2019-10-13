#---------------------------------------------------------------------------------------
# plotpostsubfunctions.R
# Leontine Alkema, 2012-2013
#---------------------------------------------------------------------------------------
PlotPostOnly <- function(# Plot histogram of posterior sample.
  ### Plot histogram of posterior sample.
  post.samp, ##<< Posterior sample from MCMC.
  parname = NULL, ##<< Parameter name for x-axis label.
  title = "" ##<< Plot title
) {
  par(mar = c(5, 5, ifelse(title == "", 1, 5), 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = title, xlim = c(minx, maxx))
}

PlotPostWithNormalPrior <- function(# Plot histogram of posterior sample.
  ### Plot histogram of posterior sample and add prior.
  post.samp, ##<< Posterior sample from MCMC.
  priormean, ##<< Prior mean of normal distribution.
  priorsd, ##<< Prior standard deviation of normal distribution.
  parname = NULL, ##<< Parameter name for x-axis label.
  title = "" ##<< Plot title
) {
  par(mar = c(5, 5, ifelse(title == "", 1, 5), 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = title, xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "test")

PlotPostWithUnifPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, ##<< Posterior sample from MCMC.
  priorlow, ##<< Prior lower bound of uniform distribution.
  priorup, ##<< Prior upper bound of uniform distribution.
  parname = NULL, ##<< Parameter name for x-axis label.
  title = "" ##<< Plot title
){
  par(mar = c(5, 5, ifelse(title == "", 1, 5), 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = title, xlim = c(minx, maxx))
  h <- 1/(priorup-priorlow)
  segments(priorlow, h, priorup, h, col = 2)
}
#PlotPostWithUnifPrior(post.samp = rnorm(100,0), priorlow = -2, priorup = 10, parname = "test")

PlotPostSDWithGammaPrior <- function(# Plot histogram of posterior sample of a SD parameter
  ### Plot histogram of posterior sample of a SD parameter, and add the prior, which is based on a Gamma for the precision
  post.samp, ##<< Posterior sample from MCMC.
  priorshape, ##<< Prior shape parameter of Gamma distribution
  priorrate, ##<< Prior rate parameter of Gamma distribution
  parname = NULL, ##<< Parameter name for x-axis label.
  title = "" ##<< Plot title
) {
  # more precisely, variance ~ InvGamma
  # often used: shape = halfnu0, rate = halfnu0*sigma2
  par(mar = c(5, 5, ifelse(title == "", 1, 5), 1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  minxprior <- ifelse(min(post.samp)<0, 1.2*min(post.samp), 0.8*min(post.samp))
  maxxprior <- ifelse(max(post.samp)<0, 0.8*max(post.samp), 1.2*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = title, xlim = c(minx, maxx))
  # note: when using "density", use to and from if sample has large negative/positive values!
  lines(density(1/sqrt(rgamma(10000, shape = priorshape, rate = priorrate)), from = minx, to = maxx), col = 2, lwd = 3)
  # change LA, 20140507: nsim for rgamma changed from 1000 to 10000
}
#PlotPostSDWithGammaPrior(post.samp = runif(100,0.01,1), priorshape = 0.5, priorrate = 0.5, parname = "test")
