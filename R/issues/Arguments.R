#Fill out the arguments to test

data
data.val
indicator.type
runname.U5MR

run.type
year.lastestimate
periods.unsmooth.list = NULL
periods.smooth.list = NULL
periods.constant.list = NULL
hiv.file
adj.file
runname.igme
  ##<< Runname of UN IGME run of the previous year, 
## with \code{output/runname.igme} containing U5MR estimates stored as a \code{res.U5MR.rda}.
mean.b0 = ifelse(indicator.type == "U5MR", 4.02, 2.19) ##<< Mean of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.b1 = ifelse(indicator.type == "U5MR", -0.0889, 0.0516) ##<< Mean of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.u = ifelse(indicator.type == "U5MR", -0.000446, 0.0271) ##<< Mean of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.var.b0 = ifelse(indicator.type == "U5MR", 0.00129, 0.490) ##<< Mean variance of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.var.b1 = ifelse(indicator.type == "U5MR", 0.000110, 0.0342) ##<< Mean variance of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.sigma.u = ifelse(indicator.type == "U5MR", 0.0424, 0.206) ##<< Mean sd of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.