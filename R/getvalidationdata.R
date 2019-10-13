#----------------------------------------------------------------------
# getvalidationdata.R
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetValidationData <- function(# Get data for validation
  data, 
  year.cutoff ##<< Cut-off year for validation exercise.
) {
  ntrain.c <- ntest.c <- ntrainnormdist.c <- ntraintdist.c <- 
    ntestnormdist.c <- ntesttdist.c <- rep(NA, data$C+1)
  geti.train.cj <- geti.test.cj <- 
  geti.trainnormdist.cj <- geti.traintdist.cj <- 
    geti.testnormdist.cj <- geti.testtdist.cj <- matrix(NA, data$C+1, data$nvrmax)
  excluded.surveys.Lc <- excludedobsandyears.Lc.i2 <- list()
  is.train.ci <- is.test.ci <- matrix(NA, data$C+1, data$nmax)
  # is.trainnormdist.ci <- is.traintdist.ci <-
  # is.testnormdist.ci <- is.testtdist.ci <- matrix(NA, data$C+1, data$nmax)
  
  for (c in 1:data$C) {
    excluded.surveys.Lc[[c]] <- NA
    surveyyears.m <- NULL
    if (data$nseriesnonvr.c[c] > 0) {
      excluded.surveys <- NULL
      for (s in 1:data$nseriesnonvr.c[c]) {
        # throw out after incl year.cutoff. if missing, use last obs year
        surveyyear <- ifelse(!is.na(data$surveyyear.Lc.s[[c]][s]) & data$surveyyear.Lc.s[[c]][s] > 0, 
                             data$surveyyear.Lc.s[[c]][s], max(data$year.Lcs.j[[c]][[s]]))
        if (surveyyear >= year.cutoff) {
          excluded.surveys <- c(excluded.surveys, s)
        }
        # add to a big surveyyears vector
        surveyyears.m <- c(surveyyears.m, rep(surveyyear, length(data$year.Lcs.j[[c]][[s]])))        
      }
      excluded.surveys.Lc[[c]] <- excluded.surveys
    }
    surveyyear.i <- c(surveyyears.m, unlist(data$yearvr.Lc.j[[c]]))
    
    # train and test sets
    is.train.i <- surveyyear.i < year.cutoff
    is.test.i <- surveyyear.i >= year.cutoff
    is.train.ci[c, 1:data$n.c[c]] <- is.train.i
    is.test.ci[c, 1:data$n.c[c]] <- is.test.i
    ntrain.c[c] <- sum(is.train.i)
    ntest.c[c] <- sum(is.test.i)
    if (ntrain.c[c] > 0) 
      geti.train.cj[c, 1:ntrain.c[c]] <- seq(1, data$n.c[c])[is.train.i]
    if (ntest.c[c] > 0) 
      geti.test.cj[c, 1:ntest.c[c]] <- seq(1, data$n.c[c])[is.test.i]
    
    # test set (excluded) obs and years
    u.i <- c(unlist(data$u.Lcs.j[[c]]), unlist(data$uvr.Lc.j[[c]]))
    year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
    excludedobsandyears.Lc.i2[[c]] <- rbind(cbind(u.i, year.i)[geti.test.cj[c, 1:ntest.c[c]], ],
                                            c(NA, NA))
    # train and test sets by normal and t distributions
    is.dhsdirectany.i <- is.element(c(unlist(data$sourceid.Lcs.j[[c]]), 
                                     rep("VR", data$nvr.c[c])),
                                   c(data$sourceid.Lc.s[[c]][data$isDHSdirectany.Lc.s[[c]] == 1], "VR"))
    # incomplete VR any observations are dropped
    if (data$nvr.c[c] == 0) { # change JR, 20140505
      is.incompletevrany.i <-  rep(FALSE, data$nvr.c[c])
    } else {
      is.incompletevrany.i <-  c(rep(FALSE, data$nnonvr.c[c]), data$isincompletevr.Lc.j[[c]] == 1)
    }
    if (ntrain.c[c] > 0) {
      is.trainnormdist.i <- surveyyear.i < year.cutoff & 
        is.dhsdirectany.i & !is.incompletevrany.i # change JR, 20140505
      is.traintdist.i <- surveyyear.i < year.cutoff & !is.dhsdirectany.i
      # is.trainnormdist.ci[c, 1:data$n.c[c]] <- is.trainnormdist.i
      # is.traintdist.ci[c, 1:data$n.c[c]] <- is.traintdist.i
      ntrainnormdist.c[c] <- sum(is.trainnormdist.i)
      ntraintdist.c[c] <- sum(is.traintdist.i)
      if (ntrainnormdist.c[c] > 0)
        geti.trainnormdist.cj[c, 1:ntrainnormdist.c[c]] <- seq(1, data$n.c[c])[is.trainnormdist.i]
      if (ntraintdist.c[c] > 0)
        geti.traintdist.cj[c, 1:ntraintdist.c[c]] <- seq(1, data$n.c[c])[is.traintdist.i]
    }
    if (ntest.c[c] > 0) {
      is.testnormdist.i <- surveyyear.i >= year.cutoff & is.dhsdirectany.i & !is.incompletevrany.i # change JR, 20140505
      is.testtdist.i <- surveyyear.i >= year.cutoff & !is.dhsdirectany.i
      # is.testnormdist.ci[c, 1:data$n.c[c]] <- is.testnormdist.i
      # is.testdist.ci[c, 1:data$n.c[c]] <- is.testdist.i
      ntestnormdist.c[c] <- sum(is.testnormdist.i)
      ntesttdist.c[c] <- sum(is.testtdist.i)
      if (ntestnormdist.c[c] > 0)
        geti.testnormdist.cj[c, 1:ntestnormdist.c[c]] <- seq(1, data$n.c[c])[is.testnormdist.i]
      if (ntesttdist.c[c] > 0)
        geti.testtdist.cj[c, 1:ntesttdist.c[c]] <- seq(1, data$n.c[c])[is.testtdist.i]
    }
  } # end country loop
  getc.train.d <- c(seq(1, data$C)[ntrain.c[1:data$C] > 0], NA)
  getc.test.d <- c(seq(1, data$C)[ntest.c[1:data$C] > 0], NA)
  getc.trainnormdist.d <- c(seq(1, data$C)[ntrainnormdist.c[1:data$C] > 0], NA) # VR and DHS Direct any with/without SE
  getc.traintdist.d <- c(seq(1, data$C)[ntraintdist.c[1:data$C] > 0], NA)
  getc.testnormdist.d <- c(seq(1, data$C)[ntestnormdist.c[1:data$C] > 0], NA) # VR and DHS Direct any with/without SE
  getc.testtdist.d <- c(seq(1, data$C)[ntesttdist.c[1:data$C] > 0], NA)
  Ctrain <- sum(!is.na(getc.train.d))
  Ctest <- sum(!is.na(getc.test.d))
  Ctrainnormdist <- sum(!is.na(getc.trainnormdist.d))
  Ctraintdist <- sum(!is.na(getc.traintdist.d))
  Ctestnormdist <- sum(!is.na(getc.testnormdist.d))
  Ctesttdist <- sum(!is.na(getc.testtdist.d))
  excluded.surveys.Lc[[data$C+1]] <- NA
  
  data.val <- list(Ctrain = Ctrain,
                   ntrain.c = ntrain.c, 
                   getc.train.d = getc.train.d,
                   geti.train.cj = geti.train.cj,
                   Ctest = Ctest,
                   ntest.c = ntest.c,
                   getc.test.d = getc.test.d,
                   geti.test.cj = geti.test.cj, 
                   Ctrainnormdist = Ctrainnormdist,
                   ntrainnormdist.c = ntrainnormdist.c,
                   getc.trainnormdist.d = getc.trainnormdist.d,
                   geti.trainnormdist.cj = geti.trainnormdist.cj,
                   Ctraintdist = Ctraintdist,
                   ntraintdist.c = ntraintdist.c,
                   getc.traintdist.d = getc.traintdist.d,
                   geti.traintdist.cj = geti.traintdist.cj,
                   Ctestnormdist = Ctestnormdist,
                   ntestnormdist.c = ntestnormdist.c,
                   getc.testnormdist.d =getc.testnormdist.d,
                   geti.testnormdist.cj = geti.testnormdist.cj,
                   Ctesttdist = Ctesttdist,
                   ntesttdist.c = ntesttdist.c,
                   getc.testtdist.d = getc.testtdist.d,
                   geti.testtdist.cj = geti.testtdist.cj, 
                   is.train.ci = is.train.ci,
                   is.test.ci = is.test.ci, 
                   # is.trainnormdist.ci = is.trainnormdist.ci,
                   # is.traintdist.ci = is.traintdist.ci,
                   # is.testnormdist.ci = is.testnormdist.ci,
                   # is.testtdist.ci = is.testtdist.ci
                   excluded.surveys.Lc = excluded.surveys.Lc,
                   excludedobsandyears.Lc.i2 = excludedobsandyears.Lc.i2)
  ##value<< List of validation data.
  return(data.val)
}
#----------------------------------------------------------------------
# plot test and training
#   pdf(file = paste0(output.dir, "/temp.pdf"), width = 21, height = 7)
#   for(c in 1:C) {
#     PlotAllDataC(data = data,
#                  c = c,
#                  plot.se = TRUE,
#                  excludedobsandyears.Lc.i2 = excludedobsandyears.Lc.i2, 
#                  igme = igme)
#   }
#   dev.off()
#--------------------------------------------------
# for paper: validation statistics
# summary(val.list$ntest.c/n.c[-(C+1)])
# summary(val.list$ntrain.c/n.c[-(C+1)])
## for non-vr countries
# summary(val.list$ntest.c[S.c[-(C+1)]>0]/n.c[-(C+1)][S.c[-(C+1)]>0])
