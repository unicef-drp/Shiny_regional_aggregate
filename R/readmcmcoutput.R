#----------------------------------------------------------------------
# readmcmcoutput.R
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
ReadMCMCOutput <- function( # Read in JAGS output and construct \code{mcmc.array} which is saved to 
  ## \code{output.dir}. This function can only be run after \code{RunMCMC} is complete.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  chain.ids = NULL, ##<< Optional: specify chain.ids to read (to use when chains are added).
  nsteps = NULL ##<< Optional: specify number of steps to read (to use when not all steps have finished).
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat("No mcmc.meta file in output.dir, run MCMC first!")
    return(invisible())
  }
  
  if (is.null(chain.ids) | is.null(nsteps)) {
    cat(paste("Reading in MCMC meta from", output.dir), "\n")
    load(file = file.path(output.dir, "mcmc.meta.rda"))
  }
  if (is.null(chain.ids))
    chain.ids <- mcmc.meta$general$chain.ids
  nchains <- length(chain.ids)
  # if (nchains == 1) {
  #   cat("You need at least two chains!")
  #   return()
  # }
  if (is.null(nsteps)) 
    nsteps <- mcmc.meta$general$nsteps
  load(paste0(output.dir, "temp.JAGSobjects/jags_mod", 1, "update_1.Rdata"))
  niterperstep <- dim(mod.upd$BUGSoutput$sims.array)[1]
  nsim <- nsteps*niterperstep
  npar <- dim(mod.upd$BUGSoutput$sims.array)[3]
  mcmc.array <- array(NA, c(nsim, nchains, npar))
  dimnames(mcmc.array) <- list(NULL, NULL, names(mod.upd$BUGSoutput$sims.array[1, 1, ]))
  for (chain in 1:nchains) {
    cat(paste0("Reading in chain number ", chain.ids[chain], " (step 1)"), "\n")
    load(paste0(output.dir,"temp.JAGSobjects/jags_mod", chain.ids[chain], "update_1.Rdata"))
    mcmc.array[1:niterperstep, chain, ] <- mod.upd$BUGSoutput$sims.array[, 1, ]
    if (nsteps > 1) {
      for (step in 2:nsteps) {
        cat(paste0("Reading in chain number ", chain.ids[chain], " (step ", step, ")"), "\n")
        load(paste0(output.dir, "temp.JAGSobjects/jags_mod", chain.ids[chain], "update_", step, ".Rdata"))
        mcmc.array[((step-1)*niterperstep+1):(step*niterperstep), chain, ] <- mod.upd$BUGSoutput$sims.array[, 1, ]
      }
    }
  }
  # take a random sample if mcmc.array is too big
  nsample.max = 10000000
  if (nsim > nsample.max)
    mcmc.array <- mcmc.array[seq(1, nsample.max, length.out = nsample.max), , , drop = FALSE]  
  save(mcmc.array, file = file.path(output.dir, "mcmc.array.rda"))
  ##value<< \code{NULL}; Saves \code{mcmc.array} to \code{output.dir}. 
  ##\code{mcmc.array} is an array of MCMC posterior samples with dimension \code{nsim} x \code{nchains} x \code{npar}.
  return(invisible())
}
