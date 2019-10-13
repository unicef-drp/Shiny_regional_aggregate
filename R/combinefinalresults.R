#----------------------------------------------------------------------
# combinefinalresults.R
#----------------------------------------------------------------------
CombineFinalResults <- function(
  runname.U5MR,
  runname.IMR, 
  crisis.free = FALSE, ##<< Exclude crisis adjustments?
  hiv.free = FALSE, ##<< Exclude HIV adjustments?
  weight.alpha.select = 0.5,
  country.info.file = NULL ##<< If \code{NULL}, country info included in package is used. 
) {
  if (is.null(country.info.file))
    country.info.file <- file.path("input", "country.info.CME.csv")
  country.info <- read.csv(file = country.info.file, header = T, stringsAsFactors = F, 
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
  
  filename.res <- ifelse(crisis.free & hiv.free, "res.crisisandhivremoved.cqt.Lw",
                         ifelse(crisis.free & !hiv.free, "res.crisisremoved.cqt.Lw",
                                ifelse(!crisis.free & hiv.free, "res.hivremoved.cqt.Lw",
                                       "res.cqt.Lw")))
  load(file = file.path("output", runname.U5MR, paste0(filename.res, ".rda")))
  eval(parse(text = paste0("res.cqt.Lw <- ", filename.res)))
  weights.alpha.plusdefault <- names(res.cqt.Lw)
  weights.alpha <- weights.alpha.plusdefault[-1]
  filename.append <- ifelse(crisis.free & hiv.free, ".crisisandhivfree",
                            ifelse(crisis.free & !hiv.free, ".crisisfree",
                                   ifelse(!crisis.free & hiv.free, ".hivfree", "")))
  if (weight.alpha.select == 0) { 
    filename <- paste0("u5", filename.append, ".ctj")
  } else {
    filename <- paste0("u5new", which(weights.alpha == weight.alpha.select), 
                       filename.append, ".ctj")
  }
  # load U5MR final results
  load(file = file.path("output", runname.U5MR, "iso.c.rda"))
  load(file = file.path("output", runname.U5MR, "name.c.rda"))
  load(file = file.path("output", runname.U5MR, "year.t.rda"))
  load(file = file.path("output", runname.U5MR, paste0(filename, ".rda")))
  eval(parse(text = paste0("u5.ctj <- ", filename)))
  eval(parse(text = paste0("rm(", filename, ")")))
  isoU5MR.c <- iso.c
  nameU5MR.c <- name.c
  yearU5MR.t <- year.t
  
  # load IMR final results
  load(file = file.path("output", runname.IMR, "iso.c.rda"))
  load(file = file.path("output", runname.IMR, "year.t.rda"))
  load(file = file.path("output", runname.IMR, paste0(filename, ".rda")))
  eval(parse(text = paste0("u1.ctj <- ", filename)))
  eval(parse(text = paste0("rm(", filename, ")")))
  isoIMR.c <- iso.c
  yearIMR.t <- year.t
  #----------------------------------------------------------------------
  # output .csv with point estimates # note: different ordering of ISOs here!
  country.info.iso <- country.info$ISO3Code[is.element(country.info$ISO3Code, isoU5MR.c)]
  resU5MR.ct <- roundoff(res.cqt.Lw[[paste0(weight.alpha.select)]][match(country.info.iso, isoU5MR.c), 2, ], 
                         digits = 1)
  load(file = file.path("output", runname.IMR, paste0(filename.res, ".rda")))
  eval(parse(text = paste0("res.cqt.Lw <- ", filename.res)))
  resIMR.ct <- roundoff(res.cqt.Lw[[paste0(weight.alpha.select)]][match(country.info.iso, isoIMR.c), 2, ],
                        digits = 1)
  colnames(resU5MR.ct) <- paste0("U5MR ", floor(yearU5MR.t))
  colnames(resIMR.ct) <- paste0("IMR ", floor(yearIMR.t))
  order <- order(country.info$CountryName[is.element(country.info$ISO3Code, country.info.iso)])
  Results.Table <- data.frame(country.info[is.element(country.info$ISO3Code, country.info.iso), ],
                              resU5MR.ct, resIMR.ct)[order, ]
  write.csv(Results.Table, file =  file.path("output", runname.U5MR, 
                                             paste0("Results.Table", filename.append, ".csv")), 
            row.names = F, na = "")
  cat(paste0("Results table written to ", 
             file.path("output", runname.U5MR, paste0("Results.Table", filename.append, ".csv")), ".\n"))
  #----------------------------------------------------------------------
  # format results for list
  isoboth.c <- isoU5MR.c
  nameboth.c <- nameU5MR.c
  yearboth.t <- union(yearU5MR.t, yearIMR.t)
  nsim <- dim(u5.ctj)[3]
  u5mrfinal.ctj <- imrfinal.ctj <- array(NA, c(length(isoboth.c), length(yearboth.t), nsim))
  u5mrfinal.ctj[, is.element(yearboth.t, yearU5MR.t), ] <- u5.ctj[, is.element(yearU5MR.t, yearboth.t), ]
  imrfinal.ctj[, is.element(yearboth.t, yearIMR.t), ] <- u1.ctj[match(isoboth.c, isoIMR.c), 
                                                              is.element(yearIMR.t, yearboth.t), ]
  u5mrfinal.ctj[u5mrfinal.ctj > 1000] <- 1000
  imrfinal.ctj[imrfinal.ctj > 1000] <- 1000
  
  # combine all results
  dimnames(u5mrfinal.ctj)[[1]] <- dimnames(imrfinal.ctj)[[1]] <- isoboth.c
  dimnames(u5mrfinal.ctj)[[2]] <- dimnames(imrfinal.ctj)[[2]] <- yearboth.t
  save(u5mrfinal.ctj, file = file.path("output", runname.U5MR, 
                                       paste0("u5mrfinal", filename.append, ".ctj.rda")))
  cat(paste0("Final U5MR trajectories written to ", 
             file.path("output", runname.U5MR, paste0("u5mrfinal", filename.append, ".ctj.rda")), ".\n"))
  save(imrfinal.ctj, file = file.path("output", runname.IMR, 
                                      paste0("imrfinal", filename.append, ".ctj.rda")))
  cat(paste0("Final IMR trajectories written to ", 
             file.path("output", runname.IMR, paste0("imrfinal", filename.append, ".ctj.rda")), ".\n"))
  
  # output list
  U5MRandIMRtrajectories.info.L <- list(runname.U5MR = runname.U5MR, runname.IMR = runname.IMR, 
 					     iso.c = isoboth.c, name.c = nameboth.c, year.t = yearboth.t, nsim = nsim)
  save(U5MRandIMRtrajectories.info.L, 
       file = file.path("output", runname.U5MR, 
                        paste0("U5MRandIMRtrajectories", filename.append, ".info.L.rda")))
  U5MRandIMRtrajectories.L <- list(runname.U5MR = runname.U5MR, runname.IMR = runname.IMR, 
                                   iso.c = isoboth.c, name.c = nameboth.c, year.t = yearboth.t, nsim = nsim,
                                   u5mrfinal.ctj = u5mrfinal.ctj, imrfinal.ctj = imrfinal.ctj)
  save(U5MRandIMRtrajectories.L, file = file.path("output", runname.U5MR, 
                                                  paste0("U5MRandIMRtrajectories", filename.append, ".L.rda")))
  cat(paste0("List of all U5MR and IMR trajectories written to ", 
             file.path("output", runname.U5MR, 
                       paste0("U5MRandIMRtrajectories", filename.append, ".L.rda")), ".\n"))
}
