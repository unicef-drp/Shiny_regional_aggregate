#----------------------------------------------------------------------
# deriveq4fromu5mrandimr.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
Deriveq4FromU5MRandIMR <- function(
  iso.select,
  runname = NULL,
  runname.U5MR,
  runname.IMR,
  country.B3info.file = NULL, ##<< If \code{NULL}, country info included in package is used.
  year1 = 1990.5, 
  year2 = 2000.5, 
  year3 = 2005.5, 
  year4 = 2015.5,
  weight.alpha.select = 0.5,
  percentiles = c(0.05, 0.5, 0.95),
  output.dir = NULL ##<< Directory where results output will be stored or if \code{NULL}, 
  ## directory \code{output/runname} is created in current working directory
) {
  if (is.null(runname))
    runname <- paste0("q4_from", runname.U5MR, "&", runname.IMR)
  if (is.null(country.B3info.file))
    country.B3info.file <- file.path("input", "infoUNinclHIV.csv")
  info <- read.csv(country.B3info.file, header = T, stringsAsFactors = F, strip.white = T)
  c <- which(info$iso.c == iso.select)
  
  # assume same set of weights.alpha are used for runname.U5MR and runname.IMR
  load(file.path("output", runname.U5MR, "res.cqt.Lw.rda"))
  weights.alpha <- names(res.cqt.Lw)[names(res.cqt.Lw) != "0"]
  w <- which(weights.alpha == weight.alpha.select)  
  files.to.load <- c("year.t.rda", "res.cqt.Lw.rda", paste0("u5new", w, ".ctj.rda")) 
  if (info$imrmethod.c[c] != "B3") {
    cat(paste0("B3 estimates are not used for ", info$name.c[c], " (", info$iso.c[c], ")", ".",  "\n"))
    return()
  } else 
  if (sum(c(!file.exists(file.path("output", runname.U5MR, files.to.load)), 
                   !file.exists(file.path("output", runname.IMR, files.to.load)))) > 0) {
    cat("Error: Results year.t.rda and/or res.cqt.Lw.rda and/or u5.ctj.rda is/are not found for ", 
        runname.U5MR, " and/or ", runname.IMR,  "!",  "\n")
    return()
  }
  cat(paste0("Deriving q4 estimates from q5 and q1 for ", iso.select, "...\n"))
  if (is.null(output.dir)) {
    dir.create(file.path(getwd(), "output"), showWarnings = FALSE) 
    dir.create(file.path(getwd(), "output", runname), showWarnings = FALSE) 
    output.dir <- paste0(getwd(), "/output/", runname, "/")
  }
  sapply(files.to.load, LoadFile, output.dir = file.path("output", runname.IMR), 
         envir = environment())
  year.t.final <- year.t
  res.cqt.Lw.IMR <- res.cqt.Lw
  eval(parse(text = paste0("u1new", w, ".ctj <- u5new", w, ".ctj")))
  sapply(files.to.load, LoadFile, output.dir = file.path("output", runname.U5MR), 
         envir = environment())
  # declare variables
  res.cqt.Lw <- res.cqt.Lw.IMR
  for (weight in 1:length(res.cqt.Lw)) {
    res.cqt.Lw[[weight]][] <- NA
  }
  eval(parse(text = paste0("u5.tj <- u5new", w, ".ctj[1, is.element(year.t, year.t.final), ]")))
  eval(parse(text = paste0("u1.tj <- u1new", w, ".ctj[1, , ]")))
  if (ncol(u5.tj) != ncol(u1.tj)) {
    cat(paste0("Warning: ", iso.select, " - u5.tj has ", ncol(u5.tj), " trajectories while u1.tj has ",
               ncol(u1.tj), " trajectories. "))
    ntrajs <- min(ncol(u5.tj), ncol(u1.tj))
    cat(paste0("The first ", ntrajs, " trajectories for u5.tj and u1.tj are used.\n"))
    u5.tj <- u5.tj[, 1:ntrajs]
    u1.tj <- u1.tj[, 1:ntrajs]
  }
  # check that u1.tj <= u5.tj for all t and j
  select.trajs.exclude <- apply(u1.tj > u5.tj, 2, sum) > 0
  if (sum(select.trajs.exclude) > 0)
    cat(paste0("Warning: ", iso.select, " - Number of trajectories excluded (where IMR > U5MR) = ", 
               sum(select.trajs.exclude), " out of ", length(select.trajs.exclude), " trajectories.\n"))
  q4.tj <- (1-(1-u5.tj[, !select.trajs.exclude]/1000)/(1-u1.tj[, !select.trajs.exclude]/1000))*1000
  # get results 
  res <- CalculateQuantities(u5temp.tj = q4.tj,
                             iso = iso.select,
                             indicator.type = "q4", # not used
                             hiv = FALSE, hiv.file = "", crisisadj = FALSE, adj.file = "",
                             year.t = year.t.final, year.i = year.t.final,
                             estyear.min = min(year.t.final),
                             year1 = year1, year2 = year2, year3 = year3, 
                             year4 = year4,
                             percentiles = percentiles)
  # store results
  res.cqt.Lw[[paste(weights.alpha[w])]][1, , ] <- res$res.qt
  iso.c <- iso.select
  save(iso.c, file = file.path(output.dir, "iso.c.rda"))
  year.t <- year.t.final
  save(year.t, file = file.path(output.dir, "year.t.rda"))
  save(res.cqt.Lw, file = file.path(output.dir, paste0("res.cqt.Lw.rda")))
  eval(parse(text = paste0("q4new", w, ".ctj <- q4.tj")))
  eval(parse(text = paste0("save(q4new", w, ".ctj, file = file.path(output.dir, \"q4new", 
                           w, ".ctj.rda", "\"))")))
  # use default name u5new
  eval(parse(text = paste0("u5new", w, ".ctj <- q4.tj")))
  eval(parse(text = paste0("save(u5new", w, ".ctj, file = file.path(output.dir, \"u5new", 
                           w, ".ctj.rda", "\"))")))
  cat(paste0("q4 results obtained for ", iso.select, " and saved in ", output.dir, "\n"))
}
