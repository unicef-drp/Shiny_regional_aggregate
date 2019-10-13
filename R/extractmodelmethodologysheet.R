#----------------------------------------------------------------------
# extractmodelmethodologysheet.R
# Yao Chen, 2014
#----------------------------------------------------------------------
ExtractModelMethodologySheet <- function(
  database.U5MR,
  database.IMR,
  estimation.info.file.U5MR = "country.estimate.info.csv",
  estimation.info.file.IMR = "country.estimate.info_IMR.csv",
  model.info.file = "infoUNinclHIV.csv",
  hiv.file.U5MR = "input/dataUNAIDS_U5MR.csv",
  hiv.file.IMR = "input/dataUNAIDS_IMR.csv",
  adj.file.U5MR = "input/dataPostAdj_U5MR.csv",
  adj.file.IMR = "input/dataPostAdj_IMR.csv",
  output.methodology.sheet.file = "output/Methodology sheet.csv",
  country.order.file = "input/MethodologyCountryOrder.csv" ##<< First two columns of last year's methodology sheet, to preserve order
) {
  database <- read.csv(database.U5MR, stringsAsFactors = FALSE, encoding = "latin1")
  
  if (is.null(country.order.file)) {
    final <- unique(database[,c("Country.Name", "Country.Code")])
    names(final) <- c("Country.Name", "ISO.Code")
  } else {
    final <- read.csv(country.order.file, stringsAsFactors = FALSE)
  }
  
  model.info <- read.csv(model.info.file, stringsAsFactors = FALSE)
  model.info <- model.info[model.info$iso.c != "LIE", ]
  model.info$imrmethod.c <- gsub("Sahel", "Sahel equation", model.info$imrmethod.c)
  model.info <- model.info[, c("iso.c", "method.c", "imrmethod.c")]
  colnames(model.info) <- c("ISO.Code", "ModelUsedU5MR", "ModelUsedIMR")

  info <- read.csv(estimation.info.file.U5MR, stringsAsFactors = FALSE)
  info$Global.smoothing.U5MR <- with(info, Rule.2 | Rule.3 | Rule.4)
  info <- merge(model.info, info, sort = FALSE)
  info <- info[, !(names(info) %in% c("Country.Name", names(info)[grepl("Rule", names(info))]))]
  names(info)[names(info) == "Series.With.Fixed.Level.Bias"] <- "Series.With.Fixed.Level.Bias.U5MR"
  
  info.IMR <- read.csv(estimation.info.file.IMR, stringsAsFactors = FALSE)
  info.IMR$Global.smoothing.IMR <- with(info.IMR, Rule.2 | Rule.3 | Rule.4)
  info.IMR <- info.IMR[, c("ISO.Code", "Series.With.Fixed.Level.Bias", "Global.smoothing.IMR")]
  names(info.IMR)[names(info.IMR) == "Series.With.Fixed.Level.Bias"] <- "Series.With.Fixed.Level.Bias.IMR"
  info <- merge(info, info.IMR, sort = FALSE)
  
  final <- merge(final, model.info, sort = FALSE)
  final <- merge(final, info, sort = FALSE)
  
  adjust <- data.frame(Country.Name = unique(database$Country.Name))
  
  for (indicator in c("U5MR", "IMR")) {
    final[[paste0("Global.smoothing.", indicator)]] <- ifelse(final[[paste0("ModelUsed", indicator)]] == "B3",
                                                          ifelse(final[[paste0("Global.smoothing.", indicator)]],
                                                                 "Yes", "No"), NA)  
    final[[paste0("Series.With.Fixed.Level.Bias.", indicator)]] <- ifelse(
      is.na(final[[paste0("Series.With.Fixed.Level.Bias.", indicator)]]) | 
        final[[paste0("ModelUsed", indicator)]] != "B3", 
      "", final[[paste0("Series.With.Fixed.Level.Bias.", indicator)]])
    
    if (indicator == "IMR")
      database <- read.csv(database.IMR, stringsAsFactors = FALSE, encoding = "latin1")
    database <- database[database$Inclusion == 1, ]
    
    # VR completeness assumptions
    vr.bias <- database[database$Series.Category == "VR" & !is.na(database$Has.Bias), ]
    if (dim(vr.bias)[1] > 0) {
      vr.bias.info <- ddply(vr.bias, "Country.Name", summarise,
                            vr.bias.start.year = min(Reference.Date),
                            vr.bias.end.year = max(Reference.Date))
      vr.bias.info$VR.bias <- paste0("Has VR bias in period ", floor(vr.bias.info$vr.bias.start.year),
                                     "-", floor(vr.bias.info$vr.bias.end.year))
      adjust <- merge(adjust, vr.bias.info[, c("Country.Name", "VR.bias")], all.x = T, sort = F)
    } else {
      adjust$VR.bias <- NA
    }
    
    vr.incomplete <- database[database$Series.Category == "VR" 
                              & !is.na(database$Set.As.Minimum)
                              & is.na(database$Minimum.Level.of.Completeness)
                              & is.na(database$Maximum.Level.of.Completeness), ]
    if (dim(vr.incomplete)[1] > 0) {
      vr.incomplete.info <- ddply(vr.incomplete, "Country.Name", summarise,
                                  vr.incomplete.start.year = min(Reference.Date),
                                  vr.incomplete.end.year = max(Reference.Date))
      vr.incomplete.info$VR.incomplete <- paste0("VR from ",
                                                 floor(vr.incomplete.info$vr.incomplete.start.year),
                                                 " assumed to be incomplete")
      adjust <- merge(adjust, vr.incomplete.info[c("Country.Name","VR.incomplete")], all.x = T, sort = F)
    } else {
      adjust$VR.incomplete <- NA
    }
    
    vr.min <- database[database$Series.Category == "VR" & !is.na(database$Minimum.Level.of.Completeness), ]
    if (dim(vr.min)[1] > 0) {
      vr.min.info <- ddply(vr.min, c("Country.Name","Minimum.Level.of.Completeness"), summarise,
                           vr.bias.start.year = min(Reference.Date),
                           vr.bias.end.year = max(Reference.Date))
      vr.min.info$VR.min <- paste0("Minimum ",
                                   round(vr.min.info$Minimum.Level.of.Completeness*100, 0),
                                   "% completeness assumed for VR from ", floor(vr.min.info$vr.bias.start.year))
      adjust <- merge(adjust, vr.min.info[c("Country.Name","VR.min")], all.x = T, sort = F)
    } else {
      adjust$VR.min <- NA
    }
    
    vr.max <- database[database$Series.Category == "VR" & !is.na(database$Maximum.Level.of.Completeness), ]
    if (dim(vr.max)[1] > 0) {
      vr.max.info <- ddply(vr.max, c("Country.Name", "Maximum.Level.of.Completeness"), summarise,
                           vr.bias.start.year = min(Reference.Date),
                           vr.bias.end.year = max(Reference.Date))
      vr.max.info$VR.max <- paste0("Maximum ",
                                   round(vr.max.info$Maximum.Level.of.Completeness*100, 0),
                                   "% completeness assumed for VR from ", floor(vr.max.info$vr.bias.start.year))
      adjust <- merge(adjust, vr.max.info[c("Country.Name","VR.max")], all.x = T, sort = F)
    } else {
      adjust$VR.max <- NA
    }
    
    # HIV adjustments
    hiv <- read.csv(get(paste0("hiv.file.", indicator)), stringsAsFactors = FALSE)
    hiv.info <- unique(hiv["country.hiv"])
    names(hiv.info)[1] <- "Country.Name"
    hiv.info$HIV <- "HIV adjustment"
    
    # Crisis adjustments
    crisis <- read.csv(get(paste0("adj.file.", indicator)), stringsAsFactors = FALSE)
    crisis.info <- ddply(crisis, c("country.adj"), summarise,
                         start.year = min(year.adj),
                         end.year = max(year.adj))
    names(crisis.info)[1] <- "Country.Name"
    crisis.info$Crisis <- with(crisis.info,
                               ifelse(start.year == end.year,
                                      paste0("Crisis adjustment in ", start.year),
                                      paste0("Crisis adjustment in ", start.year,
                                             "-", end.year)))
    
    # Merge
    adjust <- merge(adjust, hiv.info[, c("Country.Name", "HIV")], all.x = T, sort = F)
    adjust <- merge(adjust, crisis.info[, c("Country.Name", "Crisis")], all.x = T, sort = F)
    adjust[[paste0(indicator," adjustment")]] <- with(adjust, paste(VR.bias, VR.incomplete,
                                                                    VR.min, VR.max, 
                                                                    HIV, Crisis,
                                                                    sep="; "))
    adjust[[paste0(indicator," adjustment")]] <- gsub("NA; |; NA", "", 
                                                      adjust[[paste0(indicator," adjustment")]])
    adjust[[paste0(indicator," adjustment")]] <- ifelse(adjust[[paste0(indicator," adjustment")]] == "NA",
                                                        NA, adjust[[paste0(indicator," adjustment")]])
  }
  
  adjust <- adjust[, c("Country.Name", "U5MR adjustment", "IMR adjustment")]
  final <- merge(final, adjust, all.x = T, sort = F)
  final <- final[, c("Country.Name", "ISO.Code",
                     "ModelUsedU5MR", "Global.smoothing.U5MR",
                     "Series.With.Fixed.Level.Bias.U5MR", "U5MR adjustment",
                     "ModelUsedIMR", "Global.smoothing.IMR",
                     "Series.With.Fixed.Level.Bias.IMR", "IMR adjustment")]
  write.csv(final, output.methodology.sheet.file, row.names = F, na = "-", fileEncoding = "latin1")
}
