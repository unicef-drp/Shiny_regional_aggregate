read_wpp_and_completeihme<-function(data,wpp=NULL,completeihme=NULL,year.end,countrylist=countrylist){
  #WPP part
  uncode=data.frame(data$uncode.c,data$iso.c)
  if(!is.null(wpp)){
    wppmerged=merge(uncode,wpp,by.x="data.uncode.c",by.y="LocID",all.x=TRUE)
    wppmerged=subset(wppmerged,MidPeriod<=year.end+0.5)
    wpp.cqt=array(dim=c(length(unique(wppmerged$data.iso.c)),1,length(seq(min(wppmerged$MidPeriod),year.end+0.5,5))),dimnames = list(unique(data$iso.c),c(0.5),seq(min(wppmerged$MidPeriod),year.end+0.5,5)))
    #array(dim=c(length(unique(wppmerged$mcmc.meta.data.all.uncode.c)),1,length(yearspan)),dimnames = list(mcmc.meta$data.all$name.c,c("medium"),yearspan))
    for(c in 1:length(unique(wppmerged$data.uncode.c))){
      wppselected=subset(wppmerged[order(wppmerged$MidPeriod),], data.iso.c==unique(data$iso.c)[c])
      n.list=length(wppselected$MidPeriod)
      yearlocation=which(wppselected$MidPeriod %in% seq(min(wppmerged$MidPeriod),year.end+0.5,5))
      for(i in 1:length(yearlocation)){
        wpp.cqt[c,1,yearlocation[i]]=wppselected$Q5[i]
      }
    }
  } else {
    wpp.cqt=NULL
  }
  ####ihme estimates part
  if(!is.null(completeihme)){
    ihme=as.data.frame(completeihme)
    ihme$country=as.character(ihme$location)
    ihme$mean=ihme$mean*1000
    ihme$lower=ihme$lower*1000
    ihme$upper=ihme$upper*1000
    minyear=min(as.numeric(ihme$year))+0.5
    maxyear=max(as.numeric(ihme$year))+0.5
    #ihme[,2:ncol(ihme)]=as.data.frame(apply(ihme[,2:ncol(ihme)],2,function(x) as.numeric(x)))
    ihme$country=gsub(" and "," & ",ihme$country)
    ###change some countries name
    ihme$country[which(ihme$country==c("The Bahamas"))]=c("Bahamas")
    ihme$country[which(ihme$country==c("Democratic Republic of the Congo"))]=c("Congo DR")
    ihme$country[which(ihme$country==c("Cote d'Ivoire"))]=c("Cote d Ivoire")
    ihme$country[which(ihme$country==c("The Gambia"))]=c("Gambia The")
    ihme$country[which(ihme$country==c("North Korea"))]=c("Korea DPR")
    ihme$country[which(ihme$country==c("South Korea"))]=c("Korea Rep")
    ihme$country[which(ihme$country==c("Laos"))]=c("Lao PDR")
    ihme$country[which(ihme$country==c("Russia"))]=c("Russian Federation")
    ihme$country[which(ihme$country==c("Saint Vincent & the Grenadines"))]=c("St Vincent & the Grenadines")
    ihme$country[which(ihme$country==c("Palestine"))]=c("State of Palestine")
    ihme$country[which(ihme$country==c("Timor-Leste"))]=c("Timor Leste")
    ihme$country[which(ihme$country==c("United States"))]=c("United States of America")
    ihmefinal=merge(ihme,countrylist,by="country",all.y=TRUE)
    #ihmefinal=melt(ihmefinal,id=c("country","location_id"))
    ihmefinal=subset(ihmefinal,iso!="LIE")
    ihmefinal[ihmefinal$country=="Swaziland",]$country="Eswatini"
    yearspan=maxyear-minyear+1
    ihme.cqt=array(dim=c(195,3,yearspan),dimnames = list(unique(ihmefinal$iso),c(0.05,0.5,0.95),c(seq(minyear,maxyear,1))))    ####create array of data to save estimates
    for(c in 1:195){
      iso.selected=as.character(data$iso.c[c])
      ihme.selected=ihmefinal[ihmefinal$iso==iso.selected,]
      ihme.selected=ihme.selected[order(ihme.selected$year),]     #####order data by year
      ihme.cqt[c,1,]=ihme.selected$lower
      ihme.cqt[c,2,]=ihme.selected$mean
      ihme.cqt[c,3,]=ihme.selected$upper
    }
  } else {
    ihme.cqt=NULL
  }
  return(list(wpp.cqt=wpp.cqt,ihme.cqt=ihme.cqt))
}