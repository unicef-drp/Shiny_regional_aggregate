read_wpp_and_ihme<-function(data,wpp=NULL,ihme=NULL,year.end,countrylist=countrylist){
  #WPP part
  uncode=data.frame(data$uncode.c,data$iso.c)
  if(!is.null(wpp)){
  wppmerged=merge(uncode,wpp,by.x="data.uncode.c",by.y="LocID",all.x=TRUE)
  wppmerged=subset(wppmerged,MidPeriod<=year.end)
  wpp.cqt=array(dim=c(length(unique(wppmerged$data.iso.c)),1,length(seq(min(wppmerged$MidPeriod),year.end,5))),dimnames = list(unique(data$iso.c),c(0.5),seq(min(wppmerged$MidPeriod),year.end,5)))
  #array(dim=c(length(unique(wppmerged$mcmc.meta.data.all.uncode.c)),1,length(yearspan)),dimnames = list(mcmc.meta$data.all$name.c,c("medium"),yearspan))
  for(c in 1:length(unique(wppmerged$data.uncode.c))){
    wppselected=subset(wppmerged[order(wppmerged$MidPeriod),], data.iso.c==unique(data$iso.c)[c])
    n.list=length(wppselected$MidPeriod)
    yearlocation=which(wppselected$MidPeriod %in% seq(min(wppmerged$MidPeriod),year.end,5))
    for(i in 1:length(yearlocation)){
      wpp.cqt[c,1,yearlocation[i]]=wppselected$Q5[i]
    }
  }
  } else {
    wpp.cqt=NULL
  }
  ####ihme part
  if(!is.null(ihme)){
    ihme=as.data.frame(apply(ihme,2,function(x) gsub("·",".",x)))
    ihme=as.data.frame(apply(ihme,2,function(x) gsub("[()]","",x)))
    ####separate estimates column
    ihme=separate(ihme,"1970",into=c("1970","1970ci"),sep="\r\n")
    ihme=separate(ihme,"1975",into=c("1975","1975ci"),sep="\r\n")
    ihme=separate(ihme,"1980",into=c("1980","1980ci"),sep="\r\n")
    ihme=separate(ihme,"1985",into=c("1985","1985ci"),sep="\r\n")
    ihme=separate(ihme,"1990",into=c("1990","1990ci"),sep="\r\n")
    ihme=separate(ihme,"1995",into=c("1995","1995ci"),sep="\r\n")
    ihme=separate(ihme,"2000",into=c("2000","2000ci"),sep="\r\n")
    ihme=separate(ihme,"2005",into=c("2005","2005ci"),sep="\r\n")
    ihme=separate(ihme,"2010",into=c("2010","2010ci"),sep="\r\n")
    ihme=separate(ihme,"2016",into=c("2016","2016ci"),sep="\r\n")
    
    ihme=separate(ihme,"1970ci",into=c("1970lower","1970upper"),sep=" to ")
    ihme=separate(ihme,"1975ci",into=c("1975lower","1975upper"),sep=" to ")
    ihme=separate(ihme,"1980ci",into=c("1980lower","1980upper"),sep=" to ")
    ihme=separate(ihme,"1985ci",into=c("1985lower","1985upper"),sep=" to ")
    ihme=separate(ihme,"1990ci",into=c("1990lower","1990upper"),sep=" to ")
    ihme=separate(ihme,"1995ci",into=c("1995lower","1995upper"),sep=" to ")
    ihme=separate(ihme,"2000ci",into=c("2000lower","2000upper"),sep=" to ")
    ihme=separate(ihme,"2005ci",into=c("2005lower","2005upper"),sep=" to ")
    ihme=separate(ihme,"2010ci",into=c("2010lower","2010upper"),sep=" to ")
    ihme=separate(ihme,"2016ci",into=c("2016lower","2016upper"),sep=" to ")
    
    ihme$country=as.character(ihme$country)
    ihme[,2:ncol(ihme)]=as.data.frame(apply(ihme[,2:ncol(ihme)],2,function(x) as.numeric(x)))
    ###change some countries name
    ihme$country=gsub(" and "," & ",ihme$country)
    ihme$country[which(ihme==c("The Bahamas"))]=c("Bahamas")
    ihme$country[which(ihme==c("Democratic Republic of the Congo"))]=c("Congo DR")
    ihme$country[which(ihme==c("Cote d'Ivoire"))]=c("Cote d Ivoire")
    ihme$country[which(ihme==c("The Gambia"))]=c("Gambia The")
    ihme$country[which(ihme==c("North Korea"))]=c("Korea DPR")
    ihme$country[which(ihme==c("South Korea"))]=c("Korea Rep")
    ihme$country[which(ihme==c("Laos"))]=c("Lao PDR")
    ihme$country[which(ihme==c("Russia"))]=c("Russian Federation")
    ihme$country[which(ihme==c("Saint Vincent & the Grenadines"))]=c("St Vincent & the Grenadines")
    ihme$country[which(ihme==c("Palestine"))]=c("State of Palestine")
    ihme$country[which(ihme==c("Swaziland"))]=c("Eswatini")
    ihme$country[which(ihme==c("Timor-Leste"))]=c("Timor Leste")
    ihme$country[which(ihme==c("United States"))]=c("United States of America")
    ihmefinal=merge(ihme,countrylist,by="country",all.y=TRUE)
    ihmefinal=melt(ihmefinal,id=c("country","iso"))
    ihmefinal=subset(ihmefinal,iso!="LIE")
    ihme.cqt=array(dim=c(195,3,10),dimnames = list(unique(ihmefinal$iso),c(0.05,0.5,0.95),c(seq(1970.5,2010.5,5),2016.5)))     ####create array of data to save estimates
    for(c in 1:195){
      iso.selected=as.character(data$iso.c[c])
      ihme.selected=ihmefinal[ihmefinal$iso==iso.selected,]
      ihme.cqt[c,1,]=subset(ihme.selected,grepl("lower",variable)==TRUE)$value
      ihme.cqt[c,2,]=subset(ihme.selected,grepl("upper",variable)==FALSE & grepl("lower",variable)==FALSE)$value
      ihme.cqt[c,3,]=subset(ihme.selected,grepl("upper",variable)==TRUE)$value
    }
  } else {
    ihme.cqt=NULL
  }
  return(list(wpp.cqt=wpp.cqt,ihme.cqt=ihme.cqt))
}