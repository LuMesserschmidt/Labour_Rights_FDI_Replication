# Countries in data sets for economy, politics and IR are often spelled differently. This code helps to map different spellings onto an abbreviation to provide for standardization; here I use the World Bank Codes for each country. This later helps to merge more data files into one large mastertable.

# Additional information:

# You could also use the package {countrycode} see URL: Standardize country names, convert them into one of eleven coding schemes, convert between coding schemes, and assign region descriptors.

# It may be interesting for you that a similar kind of code to map countries is provided by Ben Graham, URL: https://dornsife.usc.edu/assets/sites/298/docs/country_to_gwno_2013_07_caps.txt (accessed April 8 2015). Graham maps country names onto Gleditsch-Ward country identifiers.


cat("Creating mapping list ...")
mappingList <- list(
AFG = c("Afghanistan"),
ALB = c("Albania"),
DZA = c("Algeria","algeria"),
ASM = c("American Samoa"),
ADO = c("Andorra"),
AGO = c("Angola"),
ATG = c("Antigua and Barbuda"),
ARG = c("Argentina"),
ARM = c("Armenia"),
ABW = c("Aruba"),
AUS = c("Australia","australia"),
AUT = c("Austria","austria"),
AZE = c("Azerbaijan"),
BHS = c("Bahamas", "Bahamas, The"),
BHR = c("Bahrain"),
BGD= c("Bangladesh"),
BRB = c("Barbados"),
BLR = c("Belarus"),
BEL = c("Belgium"),
BLZ = c("Belize"),
BEN = c("Benin"),
BMU = c("Bermuda"),
BTN = c("Bhutan"),
BOL = c("Bolivia", "Bolivia (Plurinational State of)"),
BIH = c("Bosnia and Herzegovina", "Bosnia-Herzegovina", "Bosnia Herzegovenia"), #the third from CIRI
BWA = c("Botswana"),
BRA = c("Brazil"),
BRN = c("Brunei Darussalam", "Brunei"),
BGR = c("Bulgaria"),
BFA = c("Burkina Faso", "Burkina"),
BDI = c("Burundi"),
KHM = c("Cambodia"),
CMR = c("Cameroon"),
CAN = c("Canada"),
CPV = c("Cape Verde"),
CYM = c("Cayman Islands"),
CAF = c("Central African Republic"),
TCD = c("Chad"),
CHI = c("Channel Islands"),
CHL = c("Chile"),
CHN = c("China"),
COL = c("Colombia"),
COM = c("Comoros"),
ZAR = c("Congo, Dem. Rep.","Democratic Republic of the Congo (Zaire)","Congo, Democratic Republic of", "Zaire", "Congo (Kinshasa)", "Congo Kinshasa", "Democratic Republic of the Congo", "Dem. Rep. of the Congo","Democratic Republic of Congo"),
COG = c("Congo, Rep.", "Congo, Republic of", "Congo (Brazzaville)", "Congo", "Congo Brazzaville"),
CRI = c("Costa Rica"),
CIV = c("Côte d'Ivoire","Cote d'Ivoire", "Ivory Coast", "Cote D'Ivoire", "Ivory Coast (Cote d'Ivoire)"),#weird numbers from UNCTAD
HRV = c("Croatia"),
CUB = c("Cuba"),
CUW = c("Curaçao", "Curacao"),
CYP = c("Cyprus"),
CZE = c("Czech Republic", "Czechoslovakia", "Czechoslovakia (former)"), #Czechoslovakia (former) because of UN FDI
DNK = c("Denmark"),
DJI = c("Djibouti"),
DMA = c("Dominica"),
DOM = c("Dominican Republic", "Dominican Rep"),
ECU = c("Ecuador"),
EGY = c("Egypt, Arab Rep.", "Egypt"),
SLV = c("El Salvador"),
GNQ = c("Equatorial Guinea"),
ERI = c("Eritrea"),
EST = c("Estonia"),
ETH = c("Ethiopia", "Ethiopia (former)","Ethiopia (-1991)"), #Ethiopia numbers UNCTAD
FRO = c("Faeroe Islands"),
FJI = c("Fiji"),
FIN = c("Finland"),
FRA = c("France"),
PYF = c("French Polynesia"),
GAB = c("Gabon"),
GMB = c("Gambia, The", "Gambia"),
GEO = c("Georgia"),
DEU = c("Germany","Germany, Federal Republic of", "Germany West", "Germany (former Federal Rep.)"), #Germany West because of polity Germany (former Federal Rep.) bec.of UN FDI
GHA = c("Ghana"),
GIB = c("Gibraltar"),
GRC = c("Greece"),
GRL = c("Greenland"),
GRD = c("Grenada"),
GUM = c("Guam"),
GTM = c("Guatemala"),
GIN = c("Guinea"),
GNB = c("Guinea-Bissau"),
GUY = c("Guyana"),
HTI = c("Haiti"),
HND = c("Honduras"),
HKG = c("Hong Kong SAR, China", "Hong Kong", "Hong Kong, China","China, Hong Kong SAR"),
HUN = c("Hungary"),
ISL = c("Iceland"),
IND = c("India"),
IDN = c("Indonesia","Indonesia including East Timor", "Indonesia (-2002)"), #Indonesia including East Timor bec. of UN FDI
IRN = c("Iran, Islamic Rep.", "Iran", "Iran (Islamic Republic of)"),
IRQ = c("Iraq"),
IRL = c("Ireland"),
IMY = c("Isle of Man"),
ISR = c("Israel", "Israel and Occupied Territories**"), #Israel and Occupied Territories** from PTS
ITA = c("Italy"),
JAM = c("Jamaica"),
JPN = c("Japan"),
JOR = c("Jordan"),
KAZ = c("Kazakhstan"),
KEN = c("Kenya"),
KIR = c("Kiribati"),
PRK = c("Korea, Dem. Rep.", "North Korea", "Korea, Democratic People's Republic of", "Korea North", "North Korea (Democratc People's Republic of Korea)","Korea, Dem. People's Rep. of"),
KOR = c("Korea, Rep.", "South Korea", "Korea, Republic of", "Korea South", "South Korea (Republic of Korea)", "Korea"), #Korea added because of OECD
KSV = c("Kosovo"),
KWT = c("Kuwait"),
KGZ = c("Kyrgyz Republic", "Kyrgyzstan"),
LAO = c("Lao PDR", "Laos", "Lao People's Dem. Rep."),
LVA = c("Latvia"),
LBN = c("Lebanon"),
LSO = c("Lesotho"),
LBR = c("Liberia"),
LBY = c("Libya", "Libyan Arab Jamahiriya"),
LIE = c("Liechtenstein"),
LTU = c("Lithuania"),
LUX = c("Luxembourg"),
MAC = c("Macao SAR, China", "Macao, China", "China, Macao SAR"),
MKD = c("Macedonia, FYR", "Macedonia", "TFYR of Macedonia"),
MDG = c("Madagascar"),
MWI = c("Malawi"),
MYS = c("Malaysia"),
MDV = c("Maldives"),
MLI = c("Mali"),
MLT = c("Malta"),
MHL = c("Marshall Islands"),
MRT = c("Mauritania"),
MUS = c("Mauritius"),
MYT = c("Mayotte"),
MEX = c("Mexico"),
FSM = c("Micronesia, Fed. Sts.","Micronesia, Federated States of", "Federated States of Micronesia", "Micronesia","Micronesia (Federated States of)"),
MDA = c("Moldova", "Republic of Moldova"),
MCO = c("Monaco"),
MNG = c("Mongolia"),
MNE = c("Montenegro"),
MAR = c("Morocco"),
MOZ = c("Mozambique"),
MMR = c("Myanmar", "Burma", "Republic of the Union of Myanmar", "Myanmar (Burma)"),
NAM = c("Namibia"),
NPL = c("Nepal"),
NLD = c("Netherlands"),
NCL = c("New Caledonia"),
NZL = c("New Zealand"),
NIC = c("Nicaragua"),
NER = c("Niger"),
NGA = c("Nigeria"),
MNP = c("Northern Mariana Islands"),
NOR = c("Norway"),
OMN = c("Oman"),
PAK = c("Pakistan"),
PLW = c("Palau"),
PAN = c("Panama","panama"),
PNG = c("Papua New Guinea"),
PRY = c("Paraguay"),
PER = c("Peru"),
PHL = c("Philippines"),
POL = c("Poland"),
PRT = c("Portugal"),
PRI = c("Puerto Rico"),
QAT = c("Qatar"),
ROM = c("Romania", "Rumania"),
RUS = c("Russian Federation", "Russia", "Russia (Soviet Union)","Union of Soviet Socialist Republics"), # deleted USSR because of PTS (duplicated with RUS) #Careful, "Soviet Union" in CIRI was not just another name for RUSSIA but other country altogether. Russia(n) Federation exists since 1991. See also duplicated() 
RWA = c("Rwanda"),
WSM = c("Samoa", "Western Samoa"),
SMR = c("San Marino"),
STP = c("São Tomé and Principe", "Sao Tome and Principe"),
SAU = c("Saudi Arabia"),
SEN = c("Senegal"),
SRB = c("Serbia"),
SYC = c("Seychelles"),
SLE = c("Sierra Leone"),
SGP = c("Singapore"),
SXM = c("Sint Maarten (Dutch part)"),
SVK = c("Slovak Republic", "Slovakia"),
SVN = c("Slovenia"),
SLB = c("Solomon Islands"),
SOM = c("Somalia"),
ZAF = c("South Africa"),
ESP = c("Spain"),
LKA = c("Sri Lanka"),
KNA = c("St. Kitts and Nevis","Saint Kitts and Nevis"),
LCA = c("St. Lucia", "Saint Lucia"),
MAF = c("St. Martin (French part)"),
VCT = c("St. Vincent and the Grenadines", "Saint Vincent and the Grenadines", "St. Vincent"),
SDN = c("Sudan","Sudan (-2011)"),
SUR = c("Suriname"),
SWZ = c("Swaziland"),
SWE = c("Sweden"),
CHE = c("Switzerland"),
SYR = c("Syrian Arab Republic", "Syria"),
TJK = c("Tajikistan"),
TZA = c("Tanzania","United Republic of Tanzania"),
THA = c("Thailand"),
TMP = c("Timor-Leste", "Democratic Republic of Timor-Leste ", "East Timor", "East Timor (Timor L'este)"),
TGO  = c("Togo"),
TON = c("Tonga"),
TTO = c("Trinidad and Tobago", "Trinidad"), #Trinidad added because of Polity data
TUN = c("Tunisia"),
TUR = c("Turkey"),
TKM = c("Turkmenistan"),
TCA = c("Turks and Caicos Islands"),
TUV = c("Tuvalu"),
UGA = c("Uganda"),
UKR = c("Ukraine"),
ARE = c("United Arab Emirates", "UAE"), #UAE because of Polity 
GBR = c("United Kingdom","UK"),
USA  = c("United States", "United States of America","USA"),
URY  = c("Uruguay"),
UZB  = c("Uzbekistan"),
VUT  = c("Vanuatu"),
VEN  = c("Venezuela, RB", "Venezuela", "Venezuela (Bolivarian Republic of)"),
VNM = c("Vietnam", "Vietnam, Socialist Republic of", "Viet Nam"),
VIR = c("Virgin Islands (U.S.)"),
WBG = c("West Bank and Gaza"),
YEM = c("Yemen, Rep.", "Yemen", "Yemen North","Yemen, North","Yemen Arab Republic","North Yemen","Yemen, Arab Republic"), #North Yemen and Yemen should be the time series; South Yemen should be discarded or seen as sep. country (see Mosley email) #decision Oct 27, 2013 (before not consistent in this)
ZMB = c("Zambia"),
ZWE = c("Zimbabwe")
) ##End of mappingList definition
mappingVector <- rep(names(mappingList),sapply(mappingList,length)) 
names(mappingVector) <- unlist(mappingList)
#the last two lines make sure when I type mappingList that I can see the world bank code when I look for a country, and when I type mappingVector that I can see the country when I have the code only

#How to map a countryname from world bank code, take the first countryname in the table x[1]
#wb2country <- sapply(mappingList,function(x)x[1])
#cat(" done\n")

#Correlation matrix with Significance levels
#From: Thilo Klein Lab08 and R help archive; can adjust the ***
#for changes read Hmisc rcorr function again!
corstars <- function(x){
  x <- as.matrix(x)
  R <- rcorr(x, type="spearman")$r   #leave out type="spearman" if you want to use pearson
  p <- rcorr(x)$P
  n <- rcorr(x)$n
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "*  ", "   ")))
  R <- format(round(R, 3))
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) #add n between mystars and sep, problems with layout though
  ## latex variante (geht nicht)
  #Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  #Rnew <- matrix(paste(Rnew, n, sep="\\n="), ncol=ncol(x))
  
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- colnames(x)
  Rnew[upper.tri(Rnew,diag=TRUE)] <- ""
  Rnew <- data.frame(Rnew)[-1,-length(colnames(x))]
  return(Rnew)
}


#Showing missingness as data frame
#Source of function: http://gettinggeneticsdone.blogspot.co.uk/2011/02/summarize-missing-data-for-all.html
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss=sum(is.na(x)),
      n=length(x),
      propmiss=sum(is.na(x))/length(x)
      )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}


# my.pipeline <- function(dv,iv,dat=D.pd,verbose=FALSE){
#   require(plm)
#   require(lmtest)
#   form <- as.formula(paste(Mos_labor.lag  ~  Mos_labor + log(US_FDIpGDP) + log(trade) + logGDP_cap_curr + logpopulation + polity_bin + confl)
#   res.po <- plm(form,model="pooling",data=dat)
#   if (verbose) summary(res.po)
#   res.BKgroup <- my.vcovBK(x=res.po,cluster="group")
#   ct.group    <- coeftest(res.po,vcov=res.BKgroup)
#   
#   if (verbose) print(ct.group)
#   plot.estim(ct.group,main=paste(dv,"+",iv),filename=NULL)
#   
#   ressum <- summary(res.po)
#   
#   result <- list(betas        = ct.group[,1],
#                  stderr       = ct.group[,2],
#                  significance = symnum(ct.group[,4],cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", " **", "  *", " . ", "   ")),              
#                  Rsquared     = ressum$r.squared,
#                  nT           = pdim(res.po)$nT
#                  )
#   
#   final <- c(paste(round(result$betas,3),result$significance," (",round(result$stderr,3),")",sep=""),
#              round(result$Rsquared[1],2),
#              unlist(result$nT[c(1,3)])
#              )
#   names(final) <- c(rownames(ct.group),"Rsq","countries","N")
#   result$final <- final
#   
#   tab_oben  <- cbind(round(result$betas,3),result$significance,paste("(",round(result$stderr,3),")",sep=""))
#   tab_unten <- cbind(c(round(result$Rsquared[1],2),unlist(result$nT[c(1,3)])),matrix(NA,ncol=2,nrow=3)) 
#   result$threeColumns <- rbind(tab_oben,tab_unten)
#   
#   return(result)
# }

my.vcovBK <- function (x, type = c("HC0", "HC1", "HC2", "HC3", "HC4"), cluster = c("group", "time"), diagonal = FALSE, ...) 
{
  type <- match.arg(type)
  model <- plm:::describe(x, "model")
  if (!model %in% c("random", "within", "pooling", "fd")) {
    stop("Model has to be either random, within, pooling or fd model")
  }
  demX <- model.matrix(x, model = model)
  demy <- pmodel.response(x, model = model)
  dimnames(demX)[[2]][1] <- attr(vcov(x), "dimnames")[[1]][1]
  pdim <- pdim(x)
  nT <- pdim$nT$N
  Ti <- pdim$Tint$Ti
  k <- dim(demX)[[2]]
  n0 <- pdim$nT$n
  t0 <- pdim$nT$T
  uhat <- x$residuals
  groupind <- as.numeric(attr(x$model, "index")[, 1])
  timeind <- as.numeric(attr(x$model, "index")[, 2])
  if (model == "fd") {
    groupind <- groupind[timeind > 1]
    timeind <- timeind[timeind > 1]
    nT <- nT - n0
    Ti <- Ti - 1
    t0 <- t0 - 1
  }
  switch(match.arg(cluster), group = {
    n <- n0
    t <- t0
    relevant.ind <- groupind
    lab <- timeind
  }, time = {
    n <- t0
    t <- n0
    relevant.ind <- timeind
    lab <- groupind
  })
  tind <- vector("list", n)
  tlab <- vector("list", n)
  for (i in 1:length(unique(relevant.ind))) {
    tind[[i]] <- which(relevant.ind == i)
    tlab[[i]] <- lab[which(relevant.ind == i)]
  }
  dhat <- function(x) {
    tx <- t(x)
    diag(crossprod(tx, solve(crossprod(x), tx)))
  }
  switch(match.arg(type), HC0 = {
    diaghat <- NULL
  }, HC1 = {
    diaghat <- NULL
  }, HC2 = {
    diaghat <- try(dhat(demX), silent = TRUE)
  }, HC3 = {
    diaghat <- try(dhat(demX), silent = TRUE)
  }, HC4 = {
    diaghat <- try(dhat(demX), silent = TRUE)
  })
  df <- nT - k
  switch(match.arg(type), HC0 = {
    omega <- function(residuals, diaghat, df) residuals
  }, HC1 = {
    omega <- function(residuals, diaghat, df) residuals * 
      sqrt(length(residuals)/df)
  }, HC2 = {
    omega <- function(residuals, diaghat, df) residuals/sqrt(1 - 
      diaghat)
  }, HC3 = {
    omega <- function(residuals, diaghat, df) residuals/(1 - 
      diaghat)
  }, HC4 = {
    omega <- function(residuals, diaghat, df) residuals/sqrt(1 - 
      diaghat)^pmin(4, length(residuals) * diaghat/as.integer(round(sum(diaghat), 
                                                                    digits = 0)))
  })
  uhat <- omega(uhat, diaghat, df)
  tres <- array(dim = c(t, t, n))
  for (i in 1:n) {
    ut <- uhat[tind[[i]]]
    tpos <- (1:t)[unique(lab) %in% tlab[[i]]]
    if (diagonal) {
      tres[tpos, tpos, i] <- diag(diag(ut %o% ut))
    }
    else {
      tres[tpos, tpos, i] <- ut %o% ut
    }
  }
  OmegaT <- apply(tres, 1:2, mean, na.rm = TRUE)
  unlabs <- unique(lab)
  salame <- array(dim = c(k, k, n))
  for (i in 1:n) {
    groupinds <- tind[[i]]
    grouplabs <- tlab[[i]]
    xi <- demX[groupinds, ,drop=FALSE]
    tpos <- unlabs %in% grouplabs
    OmegaTi <- OmegaT[tpos, tpos]
    #if ( i==15) browser()
    salame[, , i] <- crossprod(xi, OmegaTi) %*% xi
  }
  salame <- apply(salame, 1:2, sum)
  pane <- solve(crossprod(demX))
  mycov <- pane %*% salame %*% pane
  return(mycov)
}



plot.estim <- function(ct,sigcol="orange",sigthresh=0.05,lim=1, intercept=TRUE, sorted=FALSE,filename="Estimates.pdf", ...){
  ## opening the PDF if 'filename' is given
  if (!is.null(filename)) pdf(filename, width=7,height=5)
  ## throw out intercept line in 'ct' if not wanted 
  if (!intercept) ct <- ct[-grep("Intercept",rownames(ct)),]
  ## sort
  if (sorted) ct <- ct[order(ct[,1]),]
  ## define colors of bars
  colbars <- ifelse(ct[,4]<=sigthresh,sigcol,"grey")
  ## compute confidence intervals
  CI.H <- ct[,1] + 1.96*ct[,2]
  CI.L <- ct[,1] - 1.96*ct[,2]
  ## plot bars
  par(mar=c(5,7.5,3,2))
  xvals <- barplot(ct[,1],horiz=TRUE,las=1,col=colbars,xlim=c(-lim,lim), xlab="Estimate of beta",...)
  abline(v=0,lwd=2)
  ## add T-shaped arrows for confidence interval
  arrows(ct[,1], xvals, CI.H, xvals, angle=90,length=0.07) # Draw error bars
  arrows(ct[,1], xvals, CI.L, xvals, angle=90,length=0.07)  
  ## closing the PDF
  if (!is.null(filename)) cat("Plotted to",filename,"\n") else cat("Plotted to screen","\n")
  if (!is.null(filename)) dev.off()
}

flo.form <- function(y,IV,IVlags=NULL){
  if (is.null(IVlags)){
    lag.form <- as.list(c(1,rep(0,length(IV))))
  } else {
    if (length(IVlags)!=length(IV)) stop("number of lags must be number of independent variables")
    lag.form <- as.list(c(1,IVlags))
  }
  #f <- as.formula(paste(y,"~ lag(",y,",1) +",paste(IV,collapse=" + ")))
  f  <- as.formula(paste(y,"~",paste(IV,collapse=" + ")))
  fd <- dynformula(f,lag.form)
  return(fd)
}



# Lag panel data
# Chris Adolph

# Usage:
#
# laggeddata <- lagpanel(x,  # An n x k matrix of data to be lagged;
# must be in time series order and stacked by
# unit
#                        c,  # An n x 1 vector of group numbers
#                        t,  # An n x 1 vector of periods
#                        lagnum, # The desired lag to report

lagpanel <- function(x,c,t,lagnum) {
  outclass <- "matrix"
  if (any(class(x)=="data.frame")) outclass <- "data.frame"
  x <- as.matrix(x)
  outnames <- colnames(x)
  c <- as.matrix(c)
  t <- as.matrix(t)
  listc <- unique(c)
  outmat <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  runningtotal <- 0
  for (i in 1:nrow(listc)) {
    numtper <- length(unique(t[c==listc[i,]]))
    xc <- as.matrix(x[c==listc[i,],])
    if (nrow(xc)>numtper)
      stop(paste("Duplicate time periods in data for unit", listc[i]))
    if (numtper>lagnum) {
      outmat[(runningtotal+1+lagnum):(runningtotal + numtper),] <-
        xc[1:(nrow(xc)-lagnum),]
    }
    runningtotal <- runningtotal + numtper
  }
  if (outclass=="data.frame") outmat <- as.data.frame(outmat)
  colnames(outmat) <- outnames
  outmat
}


my.lag.vars <- function(x,vars,prefix="Lag_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,1) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} ##end function 


# Libraries I will need for these functions:

library(plm)
library(lmtest)
library(simcf) 
library(Hmisc)
library(gplots)
library(lattice)
library(nlme)      # Estimation of mixed effects models
library(lme4)      # Alternative package for mixed effects models
library(arm)       # Gelman & Hill code for mixed effects simulation
library(pcse)      # Calculate PCSEs for LS models (Beck & Katz)
library(tseries)   # For ADF unit root test
library(simcf)     # For panel functions and simulators

#library(devtools)
#install_github("chrisadolph/simcf") 



##------------------------------------------------------------------------------------------------------
# Transformations of imputed data same as original data (transformations set 1 - see below for another set)

my.transform1 <- function(t){
  
  # delete USA (for total FDI USA models)
  unique(t$country)
  out <- which(t$country=="United States of America")
  t.usaout <- t[-out,]
  dim(t.usaout) #3796   59
  t <- t.usaout  
  
  # Recode FDI negative values to zero (just to make sure there are no negative or zero values coming from Amelia)
  t2 <- t[,c(19:35)] #only FDI values; check with colnames(m[[1]])
  
  t3 <- as.data.frame(matrix(NA,nrow=nrow(t2),ncol=ncol(t2))) #new table to put recoded vars in
  colnames(t3) <- colnames(t2)
  for(i in 1:nrow(t2)){       
    for(j in 1:ncol(t2)){   
      if (!is.na(t2[i,j])){         #leave NAs as NAs            
        if(t2[i,j] <= 0){
          t3[i,j] <- 0              #all negative and zero values to 0              
        } # end if <= 0
        else if(t2[i,j] >0){
          t3[i,j] <- t2[i,j]        #all >0 values keep original FDI
        } # end if > 0
      } # end if !is.na
    } # end for j    
  } # end for i
  a <- colnames(t3)
  b <- paste("nonneg",a,sep="")    #new variable names
  colnames(t3) <- b                 #merge with t
  t<- cbind(t,t3)
  dim(t)# 3796   76
  
  # GDP/capita
  t$"GDPpc" <- t$GDP_curr/t$population
  
  # Add up sectors - Oct 23, 2013
  #all finance
  t$"nonnegUS_fdi_all_finance" <- t$"nonnegUS_fdi_depository"  +  t$"nonnegUS_fdi_finance_except"  
  
  #all service  
  t$"nonnegUS_fdi_service" <- t$"nonnegUS_fdi_depository"  +     t$"nonnegUS_fdi_finance_except" + t$"nonnegUS_fdi_whole_trade"                                                                                               
  
  #all low skills, high integration, and high tech
  t$"nonnegUS_fdi_food_met" <- t$"nonnegUS_fdi_food" +  t$"nonnegUS_fdi_prim_fab_metal"            
  
  # Three motivations for FDI
  #resource-seeking (with bias because NA is assumed to be 0)
  t$"nonnegUS_fdi_resource" <- t$"nonnegUS_fdi_petrol" + t$"nonnegUS_fdi_mining"
  
  #market-seeking
  t$"nonnegUS_fdi_market"   <- t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_chemical" +   t$"nonnegUS_fdi_depository"  +  t$"nonnegUS_fdi_finance_except" + t$"nonnegUS_fdi_whole_trade"
  
  #market-seeking without chemical because that has high skills and stands out
  t$"nonnegUS_fdi_market1"   <- t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" +   t$"nonnegUS_fdi_depository"  +  t$"nonnegUS_fdi_finance_except" + t$"nonnegUS_fdi_whole_trade"
  
  #market-seeking only Manufacturing (without services)
  t$"nonnegUS_fdi_market2"   <- t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_chemical"
  
  #market-seeking only Manufacturing (without services, without chemical because it stands out as high skills)
  t$"nonnegUS_fdi_market3"   <- t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal"
  
  #efficiency-seeking
  t$"nonnegUS_fdi_efficiency"     <- t$"nonnegUS_fdi_machinery" + t$"nonnegUS_fdi_electrical" + t$"nonnegUS_fdi_transport"
  
  
  # Other combinations based on skills, technology, and knowledge
  #all low skilled with petrol and mining in one 
  t$"nonnegUS_fdi_lowskill"       <- t$"nonnegUS_fdi_petrol" + t$"nonnegUS_fdi_mining" + t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal"                             
  
  #all low skilled only petrol
  t$"nonnegUS_fdi_lowskillp"      <- t$"nonnegUS_fdi_petrol" + t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal"                             
  
  #all low skilled only mining
  t$"nonnegUS_fdi_lowskillm"      <-  t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" 
  
  #all low skilled without resources 
  t$"nonnegUS_fdi_lowskill1"       <- t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal"                             
  
  
  #all high skilled             
  t$"nonnegUS_fdi_highskill"      <- t$"nonnegUS_fdi_chemical" +  t$"nonnegUS_fdi_machinery" + t$"nonnegUS_fdi_electrical" + t$"nonnegUS_fdi_transport"  +     t$"nonnegUS_fdi_depository"  +     t$"nonnegUS_fdi_finance_except" + t$"nonnegUS_fdi_whole_trade"     
  
  
  #all high tech or high knowledge
  t$"nonnegUS_fdi_hightechknow" <- t$"nonnegUS_fdi_chemical" +  t$"nonnegUS_fdi_machinery" + t$"nonnegUS_fdi_electrical" + t$"nonnegUS_fdi_transport"  +      t$"nonnegUS_fdi_depository"  +     t$"nonnegUS_fdi_finance_except"
  
  
  #all low tech or low knowledge with mining and petrol
  t$"nonnegUS_fdi_lowtechknow"  <- t$"nonnegUS_fdi_petrol" + t$"nonnegUS_fdi_mining" +  t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_whole_trade"    
  
  #all low tech or low knowledge with mining 
  t$"nonnegUS_fdi_lowtechknowm" <-  t$"nonnegUS_fdi_mining" +  t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_whole_trade" 
  
  #all low tech or low knowledge with petrol
  t$"nonnegUS_fdi_lowtechknowp" <-   t$"nonnegUS_fdi_petrol" +  t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_whole_trade" 
  
  #all low tech or low knowledge without resource sectors
  t$"nonnegUS_fdi_lowtechknow1" <-   t$"nonnegUS_fdi_food" + t$"nonnegUS_fdi_prim_fab_metal" + t$"nonnegUS_fdi_whole_trade"    
  
  
  
  # create FDI/GDP (FDI in $, GDP_curr in $)
  t$"nonnegUS_FDIpGDP"                <- (t$nonnegUS_fdi_total)/(t$GDP_curr)
  t$"nonnegUS_fdi_petrolpGDP"         <- (t$nonnegUS_fdi_petrol)/(t$GDP_curr)
  t$"nonnegUS_fdi_total_manufpGDP"    <- (t$nonnegUS_fdi_total_manuf)/(t$GDP_curr)
  t$"nonnegUS_fdi_foodpGDP"           <- (t$nonnegUS_fdi_food)/(t$GDP_curr)
  t$"nonnegUS_fdi_chemicalpGDP"       <- (t$nonnegUS_fdi_chemical)/(t$GDP_curr)
  t$"nonnegUS_fdi_prim_fab_metalpGDP" <- (t$nonnegUS_fdi_prim_fab_metal)/(t$GDP_curr)
  t$"nonnegUS_fdi_machinerypGDP"      <- (t$nonnegUS_fdi_machinery)/(t$GDP_curr)
  t$"nonnegUS_fdi_electricalpGDP"     <- (t$nonnegUS_fdi_electrical)/(t$GDP_curr)
  t$"nonnegUS_fdi_transportpGDP"      <- (t$nonnegUS_fdi_transport)/(t$GDP_curr)
  t$"nonnegUS_fdi_whole_tradepGDP"    <- (t$nonnegUS_fdi_whole_trade)/(t$GDP_curr)
  t$"nonnegUS_fdi_depositorypGDP"     <- (t$nonnegUS_fdi_depository)/(t$GDP_curr)
  t$"nonnegUS_fdi_finance_exceptpGDP" <- (t$nonnegUS_fdi_finance_except)/(t$GDP_curr)
  t$"nonnegUS_fdi_miningpGDP"         <- (t$nonnegUS_fdi_mining)/(t$GDP_curr)
  
  #new added sectors
  t$"nonnegUS_fdi_all_financepGDP"  <- (t$nonnegUS_fdi_all_finance)/(t$GDP_curr)
  t$"nonnegUS_fdi_servicepGDP"      <- (t$nonnegUS_fdi_service)/(t$GDP_curr)
  t$"nonnegUS_fdi_food_metpGDP"     <- (t$nonnegUS_fdi_food_met)/(t$GDP_curr)
  t$"nonnegUS_fdi_resourcepGDP"     <- (t$nonnegUS_fdi_resource)/(t$GDP_curr)
  t$"nonnegUS_fdi_marketpGDP"       <- (t$nonnegUS_fdi_market)/(t$GDP_curr)
  t$"nonnegUS_fdi_market1pGDP"      <- (t$nonnegUS_fdi_market1)/(t$GDP_curr)
  t$"nonnegUS_fdi_market2pGDP"      <- (t$nonnegUS_fdi_market2)/(t$GDP_curr)
  t$"nonnegUS_fdi_market3pGDP"      <- (t$nonnegUS_fdi_market3)/(t$GDP_curr)
  t$"nonnegUS_fdi_efficiencypGDP"   <- (t$nonnegUS_fdi_efficiency)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowskillpGDP"     <- (t$nonnegUS_fdi_lowskill)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowskillppGDP"    <- (t$nonnegUS_fdi_lowskillp)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowskillmpGDP"    <- (t$nonnegUS_fdi_lowskillm)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowskill1pGDP"    <- (t$nonnegUS_fdi_lowskill1)/(t$GDP_curr)
  t$"nonnegUS_fdi_highskillpGDP"    <- (t$nonnegUS_fdi_highskill)/(t$GDP_curr)
  t$"nonnegUS_fdi_hightechknowpGDP" <- (t$nonnegUS_fdi_hightechknow)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowtechknowpGDP"  <- (t$nonnegUS_fdi_lowtechknow)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowtechknowmpGDP" <- (t$nonnegUS_fdi_lowtechknowm)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowtechknowppGDP" <- (t$nonnegUS_fdi_lowtechknowp)/(t$GDP_curr)
  t$"nonnegUS_fdi_lowtechknow1pGDP" <- (t$nonnegUS_fdi_lowtechknow1)/(t$GDP_curr)
  
  # Log FDI variables with >0 = log() and 0 remains 0 (as if I had recoded to 1, which logs to zero). This is the same as using the variable 'log' from logBound()
  
  #explanation of error:
  #  Error in if (forceAny | any(!any)) { :  missing value where TRUE/FALSE needed
  #this is because there are no 0 or negative values left for many variables, except resFDI, and therefore the column ANY must be forced; it will for these variables always say 1.
  # why I still do it: because some vars have 0 values (UN FDI) and it can't hurt
  
  temp   <- logBound(t$nonnegUS_FDIpGDP,forceAny=T)
  temp1  <- logBound(t$nonnegUS_fdi_petrolpGDP,forceAny=T)
  temp2  <- logBound(t$nonnegUS_fdi_total_manufpGDP,forceAny=T)
  temp3  <- logBound(t$nonnegUS_fdi_foodpGDP,forceAny=T)
  temp4  <- logBound(t$nonnegUS_fdi_chemicalpGDP,forceAny=T)
  temp5  <- logBound(t$nonnegUS_fdi_prim_fab_metalpGDP,forceAny=T)
  temp6  <- logBound(t$nonnegUS_fdi_machinerypGDP,forceAny=T)
  temp7  <- logBound(t$nonnegUS_fdi_electricalpGDP,forceAny=T)
  temp8  <- logBound(t$nonnegUS_fdi_transportpGDP,forceAny=T)
  temp9 <- logBound(t$nonnegUS_fdi_whole_tradepGDP,forceAny=T)
  temp10 <- logBound(t$nonnegUS_fdi_depositorypGDP,forceAny=T)
  temp11 <- logBound(t$nonnegUS_fdi_finance_exceptpGDP,forceAny=T)
  temp12 <- logBound(t$nonnegUS_fdi_miningpGDP,forceAny=T)
  
  temp13 <- logBound(t$nonnegUS_fdi_all_financepGDP,forceAny=T)
  temp14 <- logBound(t$nonnegUS_fdi_servicepGDP,forceAny=T)    
  temp15 <- logBound(t$nonnegUS_fdi_food_metpGDP,forceAny=T)
  temp16 <- logBound(t$nonnegUS_fdi_resourcepGDP,forceAny=T)
  temp17 <- logBound(t$nonnegUS_fdi_marketpGDP,forceAny=T)
  temp18 <- logBound(t$nonnegUS_fdi_market1pGDP,forceAny=T)
  temp19 <- logBound(t$nonnegUS_fdi_market2pGDP,forceAny=T)     
  temp20 <- logBound(t$nonnegUS_fdi_market3pGDP,forceAny=T)     
  temp21 <- logBound(t$nonnegUS_fdi_efficiencypGDP,forceAny=T)   
  temp22 <- logBound(t$nonnegUS_fdi_lowskillpGDP,forceAny=T)     
  temp23 <- logBound(t$nonnegUS_fdi_lowskillppGDP,forceAny=T)
  temp24 <- logBound(t$nonnegUS_fdi_lowskillmpGDP,forceAny=T)   
  temp25 <- logBound(t$nonnegUS_fdi_lowskill1pGDP,forceAny=T)    
  temp26 <- logBound(t$nonnegUS_fdi_highskillpGDP,forceAny=T)    
  temp27 <- logBound(t$nonnegUS_fdi_hightechknowpGDP,forceAny=T) 
  temp28 <- logBound(t$nonnegUS_fdi_lowtechknowpGDP,forceAny=T)  
  temp29 <- logBound(t$nonnegUS_fdi_lowtechknowmpGDP,forceAny=T)
  temp30 <- logBound(t$nonnegUS_fdi_lowtechknowppGDP,forceAny=T) 
  temp31 <- logBound(t$nonnegUS_fdi_lowtechknow1pGDP,forceAny=T)
  
  
  t$"lognonnegUS_FDIpGDP"                <- temp[,2]
  t$"lognonnegUS_fdi_petrolpGDP"         <- temp1[,2]
  t$"lognonnegUS_fdi_total_manufpGDP"    <- temp2[,2]
  t$"lognonnegUS_fdi_foodpGDP"           <- temp3[,2]
  t$"lognonnegUS_fdi_chemicalpGDP"       <- temp4[,2]
  t$"lognonnegUS_fdi_prim_fab_metalpGDP" <- temp5[,2]
  t$"lognonnegUS_fdi_machinerypGDP"      <- temp6[,2]
  t$"lognonnegUS_fdi_electricalpGDP"     <- temp7[,2]
  t$"lognonnegUS_fdi_transportpGDP"      <- temp8[,2]
  t$"lognonnegUS_fdi_whole_tradepGDP"    <- temp9[,2]
  t$"lognonnegUS_fdi_depositorypGDP"     <- temp10[,2]
  t$"lognonnegUS_fdi_finance_exceptpGDP" <- temp11[,2]
  t$"lognonnegUS_fdi_miningpGDP"         <- temp12[,2]
  
  t$"lognonnegUS_fdi_all_financepGDP"  <- temp13[,2]
  t$"lognonnegUS_fdi_servicepGDP"      <- temp14[,2]
  t$"lognonnegUS_fdi_food_metpGDP"     <- temp15[,2]
  t$"lognonnegUS_fdi_resourcepGDP"     <- temp16[,2]
  t$"lognonnegUS_fdi_marketpGDP"       <- temp17[,2]
  t$"lognonnegUS_fdi_market1pGDP"      <- temp18[,2]
  t$"lognonnegUS_fdi_market2pGDP"      <- temp19[,2]
  t$"lognonnegUS_fdi_market3pGDP"      <- temp20[,2]
  t$"lognonnegUS_fdi_efficiencypGDP"   <- temp21[,2]
  t$"lognonnegUS_fdi_lowskillpGDP"     <- temp22[,2]
  t$"lognonnegUS_fdi_lowskillppGDP"    <- temp23[,2]
  t$"lognonnegUS_fdi_lowskillmpGDP"    <- temp24[,2]
  t$"lognonnegUS_fdi_lowskill1pGDP"    <- temp25[,2]
  t$"lognonnegUS_fdi_highskillpGDP"    <- temp26[,2]
  t$"lognonnegUS_fdi_hightechknowpGDP" <- temp27[,2]
  t$"lognonnegUS_fdi_lowtechknowpGDP"  <- temp28[,2]
  t$"lognonnegUS_fdi_lowtechknowmpGDP" <- temp29[,2]
  t$"lognonnegUS_fdi_lowtechknowppGDP" <- temp30[,2]
  t$"lognonnegUS_fdi_lowtechknow1pGDP" <- temp31[,2]
  
  # other logs
  t$"logGDPpc"             <- log(t$GDPpc)
  t$"logpopulation"        <- log(t$population)
  t$"logtrade"             <- log(t$trade)
  
  
  #rescale PTS ai
  # Original: 5 = Teror has expanded, 1 = secure rule of law
  t$PTS_ai_reversed <- 6-(t$PTS_ai) 
  #t[500:550,c(17,113)] #to check
  #see http://randomresearchdata.blogspot.co.uk/2012/01/recoding-variables-in-r.html
  #http://psych.hanover.edu/classes/ResearchMethods/Assignments/reliability-1.html
  #1 to 5   to get this, calculate: 6 - PTS_ai = 6-1 = 5
  #2 to 4   6 - PTS_ai = 6-2 = 4
  #3 to 3   6 - PTS_ai = 6-3 = 3
  #4 to 2   etc
  #5 to 1
  
  # re-create Mosley overall index
  # t$"lawneg"        <- 37 - t$Mos_labor_law
  # t$"pracneg"       <- 37 - t$Mos_labor_prac
  # t$"Mos_Laborneg"  <- t$lawneg + t$lawneg
  # t$"Mos_Labor"     <- 37 - t$Mos_Laborneg
  
  
  # create first difference, including for human rights variables!
  t.lag <- t
  t.lag$"Mos_labor_law.lag" <- lagpanel(t$Mos_labor_law,t$country,t$year,1) # this lag only for first differencing!
  t.lag$"Mos_labor_law.diff" <- t.lag$Mos_labor_law - t.lag$Mos_labor_law.lag
  t.lag$"Mos_labor_prac.lag" <- lagpanel(t$Mos_labor_prac,t$country,t$year,1) # this lag only for first differencing!
  t.lag$"Mos_labor_prac.diff" <- t.lag$Mos_labor_prac - t.lag$Mos_labor_prac.lag
  t.lag$"CIRI_PHYSINT.lag" <- lagpanel(t$CIRI_PHYSINT,t$country,t$year,1)
  t.lag$"CIRI_PHYSINT.diff" <- t.lag$CIRI_PHYSINT - t.lag$CIRI_PHYSINT.lag
  t.lag$"PTS_ai.lag_reversed" <- lagpanel(t$PTS_ai_reversed,t$country,t$year,1) #note I use the reversed form here
  t.lag$"PTS_ai_reversed_diff" <- t.lag$PTS_ai_reversed - t.lag$PTS_ai.lag_reversed
  
  # check all the PTS_ai variables
  #t.lag[500:550,c(17,113,118,119)] #ok
  
  
  
  return(t.lag)
} ##end function my.transform1


##------------------------------------------------------------------------------------------------------









##------------------------------------------------------------------------------------------------------
# original FDI variables per GDP (not the nonnegative ones)
my.pergdp <- function(x,cols){
  for (i in 1 : length(x)){
    d <- x[[i]][,cols]/x[[i]]$GDP_curr
    colnames(d) <- paste(cols,"_pGDP",sep="")
    x[[i]] <- cbind(x[[i]],d)
  }
  return(x)
}# end my.pergdp


##------------------------------------------------------------------------------------------------------













##------------------------------------------------------------------------------------------------------

##
## FUNCTION to log a SINGLE column according to your rules
##
my.log <- function(x){
  y <- x[!is.na(x)]
  if (all(y>0)){ 
    z <- log(y) 
  } else {
    z <- log(y - min(y) + 1) ## min(x) substracted because it is by definition negative
  } # end if
  x[!is.na(x)] <- z
  return(x)
} # end function my.log

##
## FUNCTION to log ALL columns and rename the colnames
##
my.log.vars <- function(M,cols,prefix="log_"){ ## M stands for one of your matrices (e.g. m[[1]]); cols are the columns to be logged
  # check that 'cols' specifies columns that are actually in 'M'
  if (!all(cols %in% colnames(M))) stop("cols input variable contains a name not in colnames(M) ")  
  # apply function my.log to every column specified in 'cols'
  M.log <- apply(M[,cols,drop=FALSE],2,my.log)
  # rename the column names of the logged columns
  colnames(M.log) <- paste(prefix,cols,sep="")
  # append logged columns to original matrix
  return(cbind(M,M.log))
} # end my.log.vars

##
## FUNCTION to run my.log.vars on all matrices in a list 
##
my.log.vars.list <- function(L,cols){ ## L is a list of matrices, cols the columns to be logged in each one
  L.log <- list()
  for (i in 1:length(L)){
    L.log[[i]] <- my.log.vars(L[[i]],cols)
    names(L.log)[i] <- names(L)[i]
  } ## end for i
  return(L.log)
} ## end function

#How to use it:

#Example 1:
M <- matrix(ncol=2,nrow=5)
M[,1] <- c(1,3,5,-10,-4)
M[,2] <- c(0,5,6,7,11)
colnames(M) <- c("A","B")
M <- as.data.frame(M)
M
dim(M)
attach(M)
cols <- c("A")
M2 <- my.log.vars(M,cols)
M2

#Example 2:
#load("m.Rdata")
#tobelogged <- c("US_fdi_total","US_fdi_petrol","US_fdi_total_manuf","US_fdi_food","US_fdi_chemical","US_fdi_prim_fab_metal","US_fdi_machinery","US_fdi_         electrical","US_fdi_transport","US_fdi_whole_trade","US_fdi_depository","US_fdi_finance_except","US_fdi_mining")      
#m.log <- my.log.vars.list(m,tobelogged)

##------------------------------------------------------------------------------------------------------



















##------------------------------------------------------------------------------------------------------
# Take newly logged vars and lag them

my.lag.vars <- function(x,vars,prefix="Lag_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,1) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} ##end function 

## FUNCTION to run my.lag.vars on all matrices in a list 
##
my.lag.vars.list <- function(L,cols){ ## L is a list of matrices, cols the columns to be lagged in each one
  L.lag <- list()
  for (i in 1:length(L)){
    L.lag[[i]] <- my.lag.vars(L[[i]],cols)
    names(L.lag)[i] <- names(L)[i]
  } ## end for i
  return(L.lag)
} ## end function

##------------------------------------------------------------------------------------------------------



















##------------------------------------------------------------------------------------------------------
cat("Creating mapping list ...")
mappingList <- list(
  AFG = c("Afghanistan"),
  ALB = c("Albania"),
  DZA = c("Algeria"),
  ASM = c("American Samoa"),
  ADO = c("Andorra"),
  AGO = c("Angola"),
  ATG = c("Antigua and Barbuda"),
  ARG = c("Argentina"),
  ARM = c("Armenia"),
  ABW = c("Aruba"),
  AUS = c("Australia"),
  AUT = c("Austria"),
  AZE = c("Azerbaijan"),
  BHS = c("Bahamas", "Bahamas, The"),
  BHR = c("Bahrain"),
  BGD= c("Bangladesh"),
  BRB = c("Barbados"),
  BLR = c("Belarus"),
  BEL = c("Belgium"),
  BLZ = c("Belize"),
  BEN = c("Benin"),
  BMU = c("Bermuda"),
  BTN = c("Bhutan"),
  BOL = c("Bolivia", "Bolivia (Plurinational State of)"),
  BIH = c("Bosnia and Herzegovina", "Bosnia-Herzegovina", "Bosnia Herzegovenia"), #the third from CIRI
  BWA = c("Botswana"),
  BRA = c("Brazil"),
  BRN = c("Brunei Darussalam", "Brunei"),
  BGR = c("Bulgaria"),
  BFA = c("Burkina Faso", "Burkina"),
  BDI = c("Burundi"),
  KHM = c("Cambodia"),
  CMR = c("Cameroon"),
  CAN = c("Canada"),
  CPV = c("Cape Verde"),
  CYM = c("Cayman Islands"),
  CAF = c("Central African Republic"),
  TCD = c("Chad"),
  CHI = c("Channel Islands"),
  CHL = c("Chile"),
  CHN = c("China"),
  COL = c("Colombia"),
  COM = c("Comoros"),
  ZAR = c("Congo, Dem. Rep.","Democratic Republic of the Congo (Zaire)","Congo, Democratic Republic of", "Zaire", "Congo (Kinshasa)", "Congo Kinshasa", "Democratic Republic of the Congo", "Dem. Rep. of the Congo"),
  COG = c("Congo, Rep.", "Congo, Republic of", "Congo (Brazzaville)", "Congo", "Congo Brazzaville"),
  CRI = c("Costa Rica"),
  CIV = c("Côte d'Ivoire","Cote d'Ivoire", "Ivory Coast", "Cote D'Ivoire", "Ivory Coast (Cote d'Ivoire)"),#weird numbers from UNCTAD
  HRV = c("Croatia"),
  CUB = c("Cuba"),
  CUW = c("Curaçao", "Curacao"),
  CYP = c("Cyprus"),
  CZE = c("Czech Republic", "Czechoslovakia", "Czechoslovakia (former)"), #Czechoslovakia (former) because of UN FDI
  DNK = c("Denmark"),
  DJI = c("Djibouti"),
  DMA = c("Dominica"),
  DOM = c("Dominican Republic", "Dominican Rep"),
  ECU = c("Ecuador"),
  EGY = c("Egypt, Arab Rep.", "Egypt"),
  SLV = c("El Salvador"),
  GNQ = c("Equatorial Guinea"),
  ERI = c("Eritrea"),
  EST = c("Estonia"),
  ETH = c("Ethiopia", "Ethiopia (former)","Ethiopia (-1991)"), #Ethiopia numbers UNCTAD
  FRO = c("Faeroe Islands"),
  FJI = c("Fiji"),
  FIN = c("Finland"),
  FRA = c("France"),
  PYF = c("French Polynesia"),
  GAB = c("Gabon"),
  GMB = c("Gambia, The", "Gambia"),
  GEO = c("Georgia"),
  DEU = c("Germany","Germany, Federal Republic of", "Germany West", "Germany (former Federal Rep.)"), #Germany West because of polity Germany (former Federal Rep.) bec.of UN FDI
  GHA = c("Ghana"),
  GIB = c("Gibraltar","United Kingdom Overseas Territories: Gibraltar"),
  GRC = c("Greece"),
  GRL = c("Greenland"),
  GRD = c("Grenada"),
  GUM = c("Guam"),
  GTM = c("Guatemala"),
  GIN = c("Guinea"),
  GNB = c("Guinea-Bissau"),
  GUY = c("Guyana"),
  HTI = c("Haiti"),
  HND = c("Honduras"),
  HKG = c("Hong Kong SAR, China", "Hong Kong", "Hong Kong, China","China, Hong Kong SAR"),
  HUN = c("Hungary"),
  ISL = c("Iceland"),
  IND = c("India"),
  IDN = c("Indonesia","Indonesia including East Timor", "Indonesia (-2002)"), #Indonesia including East Timor bec. of UN FDI
  IRN = c("Iran, Islamic Rep.", "Iran", "Iran (Islamic Republic of)"),
  IRQ = c("Iraq"),
  IRL = c("Ireland"),
  IMY = c("Isle of Man"),
  ISR = c("Israel", "Israel and Occupied Territories**"), #Israel and Occupied Territories** from PTS
  ITA = c("Italy"),
  JAM = c("Jamaica"),
  JPN = c("Japan"),
  JOR = c("Jordan"),
  KAZ = c("Kazakhstan"),
  KEN = c("Kenya"),
  KIR = c("Kiribati"),
  PRK = c("Korea, Dem. Rep.", "North Korea", "Korea, Democratic People's Republic of", "Korea North", "North Korea (Democratc People's Republic of Korea)","Korea, Dem. People's Rep. of"),
  KOR = c("Korea, Rep.", "South Korea", "Korea, Republic of", "Korea South", "South Korea (Republic of Korea)", "Korea"), #Korea added because of OECD
  KSV = c("Kosovo"),
  KWT = c("Kuwait"),
  KGZ = c("Kyrgyz Republic", "Kyrgyzstan"),
  LAO = c("Lao PDR", "Laos", "Lao People's Dem. Rep."),
  LVA = c("Latvia"),
  LBN = c("Lebanon"),
  LSO = c("Lesotho"),
  LBR = c("Liberia"),
  LBY = c("Libya", "Libyan Arab Jamahiriya"),
  LIE = c("Liechtenstein"),
  LTU = c("Lithuania"),
  LUX = c("Luxembourg"),
  MAC = c("Macao SAR, China", "Macao, China", "China, Macao SAR"),
  MKD = c("Macedonia, FYR", "Macedonia", "TFYR of Macedonia"),
  MDG = c("Madagascar"),
  MWI = c("Malawi"),
  MYS = c("Malaysia"),
  MDV = c("Maldives"),
  MLI = c("Mali"),
  MLT = c("Malta"),
  MHL = c("Marshall Islands"),
  MRT = c("Mauritania"),
  MUS = c("Mauritius"),
  MYT = c("Mayotte"),
  MEX = c("Mexico"),
  FSM = c("Micronesia, Fed. Sts.","Micronesia, Federated States of", "Federated States of Micronesia", "Micronesia","Micronesia (Federated States of)"),
  MDA = c("Moldova", "Republic of Moldova"),
  MCO = c("Monaco"),
  MNG = c("Mongolia"),
  MNE = c("Montenegro"),
  MAR = c("Morocco"),
  MOZ = c("Mozambique"),
  MMR = c("Myanmar", "Burma", "Republic of the Union of Myanmar", "Myanmar (Burma)"),
  NAM = c("Namibia"),
  NPL = c("Nepal"),
  NLD = c("Netherlands"),
  NCL = c("New Caledonia"),
  NZL = c("New Zealand"),
  NIC = c("Nicaragua"),
  NER = c("Niger"),
  NGA = c("Nigeria"),
  MNP = c("Northern Mariana Islands"),
  NOR = c("Norway"),
  OMN = c("Oman"),
  PAK = c("Pakistan"),
  PLW = c("Palau"),
  PAN = c("Panama"),
  PNG = c("Papua New Guinea"),
  PRY = c("Paraguay"),
  PER = c("Peru"),
  PHL = c("Philippines"),
  POL = c("Poland"),
  PRT = c("Portugal"),
  PRI = c("Puerto Rico"),
  QAT = c("Qatar"),
  ROM = c("Romania", "Rumania"),
  RUS = c("Russian Federation", "Russia", "Russia (Soviet Union)","Union of Soviet Socialist Republics"), # deleted USSR because of PTS (duplicated with RUS) #Careful, "Soviet Union" in CIRI was not just another name for RUSSIA but other country altogether. Russia(n) Federation exists since 1991. See also duplicated() 
  RWA = c("Rwanda"),
  WSM = c("Samoa", "Western Samoa"),
  SMR = c("San Marino"),
  STP = c("São Tomé and Principe", "Sao Tome and Principe"),
  SAU = c("Saudi Arabia"),
  SEN = c("Senegal"),
  SRB = c("Serbia"),
  SYC = c("Seychelles"),
  SLE = c("Sierra Leone"),
  SGP = c("Singapore"),
  SXM = c("Sint Maarten (Dutch part)"),
  SVK = c("Slovak Republic", "Slovakia"),
  SVN = c("Slovenia"),
  SLB = c("Solomon Islands"),
  SOM = c("Somalia"),
  ZAF = c("South Africa"),
  ESP = c("Spain"),
  LKA = c("Sri Lanka"),
  KNA = c("St. Kitts and Nevis","Saint Kitts and Nevis"),
  LCA = c("St. Lucia", "Saint Lucia"),
  MAF = c("St. Martin (French part)"),
  VCT = c("St. Vincent and the Grenadines", "Saint Vincent and the Grenadines", "St. Vincent"),
SSD = c("South Sudan"), #added because of Kaletski
  SDN = c("Sudan","Sudan (-2011)"),
  SUR = c("Suriname"),
  SWZ = c("Swaziland"),
  SWE = c("Sweden"),
  CHE = c("Switzerland"),
  SYR = c("Syrian Arab Republic", "Syria"),
 TWN = c("Taiwan"), #added because of Kaletski
  TJK = c("Tajikistan"),
  TZA = c("Tanzania","United Republic of Tanzania"),
  THA = c("Thailand"),
  TMP = c("Timor-Leste", "Democratic Republic of Timor-Leste ", "East Timor", "East Timor (Timor L'este)"),
  TGO  = c("Togo"),
  TON = c("Tonga"),
  TTO = c("Trinidad and Tobago", "Trinidad"), #Trinidad added because of Polity data
  TUN = c("Tunisia"),
  TUR = c("Turkey"),
  TKM = c("Turkmenistan"),
  TCA = c("Turks and Caicos Islands"),
  TUV = c("Tuvalu"),
  UGA = c("Uganda"),
  UKR = c("Ukraine"),
  ARE = c("United Arab Emirates", "UAE"), #UAE because of Polity 
  GBR = c("United Kingdom","UK"),
  USA  = c("United States", "United States of America","USA"),
  URY  = c("Uruguay"),
  UZB  = c("Uzbekistan"),
  VUT  = c("Vanuatu"),
  VEN  = c("Venezuela, RB", "Venezuela", "Venezuela (Bolivarian Republic of)"),
  VNM = c("Vietnam", "Vietnam, Socialist Republic of", "Viet Nam"),
  VIR = c("Virgin Islands (U.S.)"),
  WBG = c("West Bank and Gaza"),
  YEM = c("Yemen, Rep.", "Yemen", "Yemen South","Yemen, South","SouthYemen"), #South Yemen added because of Polity, Yemen, South added because of Mosley 2011, SouthYemen added because of PTS typo
  ZMB = c("Zambia"),
  ZWE = c("Zimbabwe")
) ##End of mappingList definition
mappingVector <- rep(names(mappingList),sapply(mappingList,length)) 
names(mappingVector) <- unlist(mappingList)
#end of funtion
##------------------------------------------------------------------------------------------------------








##------------------------------------------------------------------------------------------------------

# Adding wb_code to data set so I can make country subsets

my.transform2 <- function(t){ #my.transform.1 adds variables wbcode, oecd, dev so that I can make subests
  
  # step 2: make wb_code in t (country is the first column)                           
  wb_code.t <- mappingVector[as.character(t[,"country"])]
  length(unique(wb_code.t))                                #146
  foo <- which(!(t$country %in% names(mappingVector)))     #checking which countries are not mapped (see if they should be mapped)
  unique(t$country[foo])                                   #factor(0) because t was made from master table anyways and went through this
  wb_code.t1 <- wb_code.t[!is.na(wb_code.t)]               #delete where wb_code = NA
  length(unique(wb_code.t))                                #146 (were no NAs)
  t$"wb_code" <- wb_code.t                             
  t1 <- t[t[,"wb_code"]%in%wb_code.t,]                     #another precaution I don't really need, just for check
  dim(t)                                                   #  3796  143
  dim(t1)
  
  colnames(M)
  M1 <- M[,c(2,5,9)]
  
  # merge t and M through wb_code
  dim(t1)                                                  # 3796   94
  t.new <- merge(t1,M1,by="wb_code",all.x=TRUE) 
  dim(t.new)                                               # 3796   99
  head(t.new)
  length(unique(t.new$country))                    #We still have 146 countries
  
  
  return(t.new)
}  ##end function my.transform2
##------------------------------------------------------------------------------------------------------


















##------------------------------------------------------------------------------------------------------
# function to make subsets

my.makesubsets <- function(x) {
  
  #####################################################
  # MAKE SUBSETS FOR DEVELOPING COUNTRIES - non-oecd
  ##################################################### 
  # make first subset: only non-oecd countries
  
  m.nono <- x[x$oecd==0, ]
  #dim(t.new)  #3796  145
  #dim(t.nono) # 3042  145
  #head(t.nono)
  #save(t.nono,file="t.nono.Rdata")
  
  #####################################################
  # MAKE SUBSETS FOR DEVELOPING COUNTRIES - world bank
  #####################################################
  #alternative:
  # make second subset: only developing countries according to World Bank criteria
  # same as all non-high income countries
  
  m.dev <- x[x$wb_dev==1, ]
  #dim(t.new)  #3796   14
  #dim(t.dev) # 2678  145
  #head(t.dev)
  #save(t.dev,file="t.dev.Rdata")
  
  #y
  
  #####################################################
  # MAKE SUBSETS FOR DEVELOPING COUNTRIES - Neumayer
  #####################################################
  #another alternative: Neumayer/Soysa, Globalization and the Right to Free Association and Collective Bargaining: An Empirical Analysis, 2006 
  #"Are our results driven by the inclusion of developed countries in the sample? In Table 3, we drop Canada, the United States, Western Europe (with the exception of Malta and Cyprus), Japan, Australia, and New Zealand from the sample."
  
  # subset without U.S. (already), Canada, Japan, Australia, New Zealand, Western Europe (keeping Malta and Cyprus)
  # Westen Europe classification from Neumayer country list appendix
  # Nici: Be aware that I have more countries than Neumayer/Soysa in the data set; I checked by eye if there are any other WEur countries (no)
  unique(x$country)
  out <- which(x$country=="Canada" | x$country=="Japan" | x$country=="Australia" | x$country=="New Zealand" | 
                 x$country=="Austria"|
                 x$country=="Belgium"|
                 x$country=="Denmark"|
                 x$country=="Finland"|
                 x$country=="France"|
                 x$country=="Germany"|
                 x$country=="Greece"|
                 x$country=="Iceland"|
                 x$country=="Ireland"|
                 x$country=="Italy"|
                 x$country=="Luxembourg"|
                 x$country=="Netherlands"|
                 x$country=="Norway"|
                 x$country=="Portugal"|
                 x$country=="Spain"|
                 x$country=="Sweden"|
                 x$country=="Switzerland"|
                 x$country=="United Kingdom"
  )
  #dim(t.lag) #3796  142
  m.neum <- x[-out,]
  #dim(t.neum) # 3250  142
  #head(t.neum)
  #save(t.neum,file="t.neum.Rdata")
  
  
  return(list("m.nono"=m.nono,"m.dev"=m.dev,"m.neum"=m.neum))
  
} #end of function my.makesubsets

##------------------------------------------------------------------------------------------------------


