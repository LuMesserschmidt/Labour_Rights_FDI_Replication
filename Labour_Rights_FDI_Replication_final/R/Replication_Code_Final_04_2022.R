# Title: Unravelling the ‘race to the bottom’ argument:  How does FDI affect different types of labour rights?
# Authors: Nicole Janz, Luca Messerschmidt
# Date: 01/05/2022
 
# Necessary Files
# controls.Rdata (contains raw data from WorldBank
# LRI (contains raw data about Labour Rights)
# FDI Data from UNCTAD



#--------------
  #R Datamanagement
  
#--------------
  
  # Load Packages ----


rm(list=ls())

source("R/packages.R")
source("R/Important-functions.R")

# Lag Functions ----
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


# Load Raw Data----

## Load Labour Rights Data ----
load("data/LRI.Rdata")

df <- LRI

# Weighting Union Indicator (see paper)----

# LRI_Union_W1_5
df$LRI_Union_W1_5 = ((1.5*(df$LRI_25 + df$LRI_26))+df$LRI_27 + df$LRI_28 + df$LRI_29 + df$LRI_30 + df$LRI_31)/7
# LRI_Union_W2_5

df$LRI_Union_W2_5 = ((2.5*(df$LRI_25 + df$LRI_26))+df$LRI_27 + df$LRI_28 + df$LRI_29 + df$LRI_30 + df$LRI_31)/7

#LRI_Union_W3

df$LRI_Union_W3 = ((3*(df$LRI_25 + df$LRI_26))+df$LRI_27 + df$LRI_28 + df$LRI_29 + df$LRI_30 + df$LRI_31)/7

#LRI_Union_W3_5

df$LRI_Union_W3_5 = ((3.5*(df$LRI_25 + df$LRI_26))+df$LRI_27 + df$LRI_28 + df$LRI_29 + df$LRI_30 + df$LRI_31)/7

# LRI_Strike_W1_5
df$LRI_Strike_W1_5 = (df$LRI_32+df$LRI_33+df$LRI_34+df$LRI_35+(1.5*(df$LRI_36))+df$LRI_37 + df$LRI_38 + df$LRI_39 + df$LRI_40)/9

# LRI_Strike_2_5
df$LRI_Strike_W2_5 = (df$LRI_32+df$LRI_33+df$LRI_34+df$LRI_35+(2.5*(df$LRI_36))+df$LRI_37 + df$LRI_38 + df$LRI_39 + df$LRI_40)/9

# LRI_Strike_W3
df$LRI_Strike_W3 = (df$LRI_32+df$LRI_33+df$LRI_34+df$LRI_35+(3*(df$LRI_36))+df$LRI_37 + df$LRI_38 + df$LRI_39 + df$LRI_40)/9

# LRI_Strike_W3_5
df$LRI_Strike_W3_5 = (df$LRI_32+df$LRI_33+df$LRI_34+df$LRI_35+(3.5*(df$LRI_36))+df$LRI_37 + df$LRI_38 + df$LRI_39 + df$LRI_40)/9


# LRI_Total_W1_5
df$LRI_Total_W1_5 = (df$LRI_Contracts + df$LRI_Worktime + df$LRI_Dismiss + df$LRI_Union_W1_5 + df$LRI_Strike_W1_5)/5
# LRI_Total_W2_5
df$LRI_Total_W2_5 = (df$LRI_Contracts + df$LRI_Worktime + df$LRI_Dismiss + df$LRI_Union_W2_5 + df$LRI_Strike_W2_5)/5
# LRI_Total_W3
df$LRI_Total_W3 = (df$LRI_Contracts + df$LRI_Worktime + df$LRI_Dismiss + df$LRI_Union_W3 + df$LRI_Strike_W3)/5
# LRI_Total_W3_5
df$LRI_Total_W3_5 = (df$LRI_Contracts + df$LRI_Worktime + df$LRI_Dismiss + df$LRI_Union_W3_5 + df$LRI_Strike_W3_5)/5

library(BBmisc)

## Linear transformation to range weighted indicators between 0 and 1

df$LRI_Union_W1_5<-normalize(df$LRI_Union_W1_5, method = "range", range = c(0, 1))
df$LRI_Strike_W1_5<-normalize(df$LRI_Strike_W1_5, method = "range", range = c(0, 1))
df$LRI_Total_W1_5<-normalize(df$LRI_Total_W1_5, method = "range", range = c(0, 1))

df$LRI_Union_W2_5<-normalize(df$LRI_Union_W2_5, method = "range", range = c(0, 1))
df$LRI_Strike_W2_5<-normalize(df$LRI_Strike_W2_5, method = "range", range = c(0, 1))
df$LRI_Total_W2_5<-normalize(df$LRI_Total_W2_5, method = "range", range = c(0, 1))

df$LRI_Union_W3<-normalize(df$LRI_Union_W3, method = "range", range = c(0, 1))
df$LRI_Strike_W3<-normalize(df$LRI_Strike_W3, method = "range", range = c(0, 1))
df$LRI_Total_W3<-normalize(df$LRI_Total_W3, method = "range", range = c(0, 1))

df$LRI_Union_W3_5<-normalize(df$LRI_Union_W3_5, method = "range", range = c(0, 1))
df$LRI_Strike_W3_5<-normalize(df$LRI_Strike_W3_5, method = "range", range = c(0, 1))
df$LRI_Total_W3_5<-normalize(df$LRI_Total_W3_5, method = "range", range = c(0, 1))

LRI <- df[,-c(4:43)]




## Load Controls ----

load("data/controls.Rdata")


## Load and manage FDI data -----


# UNCTAD - FDI flow (in Millions of US $) - converted below####
###do all of below or load: UNF2.Rdata and proceed with >make UNF3 >Merge 
#DONE: first delete first rows with extra data in csv 
#DONE: delete weird signs in csv $ECONOMY manually: every time I load in the csv again, it will give a new! number. Would have to change mapping vector every time I recreate master. Use simple find-replace in csv for: Cote d'Ivoire, Curacao | #find and replace with "-" (to keep 2x separate)!: Indonesia (-2002),Ethiopia (-1991),Sudan (-2011) (they are in mapp.v.now)
UNF <- read.csv("data/unctad_inw_fdi_flow.csv",header=TRUE, na.strings=c("..","_","","***"))
dim(UNF)                                               # 11168     6
# delete blanks before country name and write those in new country column
UNF$"ECONOMY2" <- sub(" *","",as.character(UNF[,1]))
wb_codeUNF <- mappingVector[as.character(UNF$ECONOMY2)]
length(unique(wb_codeUNF)) #207
foo <- which(!(UNF$ECONOMY2 %in% names(mappingVector))) #checking which countries are not mapped (see if they should be mapped)
unique(UNF$ECONOMY2[foo])
wb_codeUNF1<- wb_codeUNF[!is.na(wb_codeUNF)]             #delete where wb_code was NA
length(unique(wb_codeUNF1))                            #206
UNF$"wb_code" <- wb_codeUNF                            #kommen einige NA's rein
UNF1 <- UNF[UNF[,"wb_code"]%in%wb_codeUNF1,]
unique(UNF$wb_code)                                    # (incl NA)
unique(UNF1$wb_code)                                   # (no NA left)
UNF1$"index"   <- paste(UNF1$"wb_code",UNF1$"YEAR",sep="-")    
a <- duplicated(UNF1$index)                            #check duplicates
which(a)     
unique(UNF1$ECONOMY[a]) #Czechoslovakia, Ethiopia (-1991),Germany, Federal Republic of,Indonesia (-2002),Sudan (-2011),Union of Soviet Socialist Republics
UNF1$ECONOMY2 <- as.factor(UNF1$ECONOMY2)
#delete duplicates (where cells are empty)
e <- which(UNF1$ECONOMY2=="Czechoslovakia" & UNF1$YEAR %in% 1980:1989)
e                                                     
f <- which(UNF1$ECONOMY2=="Czechoslovakia" & UNF1$YEAR %in% 1993:2011)
f                                                    
g <- which(UNF1$ECONOMY2=="Czech Republic" & UNF1$YEAR %in% 1980:1992)
g                                                    
h <- which(UNF1$ECONOMY2=="Ethiopia" & UNF1$YEAR %in% 1980:1991)
h                                                     
i <- which(UNF1$ECONOMY2=="Ethiopia (-1991)" & UNF1$YEAR %in% 1992:2011)
i                                                    
j <- which(UNF1$ECONOMY2=="Germany" & UNF1$YEAR %in% 1980:1989)
j
k <- which(UNF1$ECONOMY2=="Germany, Federal Republic of" & UNF1$YEAR %in% 1990:2011)
k                                                     
l <- which(UNF1$ECONOMY2=="Indonesia (-2002)" & UNF1$YEAR %in% 2003:2011)
l                                                    
m <- which(UNF1$ECONOMY2=="Indonesia" & UNF1$YEAR %in% 1980:2002)
m                                                     
n <- which(UNF1$ECONOMY2=="Sudan" & UNF1$YEAR %in% 1980:2011)
n                                                     
o <- which(UNF1$ECONOMY2=="Russian Federation" & UNF1$YEAR %in% 1980:1991)
o                                                     #4993 4994 4995 4996 4997 4998 4999 5000 5001 5002 5003 5004
o1 <- which(UNF1$ECONOMY2=="Union of Soviet Socialist Republics" & UNF1$YEAR %in% 1992:2011)
o1 
p <-  which(UNF1$ECONOMY2=="Yemen" & UNF1$YEAR %in% 1980:1989)
p1 <-  which(UNF1$ECONOMY2=="Yemen, Arab Republic" & UNF1$YEAR %in% 1990:2011)
dim(UNF1) #6816   9
UNF2 <- UNF1[-c(e,f,g,h,i,j,k,l,m,n,o,o1,p,p1),]                          
dim(UNF2) #6582    9
#check if duplicates are now gone
a1 <- duplicated(UNF2$index)                          
unique(UNF2$ECONOMY[a1])
which(a1) #integer(0)
oldcols <- colnames(UNF2)         
colnames(UNF2)[oldcols=="US.Dollars.at.current.prices.and.current.exchange.rates.in.millions"]             <- "UN_FDI_flow"
colnames(UNF2)[oldcols=="US.Dollars.at.current.prices.and.current.exchange.rates.per.capita"]             <- "UN_FDI_flow_pc" 
colnames(UNF2)[oldcols=="Percentage.of.Gross.Domestic.Product"]             <- "UN_FDI_flow_pgdp" 
#Millions of Dollars into Dollar (Oct 27, 2013)
UNF2$"UN_FDI_flow"                <- UNF2$UN_FDI_flow*(10^6)
#save UNF2 so it can be loaded to transform to UNF3 and merge
library(plm)
UNF2.pd <- plm.data(UNF2,index=c("ECONOMY2","YEAR"))
UNF2 <- UNF2.pd

#UNCTAD FDI STOCK in Mio of $ - converted below####
#DONE country name preparation like in flow in csv
###do all of below or load: UNST2.Rdata and proceed with >make UNST3 >Merge
UNST <- read.csv("data/unctad_inw_fdi_stock.csv",header=TRUE, na.strings=c("..","_","","***"))
dim(UNST) #7616    6
UNST$"ECONOMY2" <- sub(" *","",as.character(UNST[,1]))
wb_codeUNST <- mappingVector[as.character(UNST$ECONOMY2)]
length(unique(wb_codeUNST)) #206
foo <- which(!(UNST$ECONOMY2 %in% names(mappingVector))) #checking which countries are not mapped (see if they should be mapped)
unique(UNST$ECONOMY2[foo])
wb_codeUNST1<- wb_codeUNST[!is.na(wb_codeUNST)]             #delete where wb_code was NA
length(unique(wb_codeUNST1))                            #206
UNST$"wb_code" <- wb_codeUNST                            #kommen einige NA's rein
UNST1 <- UNST[UNST[,"wb_code"]%in%wb_codeUNST1,]
unique(UNST$wb_code)                                    # (incl NA)
unique(UNST1$wb_code)                                   # (no NA left)
UNST1$"index"   <- paste(UNST1$"wb_code",UNST1$"YEAR",sep="-")    
a <- duplicated(UNST1$index)                            #check duplicates
which(a)     
unique(UNST1$ECONOMY[a])
UNST1$ECONOMY2 <- as.factor(UNST1$ECONOMY2)
#delete duplicates (where cells are empty)
est <- which(UNST1$ECONOMY2=="Czechoslovakia" & UNST1$YEAR %in% 1980:1989)
est                                                     
fst <- which(UNST1$ECONOMY2=="Czechoslovakia" & UNST1$YEAR %in% 1993:2011)
fst                                                    
gst <- which(UNST1$ECONOMY2=="Czech Republic" & UNST1$YEAR %in% 1980:1992)
gst                                                    
hst <- which(UNST1$ECONOMY2=="Ethiopia" & UNST1$YEAR %in% 1980:1991)
hst                                                     
ist <- which(UNST1$ECONOMY2=="Ethiopia (-1991)" & UNST1$YEAR %in% 1992:2011)
ist                                                    
jst <- which(UNST1$ECONOMY2=="Germany" & UNST1$YEAR %in% 1980:1989)
jst
kst <- which(UNST1$ECONOMY2=="Germany, Federal Republic of" & UNST1$YEAR %in% 1990:2011)
kst                                                     
lst <- which(UNST1$ECONOMY2=="Indonesia (-2002)" & UNST1$YEAR %in% 2003:2011)
lst                                                    
mst <- which(UNST1$ECONOMY2=="Indonesia" & UNST1$YEAR %in% 1980:2002)
mst                                                     
nst <- which(UNST1$ECONOMY2=="Sudan" & UNST1$YEAR %in% 1980:2011)
nst                                                     
ost <- which(UNST1$ECONOMY2=="Russian Federation" & UNST1$YEAR %in% 1980:1991)
ost                                                     
o1st <- which(UNST1$ECONOMY2=="Union of Soviet Socialist Republics" & UNST1$YEAR %in% 1992:2011)
o1st
pst <-  which(UNST1$ECONOMY2=="Yemen" & UNST1$YEAR %in% 1980:1989)
pst1 <-  which(UNST1$ECONOMY2=="Yemen, Arab Republic" & UNST1$YEAR %in% 1990:2011)
dim(UNST1) #6816    9
UNST2 <- UNST1[-c(est,fst,gst,hst,ist,jst,kst,lst,mst,nst,ost,o1st,pst,pst1),]                          
dim(UNST2) #6582    9,
#check if duplicates are now gone
a1 <- duplicated(UNST2$index)                          
unique(UNST2$ECONOMY[a1])
which(a1) #integer(0)
oldcols <- colnames(UNST2)         
colnames(UNST2)[oldcols=="US.Dollars.at.current.prices.and.current.exchange.rates.in.millions"]             <- "UN_FDI_stock"
colnames(UNST2)[oldcols=="US.Dollars.at.current.prices.and.current.exchange.rates.per.capita"]             <- "UN_FDI_stock_pc"
colnames(UNST2)[oldcols=="Percentage.of.Gross.Domestic.Product"]             <- "UN_FDI_stock_pgdp"
#Millions of Dollars into Dollar (Oct 27, 2013)
UNST2$"UN_FDI_stock"                <- UNST2$UN_FDI_stock*(10^6)

#save UNST2 so it can be loaded to transform to UNST3 and merge
library(plm)
UNST2.pd <- plm.data(UNST2,index=c("ECONOMY2","YEAR"))
UNST2 <- UNST2.pd

# Merge Data 

UNST2 <- UNST2 %>% dplyr::select(index,UN_FDI_stock, UN_FDI_stock_pgdp,UN_FDI_stock_pc)
UNF2 <- UNF2 %>% dplyr::select(index,UN_FDI_flow, UN_FDI_flow_pgdp,UN_FDI_flow_pc)
UN <- left_join (UNST2,UNF2)


# Log Data as Blanton

# Values must be set to 1 where <=0 to prepare for logging
us7 <- UN[,c(2:7)] #these are UN vars to log that are =<0
us8 <- as.data.frame(matrix(NA,nrow=nrow(us7),ncol=ncol(us7)) ) #create empty data frame with size of us
colnames(us8) <- colnames(us7)

for(i in 1:nrow(us7)){       
  for(j in 1:ncol(us7)){   
    if (!is.na(us7[i,j])){         #won't work with na's             
      if(us7[i,j] <= 0){
        # tinylognew[i,j] <- 1   
        us8[i,j] <- 1               # set all negative and zero values to 1 as preparation for logging
        
      } # end if <= 0
      else if(us7[i,j] >0){
        us8[i,j] <- us7[i,j]  
      } # end if > 0
    } # end if !is.na
  } # end for j    
} # end for i
a <- colnames(us7)
b <- paste("l_",a,sep="")    #new variable names
colnames(us8) <- b

us9 <- log(us8)
a <- colnames(us7)
b <- paste("log_",a,sep="")    #new variable names
colnames(us9) <- b

# step7: combine all subsets and variables back onto overall table d###

UN<- cbind(
  UN,
  us9  
)


# Merge Data 

df1 <- left_join(controls,UN)
df <- left_join(df1,LRI)


tobelagged <- c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","polity2", "confl","logtrade","logpopulation","logGDPpc","log_UN_FDI_stock","log_UN_FDI_flow", "log_UN_FDI_stock_pgdp","log_UN_FDI_flow_pgdp","log_UN_FDI_stock_pc","log_UN_FDI_flow_pc") #these are the original logged FDI/GDP variables without any lag


df_main<- df

# Including Sum and Average of LRI-CBR data  ----
df_main_1 <- df_main %>%
  group_by(year, country)%>%
  summarise(LRI_sum = sum(LRI_Contracts+df_main$LRI_Worktime+df_main$LRI_Dismiss+df_main$LRI_Union+df_main$LRI_Strike,na.rm = TRUE)) %>% 
  as.data.frame()
df_main_2 <- df_main %>%
  group_by(year, country)%>%
  summarise(LRI_mean = mean(LRI_Contracts+df_main$LRI_Worktime+df_main$LRI_Dismiss+df_main$LRI_Union+df_main$LRI_Strike,na.rm = TRUE)) %>% 
  as.data.frame()

df_main_1[df_main_1=="NaN"]<- NA
df_main_1[df_main_1==0]<- NA
df_main_2[df_main_2=="NaN"]<- NA

df_main_3<- merge(df_main_1, df_main_2, by=c("year","country"))

# Lag Variables and name them accordingly (Y1:5)----

# 1 year lag
# adjust lagging function
my.lag.vars <- function(x,vars,prefix="Lag_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,1) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} 
d1 <- my.lag.vars(df_main,vars=tobelagged)
df_main1 <- d1


# 2 year lag
# adjust lagging function
my.lag.vars <- function(x,vars,prefix="Lag2_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,2) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} 
d2 <- my.lag.vars(df_main,vars=tobelagged)
df_main2 <- d2


# 3 year lag
# adjust lagging function
my.lag.vars <- function(x,vars,prefix="Lag3_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,3) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} 
d3 <- my.lag.vars(df_main,vars=tobelagged)
df_main3 <- d3

# 4 year lag
# adjust lagging function
my.lag.vars <- function(x,vars,prefix="Lag4_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,4) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} 
d4 <- my.lag.vars(df_main,vars=tobelagged)
df_main4 <- d4

# 5 year lag
# adjust lagging function
my.lag.vars <- function(x,vars,prefix="Lag5_"){
  
  if (!all(vars%in%colnames(x))) stop("some vars are not in colnames(x)")
  for (i in vars){
    newname <- paste(prefix,i,sep="")
    x[newname] <- lagpanel(x[i],x$country,x$year,5) #we don't need cbind since we already create new variables with new name
  } #end for
  
  return(x)
} 
d5 <- my.lag.vars(df_main,vars=tobelagged)
df_main5 <- d5


# Merge Data----

df<- left_join(df_main,df_main1)

df2<- left_join(df,df_main2)

df3<- left_join(df2,df_main3)

df4<- left_join(df3,df_main4)

df_5<- left_join(df4,df_main5)

df_main_lag<- df_5


df_main_lag<- merge(df_main_lag, df_main_3,by=c("year","country"))

df_main<-df_main_lag


# Save data as df_main ----
save(df_main,file="data/df_main.Rda")
rm(list=ls())


#--------------
  
  #Descriptive Statistics
  
#--------------
  
  # Load packages and Data ----
rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")


# Generate Core and Cash Variables (see paper) ----
df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)
df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))

# Trend of Core and Cash Standards (FIGURE 1: Labour Standards Protection in Developing Countries Over Time)----

## Subset Developing Countries
df_mainx<- df_main[df_main$wb_dev==1,]


## Summarize by year
df_descriptive_1 <- 
  df_mainx %>% 
  group_by(year) %>%summarise(LRI_Cash = mean(cash,na.rm = TRUE),LRI_Core = mean(core,na.rm = TRUE),FACB_Law = mean(Mos_labor_law,na.rm = TRUE),LRI_Total = mean(LRI_Total_W2_5,na.rm = TRUE),CIRI = mean(CIRI_WORKER,na.rm = TRUE)) %>% 
  as.data.frame()

df_descriptive_2 <- 
  df_mainx %>% 
  group_by(year) %>% 
  summarise(LRI_Total = mean(LRI_Total_W2_5,na.rm = TRUE),LRI_Contracts = mean(LRI_Contracts,na.rm = TRUE), LRI_Worktime = mean(LRI_Worktime,na.rm = TRUE),LRI_Dismiss = mean(LRI_Dismiss,na.rm = TRUE),LRI_Union = mean(LRI_Union_W2_5,na.rm = TRUE),LRI_Strike_W2_5 = mean(LRI_Strike_W2_5,na.rm = TRUE)) %>% 
  as.data.frame()

## By Year and Region
df_descriptive_3 <- 
  df_mainx %>% 
  group_by(year,wb_region) %>% 
  summarise(LRI_Total = mean(LRI_Total_W2_5,na.rm = TRUE),LRI_Contracts = mean(LRI_Contracts,na.rm = TRUE), LRI_Worktime = mean(LRI_Worktime,na.rm = TRUE),LRI_Dismiss = mean(LRI_Dismiss,na.rm = TRUE),LRI_Union = mean(LRI_Union_W2_5,na.rm = TRUE),LRI_Strike = mean(LRI_Strike_W2_5,na.rm = TRUE)) %>% 
  as.data.frame()

df_descriptive_3<- df_descriptive_3[df_descriptive_3$wb_region!="NorthAmerica",]
df_descriptive_3$wb_region<-as.character(df_descriptive_3$wb_region)
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="ECA"]<-"ECA"
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="EAP"]<-"EAP"
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="LAC"]<-"LAC"
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="MNA"]<-"MNA"
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="SAS"]<-"SAS"
df_descriptive_3$wb_region[df_descriptive_3$wb_region=="SSA"]<-"SSA"

df_descriptive_3$wb_region<-as.factor(df_descriptive_3$wb_region)

df_descriptive_3.3 <- 
  df_mainx %>% 
  group_by(year,wb_region) %>% 
  summarise(LRI_Cash = mean(cash,na.rm = TRUE),LRI_Core = mean(core,na.rm = TRUE)) %>% 
  as.data.frame()

df_descriptive_3.3<- df_descriptive_3.3[df_descriptive_3.3$wb_region!="NorthAmerica",]
df_descriptive_3.3$wb_region<-as.character(df_descriptive_3.3$wb_region)
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="ECA"]<-"ECA"
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="EAP"]<-"EAP"
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="LAC"]<-"LAC"
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="MNA"]<-"MNA"
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="SAS"]<-"SAS"
df_descriptive_3.3$wb_region[df_descriptive_3.3$wb_region=="SSA"]<-"SSA"

df_descriptive_3.3$wb_region<-as.factor(df_descriptive_3.3$wb_region)

## Plot Cash vs. core
a<- ggplot(df_descriptive_1, aes(x = year))+
  geom_line(aes(y = LRI_Cash, colour = "Outcome Rights"))+
  geom_line(aes(y = LRI_Core, colour = "Collective Rights"))+
  ggtitle("Outcome vs. Collective Rights")+
  xlab("")+
  ylab("")+
  theme_bw()

a<- a+labs(color='') 
a<- a + ylim(0.3, 0.7)
a<- a+ scale_color_manual(values=c("black", "red"))
a



## Plot Development of Categories
b<- ggplot(df_descriptive_2, aes(x = year))+
  geom_line(aes(y = LRI_Contracts, colour = "Contracts"))+
  geom_line(aes(y = LRI_Dismiss, colour = "Dismissal"))+
  geom_line(aes(y = LRI_Worktime, colour = "Worktime")) + 
  geom_line(aes(y = LRI_Union, colour = "Representation"))+
  geom_line(aes(y = LRI_Strike_W2_5, colour = "Industrial Action"))+
  ggtitle("Five Categories of Labour Standards, Average Over All Developing Countries")+
  xlim(1980,2010)+
  xlab("")+
  ylab("")+
  theme_bw()

b
b<- b+labs(color='') 
b<- b + ylim(0.3, 0.7)
b<- b+ scale_color_manual(values=c("dodgerblue2", "magenta3", "grey67","orange","green3"))
b <- b + theme(legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"), legend.position = "bottom")

## Plot by Region

c<- ggplot(df_descriptive_3, aes(x = year))+
  geom_point(aes(y = LRI_Total,shape=wb_region))+
  scale_shape_manual(values = c(96,96,4,96,96,96),name="Region")+
  geom_line(aes(y = LRI_Total, linetype = wb_region))+
  #scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "F1"),name="Region")+
  ggtitle("Overall Labour Standards, Developing Countries (per Region)")+
  xlim(1980,2010)+
  xlab("")+
  ylab("")+
  theme_bw()
c<-c+ scale_linetype_discrete(name = "Region")
c<- c+ theme(legend.position = "bottom")
c

c1<- ggplot(df_descriptive_3.3, aes(x = year))+
  geom_point(aes(y = LRI_Cash,shape=wb_region))+
  scale_shape_manual(values = c(96,96,4,96,96,96))+
  geom_line(aes(y = LRI_Cash, linetype = wb_region))+
  #geom_line(aes(y = LRI_Cash, linetype = wb_region),color="black")+
  # scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "1F"),name="Region")+
  #geom_point(aes(y = LRI_Cash,shape=wb_region)) +
  ggtitle("Outcome Rights (per Region)")+
  xlim(1980,2010)+
  xlab("")+
  ylab("")+
  theme_bw()

c1<-c1+labs(color='') 
c1<- c1 + ylim(0.3, 0.7)
c1<- c1+ theme(legend.position = "none")
c1

c2<- ggplot(df_descriptive_3.3, aes(x = year))+
  geom_point(aes(y = LRI_Core,shape=wb_region))+
  scale_shape_manual(values = c(96,96,4,96,96,96))+
  geom_line(aes(y = LRI_Core, linetype = wb_region))+
  #  geom_line(aes(y = LRI_Core, linetype = wb_region),color="black")+
  # scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "1F"),name="Region")+
  #  geom_point(aes(y = LRI_Core,shape=wb_region)) +
  ggtitle("Collective Rights (per Region)")+
  xlim(1980,2010)+
  xlab("")+
  ylab("")+
  theme_bw()
c2<-c2 + theme(legend.position = "none")
c2<-c2+labs(color='') 
c2<- c2 + ylim(0.3, 0.7)
c2
library(ggpubr)

z3<-ggarrange(c,                                                 # First row with scatter plot
              ggarrange(c1, c2, ncol = 2),
              b, # Second row with box and dot plots
              nrow = 3,
              heights = c(5,3.5,4.5)
) 
z3

ggsave(filename="viz/Indices_descriptive_final.jpg",
       plot=z3,
       pointsize = 24, 
       width = 14 ,
       height = 24,
       scale = 0.5,
       dpi = 800)


## Plot Cash vs. core SCATTERPLOT


f1<- ggplot(df_descriptive_3.3, aes(x=LRI_Cash, y=LRI_Core,shape=wb_region))+
  geom_point()+
  ggtitle("Scatterplot Outcome vs. Collective Rights (by region)")+
  scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "F1"),name="Region")+
  xlab("Outcome Rights")+
  ylab("Collective Rights")+
  theme_bw()
f1<-f1+labs(shape='Region') 
f1<- f1 + theme(legend.position = "bottom")
f1<- f1 + ylim(0.25, 0.75)
f1


f2<- ggplot(df_descriptive_3.3, aes(x=LRI_Cash, y=LRI_Core,color=wb_region))+
  geom_point()+
  ggtitle("By Region")+
  scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "F1"),name="Region")+
  xlab("Outcome Rights")+
  ylab("Collective Rights")+
  theme_bw()
f2<-f2+labs(color='Region') 
f2<- f2 + theme(legend.position = "right")
f2<- f2 + ylim(0.3, 0.6)
f2<- f2 + xlim(0.3, 0.6)
f2


f3<- ggplot(df_descriptive_1, aes(x=LRI_Cash, y=LRI_Core))+
  geom_point()+
  ggtitle("Overall")+
  #scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "F1"),name="Region")+
  xlab("Outcome Rights")+
  ylab("Collective Rights")+
  theme_bw()
f3<-f3+labs(color='Region') 
f3<- f3 + theme(legend.position = "bottom")
f3<- f3 + ylim(0.3, 0.6)
f3<- f3 + xlim(0.3, 0.6)
f3

z4<-ggpubr::ggarrange(f3, f2, widths =c(4,5))

z4<-ggpubr::annotate_figure(z4, top = text_grob("Outcome vs. Collective Rights by year",color = "black", size = 14))



ggsave(filename="viz/Indices_descriptive_appendix_final.jpg",
       plot=z4,
       pointsize = 24, 
       width = 24 ,
       height = 14,
       scale = 0.5,
       dpi = 800)


# Summary Statistics (TABLE A5: Summary Statistics of Variables) ----


## Statistics Summary of Model

names(df_mainx)

df_mainx <- as.data.frame(df_mainx)
df_main_summary <- na.omit(df_mainx[,cbind("LRI_Total_W2_5","LRI_Contracts", "LRI_Worktime", "LRI_Dismiss","LRI_Union_W2_5","LRI_Strike_W2_5","core","cash","log_UN_FDI_flow","log_UN_FDI_stock","log_UN_FDI_flow_pgdp","log_UN_FDI_stock_pgdp","log_UN_FDI_flow_pc","log_UN_FDI_stock_pc","logtrade","logGDPpc_diff", "logpopulation", "polity2","wb_region", "confl")]) 
df_main_summary<- as.data.frame(df_main_summary)


stargazer(df_main_summary, type="latex", float=F,covariate.labels = c("Overall Rights", "Contracts","Worktime","Dismissal","Representation","Industrial Action","Collective Rights","Outcome Rights",
                                                                     "Log FDI flow","Log FDI stock",
                                                                     "Log FDI flow/GDP","Log FDI stock/GDP",
                                                                     "Log FDI flow per Capita","Log FDI stock per Capita",
                                                                     "Log Trade",
                                                                     "Growth",
                                                                     "Log Population", 
                                                                     "Democracy", 
                                                                     "Conflict"),
          title = "Summary Statistics", style = "commadefault",decimal.mark=".",out="viz/summary_statistics_model.tex")


# Correlation Matrix (TABLE A3: Correlation Table of Variables)----

library(corrplot)


## Correlation Function (calculates p-values). Source: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
library(xtable)


#Matrix 2: All variables used in the main models (pairwise complete correlation)
df_main_cor <- na.omit(df_mainx[,cbind("LRI_Total_W2_5","LRI_Contracts", "LRI_Worktime", "LRI_Dismiss","LRI_Union_W2_5","LRI_Strike_W2_5","core","cash","log_UN_FDI_flow","log_UN_FDI_stock","log_UN_FDI_flow_pgdp","log_UN_FDI_stock_pgdp","log_UN_FDI_flow_pc","log_UN_FDI_stock_pc","logtrade","logGDPpc_diff", "logpopulation", "polity2","confl")]) 

##Rename
df_main_cor$"Overall Rights"<-df_main_cor$LRI_Total_W2_5
df_main_cor$Contracts<-df_main_cor$LRI_Contracts
df_main_cor$Worktime<-df_main_cor$LRI_Worktime
df_main_cor$Dismissal<-df_main_cor$LRI_Dismiss
df_main_cor$Representation<-df_main_cor$LRI_Union_W2_5
df_main_cor$"Industrial Action"<-df_main_cor$LRI_Strike_W2_5
df_main_cor$"Collective Rights"<-df_main_cor$core
df_main_cor$"Outcome Rights"<-df_main_cor$cash
df_main_cor$"Log FDI stock"<-df_main_cor$log_UN_FDI_stock
df_main_cor$"Log FDI flow"<-df_main_cor$log_UN_FDI_flow
df_main_cor$"Log FDI stock/GDP"<-df_main_cor$log_UN_FDI_stock_pgdp
df_main_cor$"Log FDI flow/GDP"<-df_main_cor$log_UN_FDI_flow_pgdp
df_main_cor$"Log FDI stock per Capita"<-df_main_cor$log_UN_FDI_stock_pc
df_main_cor$"Log FDI flow per Capita"<-df_main_cor$log_UN_FDI_flow_pc
df_main_cor$"Trade"<-df_main_cor$logtrade
df_main_cor$"Growth"<-df_main_cor$logGDPpc_diff
df_main_cor$"Population"<-df_main_cor$logpopulation
df_main_cor$"Democracy"<-df_main_cor$polity2
df_main_cor$Conflict<-df_main_cor$confl

df_main_cor2<- df_main_cor[,-c(1:19)]

names(df_main_cor)
## Calculate Correlation
A<-corstars(df_main_cor2)

library(xtable)
xtable(A)

M <- cor(df_main_cor2,use="pairwise.complete.obs")

C<-head(round(M,2)) # Copy Output to manuscript
rm(list=ls())

#--------------
  
  #Regression Analysis
  
#--------------
  
  # Load Data----
rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")
## Subset for Developing Countries
df_main<- df_main[df_main$wb_dev==1,]

df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)
df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))
df_main = df_main %>% group_by(country) %>% mutate(Lag2_logGDPpc_diff = dplyr::lag(Lag_logGDPpc_diff))
df_main = df_main %>% group_by(country) %>% mutate(Lag3_logGDPpc_diff = dplyr::lag(Lag2_logGDPpc_diff))


#Tests
## Hausman
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pc
fixed <- plm(form1 ,model="within",index=c("country","year"),data=df_main)
random <- plm(form1 ,model="random",index=c("country","year"),data=df_main)
pooled <- plm(form1 ,model="pooling",index=c("country","year"),data=df_main)
phtest(fixed,random)


## Testing for time FE: Lagrange Multiplier Test - time effects (Breusch-Pagan) for unbalanced panels

form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pc + factor(year)
fixed.time <- plm(form1 ,model="within",index=c("country","year"),data=df_main)
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))
## Time FE needed

# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM)
plmtest(pooled, type=c("bp"))
## Better use random effects

# Testing for heteroskedasticity

library(lmtest)
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp +factor(country)
bptest(form1, data = df_main, studentize=F)
## Panel Corrected Standard Errors needed! 
## vcovHC– function recommended for fixed effects
## Heteroskedasticity Detected --> Robust Covariance Matrix needed

## Main Model-----

#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_1.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:5,11,6:10,12),
          covariate.labels = c( "Log FDI stock",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_2_stock.tex")


stargazer(ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:5,11,6:10,12),
          covariate.labels = c( "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_2_flow.tex")


#Coefficient Plot for Main Model (FIGURE 2: Coefficient Plot of FDI Flow and Stock)----
# Coefficient Plot for Stock
model1 <- res.po2
model2 <- res.po3


# Put model estimates into temporary data.frames:

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Outcome Rights")
#model2Frame<-model2Frame[-c(1,3:10),]

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Collective Rights")
#model1Frame<-model1Frame[-c(1,3:10),]


# Combine these data.frames
allModelFrame <- data.frame(rbind(model2Frame, model1Frame))  # etc.

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(name = "",values=c("black", "red"),guide = guide_legend(nrow=2,reverse=TRUE))
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")
a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
a<-a + scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom",legend.direction="vertical")
b <- a + ggtitle("Coefficient Plot for FDI Stock")

CC_stock<- b 

CC_stock
ggsave(filename="viz/Coefficientplot_Cash_Core_Stock_FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


# Coefficient Plot for Flow
model1 <- res.po10
model2 <- res.po11


# Put model estimates into temporary data.frames:

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Outcome Rights")
#model2Frame<-model2Frame[-c(1,3:10),]

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Collective Rights")
#model1Frame<-model1Frame[-c(1,3:10),]


# Combine these data.frames
allModelFrame <- data.frame(rbind(model2Frame, model1Frame))  # etc.

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(values=c("black", "red"),guide=F)
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
#a <- a  + guides(fill=FALSE)
a
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot for FDI Flow")
CC_flow<- b 
CC_flow
ggsave(filename="viz/Coefficientplot_Cash_Core_Flow_FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)

# Coefficient Plot For Sub-Indices Stock
model1 <- res.po4
model2 <- res.po5
model3 <- res.po6
model4 <- res.po7
model5 <- res.po8

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Contracts")
#model1Frame<-model1Frame[-c(1,3:10),]

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Worktime")
#model2Frame<-model2Frame[-c(1,3:10),]

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Dismissal")
#model3Frame<-model3Frame[-c(1,3:10),]

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Representation")
#model4Frame<-model4Frame[-c(1,3:10),]

model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Industrial Action")
#model5Frame<-model5Frame[-c(1,3:10),]

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame,model4Frame,model5Frame)) 

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(name = "",values=c("dodgerblue", "green3","mediumorchid", "orange","grey74"),guide = guide_legend(nrow=2,reverse=TRUE))
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot of FDI Stock on LRI Sub-Indices",subtitle = "including regional dummies")
a<-a + scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom",legend.direction="vertical")
b
CC2_stock<- a 

ggsave(filename="viz/Coefficientplot_Sub_indices_Stock_FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


# Coefficient Plot For Sub-Indices Flow
model1 <- res.po12
model2 <- res.po13
model3 <- res.po14
model4 <- res.po15
model5 <- res.po16

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Contracts")
#model1Frame<-model1Frame[-c(1,3:10),]

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Worktime")
#model2Frame<-model2Frame[-c(1,3:10),]

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Dismissal")
#model3Frame<-model3Frame[-c(1,3:10),]

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Representation")
#model4Frame<-model4Frame[-c(1,3:10),]

model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Industrial Action")
#model5Frame<-model5Frame[-c(1,3:10),]

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame,model4Frame,model5Frame)) 

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(values=c("dodgerblue", "green3","mediumorchid", "orange","grey74"),guide=F)
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot of FDI Flow on LRI Sub-Indices",subtitle = "including regional dummies")
b
CC2_flow<- a 
ggsave(filename="viz/Coefficientplot_Sub_indices_Flow_FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)

library(lubridate)
library(cowplot) # 1.0.0
library(egg) # 0.4.5
library(ggpubr)
fin<- ggarrange(CC_flow,CC2_flow,
                CC_stock,CC2_stock,heights = c(1, 1.3),
                ncol = 2, nrow = 2)
fin
ggsave(filename="viz/Coefficientplot_all_FE.jpeg",
       plot=fin,
       pointsize = 24, 
       width = 15,
       height = 14,
       scale = 0.5,
       dpi = 800)


#2FE####

# Add Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_2FE_1.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_2FE_2.tex")

#Coefficient Plot for Main Model (FIGURE 2: Coefficient Plot of FDI Flow and Stock)----
# Coefficient Plot for Stock
model1 <- res.po2
model2 <- res.po3


# Put model estimates into temporary data.frames:

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Outcome Rights")
#model2Frame<-model2Frame[-c(1,3:10),]

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Collective Rights")
#model1Frame<-model1Frame[-c(1,3:10),]


# Combine these data.frames
allModelFrame <- data.frame(rbind(model2Frame, model1Frame))  # etc.

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(name = "",values=c("black", "red"),guide = guide_legend(nrow=2,reverse=TRUE))
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")
a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot for FDI Stock")
a<-a + scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom",legend.direction="vertical")
CC_stock<- a 

CC_stock
ggsave(filename="viz/Coefficientplot_Cash_Core_Stock_2FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


# Coefficient Plot for Flow
model1 <- res.po10
model2 <- res.po11


# Put model estimates into temporary data.frames:

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Outcome Rights")
#model2Frame<-model2Frame[-c(1,3:10),]

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Collective Rights")
#model1Frame<-model1Frame[-c(1,3:10),]


# Combine these data.frames
allModelFrame <- data.frame(rbind(model2Frame, model1Frame))  # etc.

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(values=c("black", "red"),guide=F)
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
#a <- a  + guides(fill=FALSE)
a
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot for FDI Flow")
CC_flow<- a 
CC_flow
ggsave(filename="viz/Coefficientplot_Cash_Core_Flow_2FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)

# Coefficient Plot For Sub-Indices Stock
model1 <- res.po4
model2 <- res.po5
model3 <- res.po6
model4 <- res.po7
model5 <- res.po8

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Contracts")
#model1Frame<-model1Frame[-c(1,3:10),]

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Worktime")
#model2Frame<-model2Frame[-c(1,3:10),]

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Dismissal")
#model3Frame<-model3Frame[-c(1,3:10),]

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Representation")
#model4Frame<-model4Frame[-c(1,3:10),]

model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Industrial Action")
#model5Frame<-model5Frame[-c(1,3:10),]

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame,model4Frame,model5Frame)) 

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(name = "",values=c("dodgerblue", "green3","mediumorchid", "orange","grey74"),guide = guide_legend(nrow=2,reverse=TRUE))
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot of FDI Stock on LRI Sub-Indices",subtitle = "including regional dummies")
a<-a + scale_fill_continuous(guide = "colorbar") + theme(legend.position="bottom",legend.direction="vertical")
b
CC2_stock<- a 

ggsave(filename="viz/Coefficientplot_Sub_indices_Stock_2FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)


# Coefficient Plot For Sub-Indices Flow
model1 <- res.po12
model2 <- res.po13
model3 <- res.po14
model4 <- res.po15
model5 <- res.po16

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Contracts")
#model1Frame<-model1Frame[-c(1,3:10),]

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          modelName = "Worktime")
#model2Frame<-model2Frame[-c(1,3:10),]

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          modelName = "Dismissal")
#model3Frame<-model3Frame[-c(1,3:10),]

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          modelName = "Representation")
#model4Frame<-model4Frame[-c(1,3:10),]

model5Frame <- data.frame(Variable = rownames(summary(model5)$coef),
                          Coefficient = summary(model5)$coef[, 1],
                          SE = summary(model5)$coef[, 2],
                          modelName = "Industrial Action")
#model5Frame<-model5Frame[-c(1,3:10),]

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame,model4Frame,model5Frame)) 

allModelFrame$indices <- allModelFrame$modelName
allModelFrame$Variable <- as.character(allModelFrame$Variable)
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow"] <- " "
allModelFrame$Variable <- as.factor(allModelFrame$Variable)

allModelFrame<-allModelFrame[allModelFrame$Variable==" ",]
# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot 
a <- ggplot(allModelFrame, aes(colour = indices))+  scale_color_manual(values=c("dodgerblue", "green3","mediumorchid", "orange","grey74"),guide=F)
a <- a + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
a <- a + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                            ymax = Coefficient + SE*interval1),
                        lwd = 1, position = position_dodge(width = 1/2))
a <- a + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                             ymax = Coefficient + SE*interval2),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 21, fill = "WHITE")

a <- a + coord_flip() + theme_bw()
a<-a+labs(x = "", y = "")
b <- a + ggtitle("Coefficient Plot of FDI Flow on LRI Sub-Indices",subtitle = "including regional dummies")
b
CC2_flow<- a 
ggsave(filename="viz/Coefficientplot_Sub_indices_Flow_2FE.jpeg",
       plot=b,
       pointsize = 24, 
       width = 18 ,
       height = 10,
       scale = 0.5,
       dpi = 800)

library(lubridate)
library(cowplot) # 1.0.0
library(egg) # 0.4.5
library(ggpubr)
fin<- ggarrange(CC_flow,CC2_flow,
                CC_stock,CC2_stock,heights = c(1, 1.3),
                ncol = 2, nrow = 2)
fin
ggsave(filename="viz/Coefficientplot_all_2FE.jpeg",
       plot=fin,
       pointsize = 24, 
       width = 14,
       height = 14,
       scale = 0.5,
       dpi = 800)




#--------------
  
  #Robustness Checks
  
#--------------
  # Lag 2 Year ----


#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_Lag2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_2_Lag2.tex")

#2FE####

# Add Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_stock + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_flow + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_1_Lag2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_2_Lag2.tex")


# Lag 3 Year ----

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_Lag3.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_2_Lag3.tex")

#2FE####

# Add Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_stock + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_flow + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_1_Lag3.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_2_Lag3.tex")




# 3 Year Average ----
rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")
df_main<- df_main[df_main$wb_dev==1,]

df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)

df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))

#Average Regression
df_mainZZ<-df_main


Level <-cut(df_mainZZ$year,seq(1982,2010,by=5),right=F)
id <- c("LRI_Total_W2_5","core","cash","LRI_Contracts","LRI_Dismiss","LRI_Worktime","LRI_Union_W2_5","LRI_Strike_W2_5",
        "Lag_log_UN_FDI_stock","Lag_log_UN_FDI_flow","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


df_mainZZZ<- aggregate(df_mainZZ[id],list(df_mainZZ$country,Level),mean,na.rm=T)

names(df_mainZZZ)
df_mainZZZZ<- df_mainZZZ %>% 
  mutate(country = Group.1,
         year = recode(
           Group.2,
           "[1982,1987)" = 1987, 
           "[1987,1992)" = 1992, 
           "[1992,1997)" = 1997,
           "[1997,2002)" = 2002, 
           "[2002,2007)" = 2007
         )) %>% dplyr::select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,16)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_5Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))

#4 Years

Level <-cut(df_mainZZ$year,seq(1982,2010,by=4),right=F)
id <- c("LRI_Total_W2_5","core","cash","LRI_Contracts","LRI_Dismiss","LRI_Worktime","LRI_Union_W2_5","LRI_Strike_W2_5",
        "Lag_log_UN_FDI_stock","Lag_log_UN_FDI_flow","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


df_mainZZZ<- aggregate(df_mainZZ[id],list(df_mainZZ$country,Level),mean,na.rm=T)

names(df_mainZZZ)
df_mainZZZZ<- df_mainZZZ %>% 
  mutate(country = Group.1,
         year = recode(
           Group.2,
           "[1982,1986)" = 1986, 
           "[1986,1990)" = 1990, 
           "[1990,1994)" = 1994,
           "[1994,1998)" = 1998, 
           "[1998,2002)" = 2002,
           "[2002,2006)" = 2006,
           "[2006,2010)" = 2010
         )) %>% dplyr::select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,16)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_4Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))

#3 Years

Level <-cut(df_mainZZ$year,seq(1982,2010,by=3),right=F)
id <- c("LRI_Total_W2_5","core","cash","LRI_Contracts","LRI_Dismiss","LRI_Worktime","LRI_Union_W2_5","LRI_Strike_W2_5",
        "Lag_log_UN_FDI_stock","Lag_log_UN_FDI_flow","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


df_mainZZZ<- aggregate(df_mainZZ[id],list(df_mainZZ$country,Level),mean,na.rm=T)

names(df_mainZZZ)
df_mainZZZZ<- df_mainZZZ %>% 
  mutate(country = Group.1,
         year = recode(
           Group.2,
           "[1982,1985)" = 1985, 
           "[1985,1988)" = 1988, 
           "[1988,1991)" = 1991,
           "[1991,1994)" = 1994, 
           "[1994,1997)" = 1997,
           "[1997,2000)" = 2000,
           "[2000,2003)" = 2003,
           "[2003,2006)" = 2006,
           "[2006,2009)" = 2009
         )) %>% dplyr::select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,4)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_3Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# Add PCSE
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",out="regression/final_regression_1_3Y_FE.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",out="regression/final_regression_2_3Y_FE.tex")


# Non-OECD

rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")

df_main<- df_main[df_main$oecd==0,]

df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)

df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_non_oecd.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_2_non_oecd.tex")

# Robustness: Cross Control Core, Cash ----

rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")
df_main<- df_main[df_main$wb_dev==1,]

df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main$Lag_core<- (df_main$Lag_LRI_Union_W2_5+df_main$Lag_LRI_Strike_W2_5)/2
df_main$Lag_cash<- (df_main$Lag_LRI_Contracts +df_main$Lag_LRI_Worktime+df_main$Lag_LRI_Dismiss)/3


df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)

df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl + Lag_Mos_labor_law
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl+ Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_Mos_labor_law 
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +  Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Add PCSE
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl + Lag_Mos_labor_law
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl+ Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_Mos_labor_law 
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +  Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1



ff4<-res.po1
ff5<-res.po2
ff6<-res.po3
ff12<-res.po9
ff13<-res.po10
ff14<-res.po11




stargazer(ff2,ff5,ff3,ff6,ff10,ff13,ff11,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Collective Rights","Outcome Rights","Collective Rights","Outcome Rights"),
          column.labels = c("FE", "2FE","FE", "2FE","FE", "2FE","FE", "2FE"),
          type = "latex",
          order=c(1,2,13,14,3:6,12,7:11,15),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Outcome Rights",
                                "Collective Rights",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",
          decimal.mark=".",out="regression/robust_regression_lagcashcore.tex")



# Robust Regression: Weights = 0 -----

# CBRT
form1   <- LRI_Total ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Add PCSE
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1

res.po4<- TFUN.NOE.LRIC.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.LRID.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.LRIW.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1

res.po7<- TFUN.NOE.LRIU.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.LRIS.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1

res.po12<- TFUN.NOE.LRIC.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po12,cluster="time")
ct.time1     <- coeftest(res.po12,vcov=res.BKtime1)
res.po12$vcov <- res.BKtime1

res.po13<- TFUN.NOE.LRID.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po13,cluster="time")
ct.time1     <- coeftest(res.po13,vcov=res.BKtime1)
res.po13$vcov <- res.BKtime1

res.po14<- TFUN.NOE.LRIW.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po14,cluster="time")
ct.time1     <- coeftest(res.po14,vcov=res.BKtime1)
res.po14$vcov <- res.BKtime1

res.po15<- TFUN.NOE.LRIU.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po15,cluster="time")
ct.time1     <- coeftest(res.po15,vcov=res.BKtime1)
res.po15$vcov <- res.BKtime1

res.po16<- TFUN.NOE.LRIS.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po16,cluster="time")
ct.time1     <- coeftest(res.po16,vcov=res.BKtime1)
res.po16$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16

stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_weight_0.tex")


# Robust Regression Fractional Logit----


# CBRT
form1   <- LRI_Total ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Core
form1   <- core ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIU
form1   <- LRI_Union ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIS
form1   <- LRI_Strike ~ Lag_log_UN_FDI_stock + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))


# CBRT_Flow
form1   <- LRI_Total ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIU_flow
form1   <- LRI_Union ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIS_flow
form1   <- LRI_Strike ~ Lag_log_UN_FDI_flow + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))


# Add PCSE


res.po1<- coeftest(TFUN.NOE.CBRT.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.CBRT.l1.wb.POLS1, type="HC0"))

res.po2<- coeftest(TFUN.NOE.CORE.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.CORE.l1.wb.POLS1, type="HC0"))

res.po3<- coeftest(TFUN.NOE.CASH.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.CASH.l1.wb.POLS1, type="HC0"))

res.po4<- coeftest(TFUN.NOE.LRIC.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.LRIC.l1.wb.POLS1, type="HC0"))

res.po5<- coeftest(TFUN.NOE.LRID.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.LRID.l1.wb.POLS1, type="HC0"))

res.po6<- coeftest(TFUN.NOE.LRIW.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.LRIW.l1.wb.POLS1, type="HC0"))

res.po7<- coeftest(TFUN.NOE.LRIU.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.LRIU.l1.wb.POLS1, type="HC0"))

res.po8<- coeftest(TFUN.NOE.LRIS.l1.wb.POLS1, vcov.=vcovHC(TFUN.NOE.LRIS.l1.wb.POLS1, type="HC0"))

res.po9<- coeftest(TFUN.NOE.CBRT.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.CBRT.l1.wb.POLS1_F, type="HC0"))

res.po10<- coeftest(TFUN.NOE.CORE.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.CORE.l1.wb.POLS1_F, type="HC0"))

res.po11<- coeftest(TFUN.NOE.CASH.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.CASH.l1.wb.POLS1_F, type="HC0"))

res.po12<- coeftest(TFUN.NOE.LRIC.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.LRIC.l1.wb.POLS1_F, type="HC0"))

res.po13<- coeftest(TFUN.NOE.LRID.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.LRID.l1.wb.POLS1_F, type="HC0"))

res.po14<- coeftest(TFUN.NOE.LRIW.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.LRIW.l1.wb.POLS1_F, type="HC0"))

res.po15<- coeftest(TFUN.NOE.LRIU.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.LRIU.l1.wb.POLS1_F, type="HC0"))

res.po16<- coeftest(TFUN.NOE.LRIS.l1.wb.POLS1_F, vcov.=vcovHC(TFUN.NOE.LRIS.l1.wb.POLS1_F, type="HC0"))

ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11
ff12<-res.po12
ff13<-res.po13
ff14<-res.po14
ff15<-res.po15
ff16<-res.po16


stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          column.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_fractional_logit.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          column.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock",
                                "Log FDI flow",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2_fractional_logit.tex")


# Robustness Check: Set Flow to 0/1 dummy----

df_main$Lag_FDI_flow01 <- df_main$Lag_log_UN_FDI_flow
df_main$Lag_FDI_flow01[df_main$Lag_FDI_flow01>0 ]<-1


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Add PCSE

res.po4<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po4,cluster="time")
ct.time1     <- coeftest(res.po4,vcov=res.BKtime1)
res.po4$vcov <- res.BKtime1

res.po5<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po5,cluster="time")
ct.time1     <- coeftest(res.po5,vcov=res.BKtime1)
res.po5$vcov <- res.BKtime1

res.po6<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po6,cluster="time")
ct.time1     <- coeftest(res.po6,vcov=res.BKtime1)
res.po6$vcov <- res.BKtime1




# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_FDI_flow01 + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE

res.po7<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po7,cluster="time")
ct.time1     <- coeftest(res.po7,vcov=res.BKtime1)
res.po7$vcov <- res.BKtime1

res.po8<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po8,cluster="time")
ct.time1     <- coeftest(res.po8,vcov=res.BKtime1)
res.po8$vcov <- res.BKtime1

res.po9<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1



ff4<-res.po4
ff5<-res.po5
ff6<-res.po6
ff7<-res.po7
ff8<-res.po8
ff9<-res.po9

stargazer(ff4,ff5,ff6,ff7,ff8,ff9,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome","Overall","Collective","Outcome"),
          column.labels = c("Time FE","Time FE","Time FE","2FE","2FE","2FE"),
          type = "latex",order=c(1:5,11,6:10,112),
          covariate.labels = c( "FDI flow Dummy",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_flow01.tex")







#--------------
  
  #ROBUSTNESS: Check for FDI/GDP & FDI/Capita
  
#--------------
  
  # Load Data----
rm(list=ls())
source("R/packages.R")
load("data/df_main.Rda")
## Subset for Developing Countries
df_main<- df_main[df_main$wb_dev==1,]

df_main$core<- (df_main$LRI_Union_W2_5+df_main$LRI_Strike_W2_5)/2
df_main$cash<- (df_main$LRI_Contracts +df_main$LRI_Worktime+df_main$LRI_Dismiss)/3

df_main = df_main %>% group_by(country,year) %>% mutate(logGDPpc_diff = logGDPpc-Lag_logGDPpc)
df_main = df_main %>% group_by(country) %>% mutate(Lag_logGDPpc_diff = dplyr::lag(logGDPpc_diff))

##  Model with FDI per Capita-----

#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)



# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1



res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11


# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock per Capita",
                                "Log FDI flow per Capita",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_1_PC.tex")


#2FE####

# Add Time and Two Way Fixed Effects ----

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pc + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1


ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock per Capita",
                                "Log FDI flow per Capita",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_2FE_1_PC.tex")


##  Model with FDI/GDP-----

#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)



# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1



res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1



ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11


# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock/GDP",
                                "Log FDI flow/GDP",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA ",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_1_GDP.tex")




#2FE####

# Add Time and Two Way Fixed Effects ----

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Add PCSE to Main Model ----
res.po1<- TFUN.NOE.CBRT.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po1,cluster="time")
ct.time1     <- coeftest(res.po1,vcov=res.BKtime1)
res.po1$vcov <- res.BKtime1

res.po2<- TFUN.NOE.CORE.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po2,cluster="time")
ct.time1     <- coeftest(res.po2,vcov=res.BKtime1)
res.po2$vcov <- res.BKtime1

res.po3<- TFUN.NOE.CASH.l1.wb.POLS1
res.BKtime1  <- vcovBK(x=res.po3,cluster="time")
ct.time1     <- coeftest(res.po3,vcov=res.BKtime1)
res.po3$vcov <- res.BKtime1


res.po9<- TFUN.NOE.CBRT.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po9,cluster="time")
ct.time1     <- coeftest(res.po9,vcov=res.BKtime1)
res.po9$vcov <- res.BKtime1

res.po10<- TFUN.NOE.CORE.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po10,cluster="time")
ct.time1     <- coeftest(res.po10,vcov=res.BKtime1)
res.po10$vcov <- res.BKtime1

res.po11<- TFUN.NOE.CASH.l1.wb.POLS1_F
res.BKtime1  <- vcovBK(x=res.po11,cluster="time")
ct.time1     <- coeftest(res.po11,vcov=res.BKtime1)
res.po11$vcov <- res.BKtime1


ff1<-res.po1
ff2<-res.po2
ff3<-res.po3
ff9<-res.po9
ff10<-res.po10
ff11<-res.po11

# Print Main Model----
stargazer(ff1,ff2,ff3,ff9,ff10,ff11,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for Developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Overall","Collective","Outcome","Overall","Collective","Outcome"),
          type = "latex",order=c(1:6,12,7:11,13),
          covariate.labels = c( "Log FDI stock/GDP",
                                "Log FDI flow/GDP",
                                "Log Trade",
                                "Log GDP Growth",
                                "Log Population", 
                                "Democracy",
                                "Conflict",
                                "ECA",
                                "LAC",
                                "MENA",
                                "SA",
                                "SSA",
                                "Constant"),
          style = "commadefault",decimal.mark=".",out="regression/final_regression_2FE_1_GDP.tex")




