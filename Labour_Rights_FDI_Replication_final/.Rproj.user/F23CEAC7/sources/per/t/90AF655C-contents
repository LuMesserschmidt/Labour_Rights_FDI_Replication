# Title: Unravelling the ‘race to the bottom’ argument:  How does FDIaffect different types of labour rights?
# Authors: Nicole Janz, Luca Messerschmidt
# Date: 09/05/2021

# Necessary Files
# mastertablev10.RData (contains raw data from WorldBank)
# df_oecd (contains OECD data on FDI)

# Packages: 
#library(tidyverse)
#library(magrittr)
#library(ggiraph)
#library(stargazer) 
#library(foreign)
#library(ggthemes)
#library(readxl)
#library(bit64)
#library(data.table)
#library(plm)
#library(rms) 
#library(lmtest)
#library(pwr)


--------------
  #R Datamanagement
  
  --------------
  
  # Load Packages ----


rm(list=ls())

source("R/packages.R")


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
load("data/mastertablev10.Rdata")

df<- mastertablev10

names(df)

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




# Add logged OECD Sector FDI----
load("data/df_oecd.Rda")
df<- merge(df,df_oecd, by=c("country", "year"))
names(df)

tobeused  <- c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
               "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","CIRI_WORKER","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
               "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log","log_UN_FDI_flow_pgdp") #these are the original logged FDI/GDP variables without any lag

tobelagged <- c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log","log_UN_FDI_flow_pgdp") #these are the original logged FDI/GDP variables without any lag


df_main<- df[,tobeused]

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

df<- merge(df_main,df_main1, by=c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","CIRI_WORKER","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                                  "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services", "OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                                  "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log", "log_UN_FDI_flow_pgdp"))

df2<- merge(df,df_main2, by=c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","CIRI_WORKER","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                              "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                              "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log", "log_UN_FDI_flow_pgdp"))

df3<- merge(df2,df_main3, by=c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","CIRI_WORKER","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                               "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                               "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log", "log_UN_FDI_flow_pgdp"))

df4<- merge(df3,df_main4, by=c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","CIRI_WORKER","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                               "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                               "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log", "log_UN_FDI_flow_pgdp"))

df_5<- merge(df4,df_main5, by=c("LRI_Union_W1_5", "LRI_Total_W1_5","LRI_Union_W2_5", "LRI_Total_W2_5","LRI_Union_W3", "LRI_Total_W3" ,"LRI_Strike_W1_5","LRI_Strike_W2_5","LRI_Strike_W3","LRI_Union_W3_5", "LRI_Total_W3_5","LRI_Strike_W3_5","CIRI_WORKER","country", "year", "wb_region", "oecd", "wb_low","wb_middle","wb_high","wb_dev", "Mos_labor", "Mos_labor_law", "Mos_labor_prac", "LRI_Total", "LRI_Contracts", "LRI_Worktime", "LRI_Dismiss", "LRI_Union", "LRI_Strike","lognonnegUS_fdi_totalpGDP","lognonnegUS_fdi_petrolpGDP","lognonnegUS_fdi_foodpGDP","lognonnegUS_fdi_chemicalpGDP","lognonnegUS_fdi_prim_fab_metalpGDP","lognonnegUS_fdi_machinerypGDP","lognonnegUS_fdi_electricalpGDP","lognonnegUS_fdi_transportpGDP", 
                                "lognonnegUS_fdi_whole_tradepGDP","lognonnegUS_fdi_depositorypGDP","lognonnegUS_fdi_miningpGDP", "log_UN_FDI_stock_pgdp", "lognonnegUS_fdi_total_manufpGDP", "lognonnegUS_fdi_servicespGDP","polity2", "confl","logtrade","logpopulation","logGDPpc","log_Employment_Industry", "log_Employment_Services","OECD_FDI_PETROL_log","OECD_FDI_MANUF_total_log","OECD_FDI_food_log","OECD_FDI_textile_log","OECD_FDI_print_log","OECD_FDI_refpetrol_log","OECD_FDI_chemical_log","OECD_FDI_plastic_log","OECD_FDI_metal_log","OECD_FDI_machinery_log","OECD_FDI_comp_electronic_log","OECD_FDI_precinstruments_log","OECD_FDI_manuf_transport_log","OECD_FDI_utilities_log",
                                "OECD_FDI_construction_log","OECD_FDI_services_log","OECD_FDI_total_traderepair_log","OECD_FDI_hotelrestaur_log","OECD_FDI_transp_log","OECD_FDI_post_tele_log","OECD_FDI_total_finance_log","OECD_FDI_real_rent_bus.act_log","OECD_FDI_rent_mach_log","OECD_FDI_prof_scient_comp_log","OECD_FDI_prof_scient_research_log","OECD_FDI_prof_scient_other_log","OECD_FDI_other_services_log","OECD_FDI_unallocated_log","OECD_FDI_TOTAL_log", "log_UN_FDI_flow_pgdp"))

df_main_lag<- df_5


df_main_lag<- merge(df_main_lag, df_main_3,by=c("year","country"))

df_main<-df_main_lag


# Save data as df_main ----
save(df_main,file="data/df_main.Rda")






--------------
  
  #Descriptive Statistics
  
  --------------
  
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
  xlab("")+
  ylab("")+
  theme_bw()
f1<-f1+labs(shape='Region') 
f1<- f1 + theme(legend.position = "bottom")
f1<- f1 + ylim(0.25, 0.75)
f1


f2<- ggplot(df_descriptive_3.3, aes(x=LRI_Cash, y=LRI_Core,color=wb_region))+
  geom_point()+
  ggtitle("By Region")+
  scale_linetype_manual(values=c("twodash","longdash", "solid", "12345678","dotted", "F1"),name="Region")+
  xlab("")+
  ylab("")+
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
  xlab("")+
  ylab("")+
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

## Of all Variables
df_main<- df_main[df_main$wb_dev==1,]

class(df_main)

df_main <- as.data.frame(df_main)

stats_summary<- c(df_main[,cbind("Mos_labor","Mos_labor_law", "Mos_labor_prac","LRI_Total_W2_5","LRI_Contracts", "LRI_Worktime", "LRI_Dismiss","LRI_Union_W2_5","LRI_Strike_W2_5", "log_UN_FDI_flow_pgdp","log_UN_FDI_stock_pgdp","logtrade","logGDPpc_diff", "logpopulation", "polity2","wb_region", "confl")]) 
stats_summary <- as.data.frame(stats_summary)
stargazer(stats_summary, type="html", float=F,covariate.labels = c("FACB","FACB de jure", "FACB Practice", "CBR-LRI Total", 
                                                                   "CBR-LRI Contracts","CBR-LRI Worktime","CBR-LRI Dismiss","CBR-LRI Union","CBR-LRI Strike",
                                                                   "Log UN Total FDI Flow",
                                                                   "Log UN Total FDI Stock",
                                                                   "Log Trade",
                                                                   "Growth",
                                                                   "Log Population", 
                                                                   "Democracy", 
                                                                   "Conflict"),
          title = "Summary Statistics", style = "commadefault",decimal.mark=".",out="viz/summary_statistics.tex")

names(df_main)

## Statistics Summary of Model

df_main_summary <- na.omit(df_main[,cbind("LRI_Total_W2_5","LRI_Contracts", "LRI_Worktime", "LRI_Dismiss","LRI_Union_W2_5","LRI_Strike_W2_5","core","cash","log_UN_FDI_flow_pgdp","log_UN_FDI_stock_pgdp","logtrade","logGDPpc_diff", "logpopulation", "polity2","wb_region", "confl")]) 
df_main_summary<- as.data.frame(df_main_summary)


stargazer(df_main_summary, type="html", float=F,covariate.labels = c("Overall Rights", "Contracts","Worktime","Dismissal","Representation","Industrial Action","Collective Rights","Outcome Rights",
                                                                     "Log FDI flow/GDP","Log FDI stock/GDP",
                                                                     "Log Trade",
                                                                     "Growth",
                                                                     "Log Population", 
                                                                     "Democracy", 
                                                                     "Conflict"),
          title = "Summary Statistics", style = "commadefault",decimal.mark=".",out="viz/summary_statistics_model.tex")



# Correlation Matrix (TABLE A3: Correlation Table of Variables)----
## Matrix 1: Correlation with other Labour Rights Measurements
library(corrplot)
names(df_main)

##Subset 
y<- c("LRI_Total_W2_5",
      "Mos_labor",
      "Mos_labor_law",
      "Mos_labor_prac",
      "LRI_Contracts", 
      "LRI_Worktime", 
      "LRI_Dismiss",
      "LRI_Union_W2_5",
      "LRI_Strike_W2_5",
      "CIRI_WORKER",
      "core","cash") 
df_main_cor <- df_main[,cbind(y)]

## Rename
df_main_cor$"Overall Rights"<-df_main_cor$LRI_Total_W2_5
df_main_cor$Contracts<-df_main_cor$LRI_Contracts
df_main_cor$Worktime<-df_main_cor$LRI_Worktime
df_main_cor$Dismissal<-df_main_cor$LRI_Dismiss
df_main_cor$Representation<-df_main_cor$LRI_Union_W2_5
df_main_cor$"Industrial Action"<-df_main_cor$LRI_Strike_W2_5
df_main_cor$"Collective Rights"<-df_main_cor$core
df_main_cor$"Outcome Rights"<-df_main_cor$cash
df_main_cor$"Mosley FACB Overall"<-df_main_cor$Mos_labor
df_main_cor$"Mosley FACB Law"<-df_main_cor$Mos_labor_law
df_main_cor$"Mosley FACB Practice"<-df_main_cor$Mos_labor_prac
df_main_cor$"CIRI worker rights"<-df_main_cor$CIRI_WORKER
df_main_cor1<- df_main_cor[,-c(1:12)]

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
A<-corstars(df_main_cor)

library(xtable)
xtable(A)

## Control with cor() function 
M <- cor(df_main_cor1,use="pairwise.complete.obs")

C<-head(round(M,2))

M
C

#Matrix 2: All variables used in the main models (pairwise complete correlation)
df_main_cor <- na.omit(df_main[,cbind("LRI_Total_W2_5","LRI_Contracts", "LRI_Worktime", "LRI_Dismiss","LRI_Union_W2_5","LRI_Strike_W2_5","core","cash","log_UN_FDI_stock_pgdp","logtrade","logGDPpc_diff", "logpopulation", "polity2","confl")]) 

##Rename
df_main_cor$"Overall Rights"<-df_main_cor$LRI_Total_W2_5
df_main_cor$Contracts<-df_main_cor$LRI_Contracts
df_main_cor$Worktime<-df_main_cor$LRI_Worktime
df_main_cor$Dismissal<-df_main_cor$LRI_Dismiss
df_main_cor$Representation<-df_main_cor$LRI_Union_W2_5
df_main_cor$"Industrial Action"<-df_main_cor$LRI_Strike_W2_5
df_main_cor$"Collective Rights"<-df_main_cor$core
df_main_cor$"Outcome Rights"<-df_main_cor$cash
df_main_cor$"FDI stock/GDP"<-df_main_cor$log_UN_FDI_stock_pgdp
df_main_cor$"FDI flow/GDP"<-df_main_cor$log_UN_FDI_flow_pgdp
df_main_cor$"Trade"<-df_main_cor$logtrade
df_main_cor$"Growth"<-df_main_cor$logGDPpc_diff
df_main_cor$"Population"<-df_main_cor$logpopulation
df_main_cor$"Democracy"<-df_main_cor$polity2
df_main_cor$Conflict<-df_main_cor$confl

df_main_cor2<- df_main_cor[,-c(1:14)]


## Calculate Correlation
A<-corstars(df_main_cor2)

library(xtable)
xtable(A)

M <- cor(df_main_cor2,use="pairwise.complete.obs")

C<-head(round(M,2))


--------------
  
  #Regression Analysis
  
  --------------
  
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
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp 
fixed <- plm(form1 ,model="within",index=c("country","year"),data=df_main)
random <- plm(form1 ,model="random",index=c("country","year"),data=df_main)
pooled <- plm(form1 ,model="pooling",index=c("country","year"),data=df_main)

phtest(fixed,random)
## Result: p<0,05 FE and NOT RE

## Testing for time FE: Lagrange Multiplier Test - time effects (Breusch-Pagan) for unbalanced panels

form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + factor(year)
fixed.time <- plm(form1 ,model="within",index=c("country","year"),data=df_main)
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))
## Time FE needed

# Testing for random effects: Breusch-Pagan Lagrange multiplier (LM)
plmtest(pooled, type=c("bp"))
## Better use random effects

# Testing for serial correlation

pbgtest(fixed)
## Serial Correlation Exists

# Dickey-Fuller Test for stochastic trends
Panel.set <- plm.data(df_main, index = c("country", "year"))

library(tseries)
adf.test(Panel.set$y, k=2)

## Unit roots present --> Better use FD model

# Testing for heteroskedasticity

library(lmtest)
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp +factor(country)
bptest(form1, data = df_main, studentize=F)
## Panel Corrected Standard Errors needed! 
## vcovHC– function: "arellano" - both heteroskedasticityand serial correlation. Recommended for fixed effects


## Heteroskedasticity Detected --> Robust Covariance Matrix needed

## Difference accross countries: simple OLS not appropriate

# Tests for Poolability

form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade + Lag_logGDPpc + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
znp <- pvcm(form1 ,model="within",data=df_main)
zplm <- plm(form1 ,model="within",data=df_main)

## Main Model-----

#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_1.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
          style = "commadefault",decimal.mark=".",out="regression/final_regression_FE_2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
          type = "latex",order=c(1:5,11,6:10,12),
          covariate.labels = c( "Log FDI stock/GDP",
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
          covariate.labels = c( "Log FDI flow/GDP",
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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock_pgdp"] <- " "
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

CC_stock<- b + scale_y_continuous(limits=c(-0.01, 0.015))

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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow_pgdp"] <- " "
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
CC_flow<- b + scale_y_continuous(limits=c(-0.01, 0.015))
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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock_pgdp"] <- " "
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
CC2_stock<- a + scale_y_continuous(limits=c(-0.01, 0.015))

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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow_pgdp"] <- " "
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
CC2_flow<- a + scale_y_continuous(limits=c(-0.01, 0.015))
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
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",decimal.mark=".",out="regression/final_regression_2FE_1.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock_pgdp"] <- " "
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
CC_stock<- a + scale_y_continuous(limits=c(-0.01, 0.01))

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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow_pgdp"] <- " "
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
CC_flow<- a + scale_y_continuous(limits=c(-0.01, 0.01))
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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_stock_pgdp"] <- " "
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
CC2_stock<- a + scale_y_continuous(limits=c(-0.01, 0.01))

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
allModelFrame$Variable[allModelFrame$Variable=="Lag_log_UN_FDI_flow_pgdp"] <- " "
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
CC2_flow<- a + scale_y_continuous(limits=c(-0.01, 0.01))
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




--------------
  
  #Robustness Checks
  
  --------------
  # Lag 2 Year ----


#  Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_Lag2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_2_Lag2.tex")

#2FE####

# Add Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag2_log_UN_FDI_flow_pgdp + Lag2_logtrade +Lag2_logGDPpc_diff + Lag2_logpopulation + Lag2_polity2 + wb_region + Lag2_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_1_Lag2.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_2_Lag2.tex")


# Lag 3 Year ----

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag2_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_Lag3.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_2_Lag3.tex")

#2FE####

# Add Time and Two Way Fixed Effects ----


# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Main Model: Pooled OLS (TABLE2: FDI stock and flow, TABLE3: Five Categories) ----
# CBRT
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_stock_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag3_log_UN_FDI_flow_pgdp + Lag3_logtrade +Lag3_logGDPpc_diff + Lag3_logpopulation + Lag3_polity2 + wb_region + Lag3_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2FE_1_Lag3.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
        "Lag_log_UN_FDI_stock_pgdp","Lag_log_UN_FDI_flow_pgdp","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


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
         )) %>% select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,16)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_5Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))

#4 Years

Level <-cut(df_mainZZ$year,seq(1982,2010,by=4),right=F)
id <- c("LRI_Total_W2_5","core","cash","LRI_Contracts","LRI_Dismiss","LRI_Worktime","LRI_Union_W2_5","LRI_Strike_W2_5",
        "Lag_log_UN_FDI_stock_pgdp","Lag_log_UN_FDI_flow_pgdp","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


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
         )) %>% select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,16)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_4Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))

#3 Years

Level <-cut(df_mainZZ$year,seq(1982,2010,by=3),right=F)
id <- c("LRI_Total_W2_5","core","cash","LRI_Contracts","LRI_Dismiss","LRI_Worktime","LRI_Union_W2_5","LRI_Strike_W2_5",
        "Lag_log_UN_FDI_stock_pgdp","Lag_log_UN_FDI_flow_pgdp","Lag_logtrade","Lag_logGDPpc_diff","Lag_logpopulation","Lag_polity2","Lag_confl")


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
         )) %>% select (3:19)

df_mainZZZZ[is.na (df_mainZZZZ)] = NA
#df_mainZZZZ$country<-as.character(df_mainZZZZ$country)

df_mainwb<-df_main[,c(2,16)] 
#df_mainwb$country<-as.character(df_mainwb$country)

df_mainwb<- distinct(df_mainwb) 

df_main_3Y<- left_join(df_mainZZZZ,df_mainwb, by=c("country"))

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main_3Y)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",out="regression/final_regression_1_3Y_FE.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# CBRT
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_FE_1_non_oecd.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          dep.var.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl + Lag_Mos_labor_law
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl+ Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_Mos_labor_law 
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +  Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_core 
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
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl + Lag_Mos_labor_law
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl+ Lag_core 
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# CBRT_Flow
form1   <- LRI_Total_W2_5 ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_Mos_labor_law 
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +  Lag_cash
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect = "twoways",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl +Lag_core 
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
          covariate.labels = c( "Log FDI stock/GDP",
                                "Log FDI flow/GDP",
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
form1   <- LRI_Total ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIU
form1   <- LRI_Union ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIS
form1   <- LRI_Strike ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# CBRT_Flow
form1   <- LRI_Total ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIU_flow
form1   <- LRI_Union ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- plm(form1 ,model="within",effect="time",index=c("country","year"),data=df_main)

# LRIS_flow
form1   <- LRI_Strike ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_weight_0.tex")


# Robust Regression Fractional Logit----


# CBRT
form1   <- LRI_Total ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Core
form1   <- core ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Cash
form1   <- cash ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIC
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))
# LRID

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIW
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIU
form1   <- LRI_Union ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIS
form1   <- LRI_Strike ~ Lag_log_UN_FDI_stock_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIS.l1.wb.POLS1 <- glm(form1, data = df_main, family = quasibinomial('logit'))


# CBRT_Flow
form1   <- LRI_Total ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CBRT.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))


# Core_Flow
form1   <- core ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CORE.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# Cash_Flow
form1   <- cash ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.CASH.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIC_flow
form1   <- LRI_Contracts ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIC.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))
# LRID_flow

form1   <- LRI_Dismiss ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRID.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIW_flow
form1   <- LRI_Worktime ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIW.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIU_flow
form1   <- LRI_Union ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
TFUN.NOE.LRIU.l1.wb.POLS1_F <- glm(form1, data = df_main, family = quasibinomial('logit'))

# LRIS_flow
form1   <- LRI_Strike ~ Lag_log_UN_FDI_flow_pgdp + Lag_logtrade +Lag_logGDPpc_diff + Lag_logpopulation + Lag_polity2 + wb_region + Lag_confl
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_fractional_logit.tex")

stargazer(ff7,ff8,ff4,ff5,ff6,ff15,ff16,ff12,ff13,ff14,
          star.cutoffs = c(0.1, 0.05, 0.01),
          report = c("vc*s"), se = NULL , float=F,
          title = "Pooled OLS Regression on Labour Rights Indices for developing Countries with Panel Corrected SE, Lag = 1 Year",
          column.labels=c("Representation","Industrial Action","Contracts","Worktime","Dismissal","Representation","Industrial Action","Contracts","Worktime","Dismissal"),
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
          style = "commadefault",decimal.mark=".",out="regression/robust_regression_2_fractional_logit.tex")


# Robustness Check: Set Flow to 0/1 dummy----

df_main$Lag_FDI_flow01 <- df_main$Lag_log_UN_FDI_flow_pgdp
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










