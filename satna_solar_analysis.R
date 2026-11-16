#########################################SATNA SOLAR PANEL ANALYSIS######################################################################
##########################################LANGUAGE: R, SOFTWARE: R Studio###############################################################
install.packages("tidyverse")
install.packages("nasapower")
install.packages("fixest")
install.packages("Metrics")
library(tidyverse)
library(nasapower)
library(fixest)
library(Metrics)
################################Insert the local path to the downloaded satna dataset#########################################################
NEWSATNASOLAR=read.csv("INSERT FILE PATH HERE") #INSERT FILE PATH HERE
NEWSATNASOLAR=NEWSATNASOLAR%>%filter(!HouseholdID %in% c("HH_97","HH_98","HH_99"))%>%drop_na(Import,Export,Solar_Capacity_kw)
###########################################Gross Generation Estimation########################################################################
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(grossGEN=0.80*(GHI*Solar_Capacity_kw)) 
###########################################Self Consumption Extraction#########################################################################
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(SelfCon=grossGEN-Export)       #SelfCon=Self Consumption
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(DemE=SelfCon+Import, SSR=SelfCon/(SelfCon+Import)) #DemE=estimated total demand #SSR=Self Sufficiecy Ratio
##########################################Ambient Temperature Extraction######################################################################
Satna_Temp_daily=get_power(community = "re",
                     temporal_api = "hourly",
                     lonlat =c(80.86,24.56),
                     dates=c("2024-06-01","2026-05-31"),
                     pars = "T2m")
Temp_SATNA=Satna_Temp_daily%>%group_by(DY,MO,YEAR)%>%summarise(TEMP=sum(T2M))
Temp_SATNA1=Temp_SATNA%>%group_by(DY,MO,YEAR)%>%summarise(TEMP1=TEMP/24)
Temp_SATNA2=Temp_SATNA1%>%group_by(YEAR,MO)%>%summarise(TEMP2=mean(TEMP1))
Temp_SATNA2$month=seq(from=as.Date("2024-06-01"),to=as.Date("2026-05-01"),by="month")
Temp_SATNA2=Temp_SATNA2%>%mutate(month=format(month,"%d-%m-%Y"))
NEWSATNASOLAR=NEWSATNASOLAR%>%left_join(Temp_SATNA2,by="month")
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(EDR=Export/(SelfCon+Import)) #####EDR= Export-to-Demand Ratio(EDR)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(GDR=Import/(SelfCon+Import)) #####GDR= Grid Dependency Ratio (GDR)
####################################################CO2save##############################################################################
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(CO2save=(SelfCon+Export)*0.675)    ####Emission Factor=0.675
####################################################C02emitt#############################################################################
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(CO2emitt=Import*0.675)
#############################################NET CARBON EMISSION (NCE)####################################################################
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(NCE=CO2emitt-CO2save) #NCE=NET CARBON EMISSION
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PEDR=EDR*100)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PGDR=GDR*100)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PSSR=SSR*100)
plot_data3=NEWSATNASOLAR%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y"))%>%filter(PGDR>0,PGDR<100)
plot_data4=plot_data3%>%filter(PEDR>0,PEDR<100)
plot_data5=plot_data4%>%filter(PSSR>0,PSSR<100)
monthlyplot1=plot_data5%>%mutate(month1=as.Date(month, format="%Y-%m"))%>%group_by(month)%>%summarise(meanPGDR=mean(PGDR))
monthlyplot1=monthlyplot1%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y"))
monthlyplot2=plot_data5%>%mutate(month1=as.Date(month, format="%Y-%m"))%>%group_by(month)%>%summarise(meanPEDR=mean(PEDR))
monthlyplot2=monthlyplot2%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y"))
monthlyplot3=plot_data5%>%mutate(month1=as.Date(month, format="%Y-%m"))%>%group_by(month)%>%summarise(meanPSSR=mean(PSSR))
monthlyplot3=monthlyplot3%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y"))
monthlyplot4=plot_data5%>%mutate(month1=as.Date(month, format="%Y-%m"))%>%group_by(month)%>%summarise(meanNCE=mean(NCE))
monthlyplot4=monthlyplot4%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y"))
monthlyplot5=plot_data5%>%mutate(month1=as.Date(month, format="%Y-%m"))%>%group_by(month)%>%summarise(meanPSSR=mean(PSSR),meanPEDR=mean(PEDR),meanPGDR=mean(PGDR))
monthlyplot5=monthlyplot5%>%mutate(MONTH1=as.Date(month,format= "%d-%m-%Y")) 
long_plot=monthlyplot5%>%pivot_longer(cols = c(meanPSSR,meanPGDR,meanPEDR),
                                      names_to = "RATIO",
                                      values_to = "score")
################################################ Fig1 Panel (A) ###########################################################################
plotPGDR=ggplot(data=monthlyplot1, aes(x=MONTH1,y=meanPGDR))+
  geom_point(color="black")+
  geom_line()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  labs(
    title = "(A) GRID DEPENDENCY RATIO (GDR)",
    x= "months",
    y= "(%) percentage"
  )+
  theme_classic()
#############################################Fig1 Panel (B)#################################################################################
plotEDR=ggplot(data = monthlyplot2, aes(x=MONTH1,y=meanPEDR))+
  geom_point(color="black")+
  geom_line()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  labs(
    title = "(B) EXPORT-TO-DEMAND RATIO (EDR)",
    x= "months",
    y= "(%) percentage")+
  theme_classic()
######################################Fig1 Panel (C)########################################################################################
plotPSSR=ggplot(data = monthlyplot3, aes(x=MONTH1,y=meanPSSR))+
  geom_point(color="black")+
  geom_line()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  labs(
    title = " (C) SELF SUFFICIENCY RATIO (SSR)",
    x= "months",
    y= "(%) percentage")+
  theme_classic()
#####################################Fig3 Panel (a)#######################################################################################
plotNCE=ggplot(data = monthlyplot4, aes(x=MONTH1,y=meanNCE))+
  geom_point(color="black")+
  geom_line()+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  labs(
    title = " (a) Net Carbon emission (NCE)",
    x= "months",
    y= "Carbon Saved (Per KG)"
  )+
  theme_classic()
 #####################################Fig2 Panel (a)####################################################################################### 
glitGDR=ggplot(data = plot_data5, aes(x=MONTH1,y=PGDR))+
  geom_jitter(width = 0.5, height=0, alpha=0.2,color="grey30")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  geom_smooth(aes(group = 1), method = "gam", color="red")+
  labs(
    title = "(a) GRID DEPENDENCY RATIO(GDR)",
    x= "months",
    y= "(%) percentage"
  )+
  theme_classic()
###################################Fig2 Panel (b)######################################################################################
glitPEDR=ggplot(data = plot_data5, aes(x=MONTH1,y=PEDR))+
  geom_jitter(width = 0.5, height=0, alpha=0.2,color="grey30")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  geom_smooth(aes(group = 1), method = "gam", color="red")+
  labs(
    title = "(b) EXPORT-TO-DEMAND RATIO(EDR)",
    x= "months",
    y= "(%) percentage")+
  theme_classic()
################################Fig2 Panel (c)########################################################################################
glitPSSR=ggplot(data = plot_data5, aes(x=MONTH1,y=PSSR))+
  geom_jitter(width = 0.5, height=0, alpha=0.2,color="grey30")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  geom_smooth(aes(group = 1), method = "gam", color="red")+
  labs(
    title = "(c) SELF SUFFICIENCY RATIO (SSR)",
    x= "months",
    y= "(%) percentage")+
  theme_classic()
############################Fig3 panel(b)############################################################################################
glitNCE=ggplot(data = plot_data5, aes(x=MONTH1,y=NCE))+
  geom_jitter(width = 0.5, height=0, alpha=0.2,color="grey30")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  geom_smooth(aes(group = 1), method = "gam", color="red")+
  labs(
    title = " (b) Net Carbon emission (NCE)",
    x= "months",
    y= "Carbon Saved (Per KG)"
  )+
  theme_classic()
#########################Fig4#######################################################################################################
multiplot=ggplot(data = long_plot,aes(x=MONTH1,y=score, color= RATIO,linetype = RATIO))+
  geom_point(color="black")+
  geom_line(linewidth = 0.8)+
  scale_colour_brewer(palette = "Dark2",name = "RATIO",
                      labels=c("meanPSSR"="SSR","meanPGDR"="GDR","meanPEDR"="EDR"))+
  scale_linetype_manual(values = c("meanPSSR"="longdash","meanPGDR"="dotted","meanPEDR"="solid"),
                        labels = c("meanPSSR"="SSR","meanPGDR"="GDR","meanPEDR"="EDR"))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 months")+
  labs(
    title = "SSR,GDR & EDR",
    x= "months",
    y= "(%) percentage"
  )+
  theme_classic()
#########################OVERALL SELF SUFFICIENCY RATIO(SSR) OF ENTIRE PANEL##############################################################
OSSR=sum(NEWSATNASOLAR$SelfCon)/(sum(NEWSATNASOLAR$SelfCon)+sum(NEWSATNASOLAR$Import))*100
print(OSSR)
########################OVERALL GRID DEPENDECT RATIO (GDR) OF ENTIRE PANEL##############################################################
OGDR=sum(NEWSATNASOLAR$Import)/(sum(NEWSATNASOLAR$SelfCon)+sum(NEWSATNASOLAR$Import))*100
print(OGDR)
#######################OVERALL EXPORT-TO-DEMAND RATIO (EDR) OF ENTIRE PANEL##############################################################
OEDR=sum(NEWSATNASOLAR$Export)/(sum(NEWSATNASOLAR$SelfCon)+sum(NEWSATNASOLAR$Import))*100
print(OEDR)
#######################ADJUSTED SUBSIDY COST OF CARBON ABATEMENT (ASCCA)##################################################################
ASSC=NEWSATNASOLAR%>%group_by(HouseholdID,Solar_Capacity_kw)%>%summarise(CO2s=mean(CO2save,na.rm=TRUE))%>%mutate(subsidy=case_when(Solar_Capacity_kw>=1 & Solar_Capacity_kw<2~30000,
                                       Solar_Capacity_kw>=2 & Solar_Capacity_kw<3~60000,
                                       Solar_Capacity_kw>=3~78000))
ASSC=ASSC%>%mutate(ASCCA=(subsidy/sum((CO2s*12)*(1-0.006)^(1:20)))*1000)
#############################OVERALL MEAN OF ASCCA FOR THE PANEL##########################################################################
meanACSSC=mean(ASSC$ASCCA)
print(meanACSSC)
#############################OVERALL MEAN OF NET CARBON EMISION (NCE)#####################################################################
mNCE=mean(NEWSATNASOLAR$NCE)
print(mNCE)
#############################ONE WAY FIXED EFFECT MODEL (WG)##############################################################################
selfc=feols(SelfCon~TEMP2|HouseholdID,data = NEWSATNASOLAR,cluster = ~HouseholdID)
imp=feols(Import~TEMP2|HouseholdID,data = NEWSATNASOLAR,cluster = ~HouseholdID)
nce_model=feols(NCE~TEMP2|HouseholdID,data = NEWSATNASOLAR,cluster = ~HouseholdID)
etable(selfc,imp,nce_model)
