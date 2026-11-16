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

NEWSATNASOLAR=read.csv("C:\\Users\\hp\\Documents\\empty_solar_panel.csv")
NEWSATNASOLAR=NEWSATNASOLAR%>%filter(!HouseholdID %in% c("HH_97","HH_98","HH_99"))%>%drop_na(Import,Export,Solar_Capacity_kw)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(grossGEN=0.80*(GHI*Solar_Capacity_kw))
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(SelfCon=grossGEN-Export)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(DemE=SelfCon+Import, SSR=SelfCon/(SelfCon+Import))
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
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(EDR=Export/(SelfCon+Import))
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(GDR=Import/(SelfCon+Import))
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(CO2save=(SelfCon+Export)*0.675)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(CO2emitt=Import*0.675)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(NCE=CO2emitt-CO2save)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PEDR=EDR*100)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PGDR=GDR*100)
NEWSATNASOLAR=NEWSATNASOLAR%>%mutate(PSSR=SSR*100)
