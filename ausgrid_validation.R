###DOWNLOAD THE VALIDATION DATA SET utlising link provided in the README.md#############################################################
##########################################LANGUAGE: R, SOFTWARE: R Studio###############################################################
######################################LOADING DATA SET R STUDIO#########################################################################
install.packages("tidyverse")
install.packages("nasapower")
install.packages("fixest")
install.packages("Metrics")
library(tidyverse)
library(nasapower)
library(fixest)
library(Metrics)
################################Insert the local path to the downloaded Ausgrid dataset################################################
validation=read.csv("INSERT FILE PATH HERE",skip = 1)  #INSERT FILE PATH HERE
ausgridV=validation%>%pivot_longer(cols =X0.30:X0.00,
                                   names_to = "half_hour",
                                   values_to = "kWh")%>%pivot_wider(names_from = "Consumption.Category",
                                                                    values_from = "kWh")
ausgridV=ausgridV%>%mutate(clean_date=dmy(date))
ausgridV=ausgridV%>%mutate(YEAR=year(clean_date),MM=month(clean_date),DD=day(clean_date))
ausgridV=ausgridV%>%group_by(YEAR,MM,Customer,Generator.Capacity,Postcode)%>%summarise(Gross_Generation=sum(GG,na.rm = TRUE),Consumption=sum(GC,na.rm = TRUE),OffPeakC=sum(CL,na.rm = TRUE))
sydney_vector=c(2000,2006,2007,2008,2009,2010,2011,2015,2016,2017,2018,2019,
                2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031,
                2032,2033,2034,2035,2036,2037,2038,2039,2040,2041,2042,2043,
                2044,2045,2046,2047,2048,2049,2050,2052,2060,2061,2062,2063,
                2064,2065,2066,2067,2068,2069,2070,2071,2072,2073,2074,2075,
                2076,2077,2079,2080,2081,2082,2083,2084,2085,2086,2087,2088,
                2089,2090,2092,2093,2094,2095,2096,2097,2099,2100,2101,2102,
                2103,2104,2105,2106,2107,2108,2109,2110,2111,2112,2113,2114,
                2115,2116,2117,2118,2119,2120,2121,2122,2123,2125,2126,2127,
                2128,2129,2130,2131,2132,2133,2134,2135,2136,2137,2138,2139,
                2140,2141,2142,2143,2144,2145,2146,2147,2148,2150,2151,2152,
                2153,2154,2155,2156,2157,2158,2159,2160,2161,2162,2163,2164,
                2165,2166,2167,2168,2170,2171,2172,2173,2174,2175,2176,2177,
                2178,2179,2190,2191,2192,2193,2194,2195,2196,2197,2198,2199,
                2200,2203,2204,2205,2206,2207,2208,2209,2210,2211,2212,2213,
                2214,2216,2217,2218,2219,2220,2221,2222,2223,2224,2225,2226,
                2227,2228,2229,2230,2231,2232,2233,2234,2555,2556,2557,2558,
                2559,2560,2563,2564,2565,2566,2567,2568,2569,2570,2571,2572,
                2573,2574,2745,2747,2748,2749,2750,2752,2753,2754,2755,2756,
                2757,2758,2759,2760,2761,2762,2763,2765,2766,2767,2768,2769,
                2770,2773,2774,2776,2777,2778,2779,2780,2782,2783,2784,2785,2786,2787,2790)
CentralCoast_veector=c(2083,2250,2251,2256,2257,2258,2259,2260,2261,2262,2263)
NewCastleLakeMACC=c(2259,2264,2265,2267,2278,2280,2281,2282,2283,2284,2285,2286,
                    2287,2289,2290,2291,2292,2293,2294,2295,2296,2297,2298,2299,
                    2300,2302,2303,2304,2305,2306,2307,2308,2318,2322,2323)
ausgridV=ausgridV%>%mutate(region=case_when(Postcode%in%sydney_vector~"sydney",
                                            Postcode%in%CentralCoast_veector~"CentralCoast",
                                            Postcode%in%NewCastleLakeMACC~"NewCastleLake",
                                            TRUE~"unassigned"))
GHI_SYDNEY=get_power(community = "re",
                     temporal_api = "daily",
                     lonlat = c(151.16,-33.822082),
                     dates = c("2012-07-01","2013-06-30"),
                     pars = "ALLSKY_SFC_SW_DWN")
GHI_SYDNEY=GHI_SYDNEY%>%group_by(YEAR,MM)%>%summarise(GHI=sum(ALLSKY_SFC_SW_DWN))
GHI_SYDNEY$region="sydney"
GHI_COAST=get_power(community = "re",
                    temporal_api = "daily",
                    lonlat = c(151.356851,-33.306629),
                    dates = c("2012-07-01","2013-06-30"),
                    pars = "ALLSKY_SFC_SW_DWN")
GHI_COAST=GHI_COAST%>%group_by(YEAR,MM)%>%summarise(GHI=sum(ALLSKY_SFC_SW_DWN))
GHI_COAST$region="CentralCoast"
GHI_NewCastle=get_power(community = "re",
                        temporal_api = "daily",
                        lonlat = c(151.6911181,-32.9645640),
                        dates = c("2012-07-01","2013-06-30"),
                        pars = "ALLSKY_SFC_SW_DWN")
GHI_NewCastle=GHI_NewCastle%>%group_by(YEAR,MM)%>%summarise(GHI=sum(ALLSKY_SFC_SW_DWN))
GHI_NewCastle$region="NewCastleLake"
GHI_AUSTRALIA=bind_rows(GHI_SYDNEY,GHI_NewCastle,GHI_COAST)
ausgridV=ausgridV%>%left_join(GHI_AUSTRALIA,by=c("YEAR"="YEAR","region"="region","MM"="MM"))
ausgridF=ausgridV%>%mutate(GHI_C=GHI*Generator.Capacity)
ausgridF1=ausgridF%>%filter(!is.na(GHI_C))
ausgridF2=ausgridF1%>%filter(!Gross_Generation==0)
ausgridF3=ausgridF2%>%mutate(e_gross=0.80*(GHI_C))
ausgridF4=ausgridF3%>%filter(!is.na(e_gross))
AC1=ausgridF4$Gross_Generation
PC1=ausgridF4$e_gross
####################################MEAN AVERAGE ERROR##################################
ausMAE=mae(AC1,PC1)
print(ausMAE)
####################################RMSE###############################################
ausRMSE=rmse(AC1,PC1)
print(ausRMSE)
######################################MBE##############################################
ausBIAS=mean(PC1-AC1)
print(ausBIAS)
#####################################MEAN BIAS ERROR (%)###############################
ausMBE=(mean(PC1-AC1)/mean(AC1))*100
ausMBE1=(sum(PC1-AC1)/sum(AC1))*100
print(ausMBE)
print(ausMBE1)
###################################R2#################################################
rss=sum((AC1-PC1)^2)
tss=sum((AC1-mean(AC1))^2)
R2=1-(rss/tss)
print(R2)
#############################MAPE#######################################################
ausMAPE=mape(AC1,PC1)
print(ausMAPE)
#############################RMSE%#####################################################
nRMSERANGE=(sqrt(mean((AC1-PC1)^2))/(max(AC1)-min(AC1)))*100
print(nRMSERANGE)
############################nRMSE(MEAN)#######################################################
nRMSEMEAN=(sqrt(mean((AC1-PC1)^2))/mean(AC1))*100
print(nRMSEMEAN)
######################################nMAE(MEAN)#############################################
nMAE=(mae(AC1,PC1)/mean(AC1))*100
print(nMAE)
#####################################nMAE(RANGE)########################################
nMAERANGE=(mae(AC1,PC1)/(max(AC1)-min(AC1)))*100
print(nMAERANGE)
