# ########################################################################################
# JHU Data Science - EDA C4W4
# Kevin Guillemot
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()


# ########################################################################################
# Libraries
# ########################################################################################

library(data.table)
library(reshape2)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(knitr)

###########################################################################################
# Question framing
###########################################################################################

#General question - What is the goal of the analysis?
# Q: Study fine particle matter pollution in the US 1999-2008

#Data question - Question reformulted in data analysis vocabulary
# Q: Are there changes in PM2.5 levels in the US 1999-2008

#Type of analysis - Descriptive/Exploratory/Inferential/Predictive
# A: Descriptive. Indentify situation.


###########################################################################################
# Raw Data
###########################################################################################

#Type of Analysis - Ideal Dataset 
# Descriptive - Entire population

#Dataset chosen
# Dataset: NEI_data.zip
# Dataset desc: PM.25 emission data from EPA website
# Data date: NA
# Source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# Dataset Date: 2017-03-27
# Sampling method: locations monitored by EPA
# Preprocessing: yes, but NA

#Import data
rawNEI <- readRDS("summarySCC_PM25.rds")
rawSCC <- readRDS("Source_Classification_Code.rds")


###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(rawNEI)
# Observations in rows 
# Feature in columns with descriptive names (not duplicated, no special characters except . and _)
# One table per type of variable (and per file)
# If multiple tables commum columns to perform joins
# Char should be made into factors when appropriate and should be descriptive (instead of "0" and "1")

#Data type normalization
# Desc: features to factors
rawNEI$fips<-factor(rawNEI$fips)
rawNEI$SCC<-factor(rawNEI$SCC)
rawNEI$Pollutant<-factor(rawNEI$Pollutant)
rawNEI$type<-factor(rawNEI$type)
rawNEI$year<-factor(rawNEI$year)

#Drop polluant, only 1 level
rawNEI<-select(rawNEI,-Pollutant)

#Missing Data
# Desc: None
sum(!complete.cases(rawNEI))



###########################################################################################
# EDA
###########################################################################################
#GGplot options
#Scatter: +geom_point()
#Scatter w color/factor: geom_point(aes(color=cyl))
#Lines: +geom_smooth()
#Regression total: +geom_smooth(method = "lm")
#Regression/factor: +geom_smooth(aes(color=cyl),method = "lm")
#Pannels/factor: +facet_grid(.~drv)
#Theme: +theme_bw()
#Title/Axis: +labs(title="hwy")+labs(x="displ",y="hwy")
#Points attributes: geom_point(color="steelblue",size=2,alpha=0.5)


#Factors count
# Desc: validate subsampling
prop.table(table(rawNEI$year))



#Emissions in the US
emissionsPerYear<-rawNEI %>% 
                  group_by(year) %>% 
                  summarise(totalEmissions=sum(Emissions))

plot(emissionsPerYear$year,emissionsPerYear$totalEmissions/10^6,
        type="l",
        main="PM2.5 emissions in the US",
        xlab="Year",
        ylab="Total Emissions in M tons")



#Emissions in Baltimore
baltimoreNEI<-filter(rawNEI,fips == "24510") %>% 
              group_by(year) %>% 
              summarise(totalEmissions=sum(Emissions))

plot(baltimoreNEI$year,baltimoreNEI$totalEmissions/10^6,
     type="l",
     main="PM2.5 emissions in Baltimore",
     xlab="Year",
     ylab="Total Emissions in M tons")



#Emissions in Baltimore by type
baltimoreTypeNEI<-filter(rawNEI,fips == "24510") %>% 
                  group_by(year,type) %>% 
                  summarise(totalEmissions=sum(Emissions))

ggplot(data = baltimoreTypeNEI, aes(x=year,y=totalEmissions,group=type))+
  geom_line(aes(color=type)) +
  labs(x="Year", y=expression("Total Emissions in tons")) + 
  labs(title="PM2.5 emissions in Baltimore")



#Coal Emissions in the US
coalSources<-filter(rawSCC,grepl("[Cc]oal",EI.Sector)) %>% select(SCC)
emissionsFromCoal<-filter(rawNEI,as.character(SCC) %in% coalSources$SCC) %>%
                    group_by(year) %>% 
                    summarise(totalEmissions=sum(Emissions))

ggplot(data = emissionsFromCoal, aes(year,totalEmissions/10^3))+
  geom_bar(stat="identity") +
  labs(x="Year", y=expression("Total Emissions in K tons")) + 
  labs(title="PM2.5 emissions from coal in the US")



#Vehicle Emissions in Baltimore
vehicleSources<-filter(rawSCC,grepl("[Vv]ehicle",EI.Sector)) %>% select(SCC)
emissionsVehicleBaltimore <- filter(rawNEI,fips == "24510") %>%
                              filter(as.character(SCC) %in% vehicleSources$SCC) %>%
                              group_by(year) %>% 
                              summarise(totalEmissions=sum(Emissions))
ggplot(data = emissionsVehicleBaltimore, aes(year,totalEmissions))+
  geom_bar(stat="identity") +
  labs(x="Year", y=expression("Total Emissions in tons")) + 
  labs(title="PM2.5 emissions from vehicles in Baltimore")


#Vehicle Emissions Baltimore vs Los Angeles County
vehicleSources<-filter(rawSCC,grepl("[Vv]ehicle",EI.Sector)) %>% select(SCC)
emissionsVehicleBaltLA <- filter(rawNEI,fips == "24510" | fips == "06037") %>%
                      filter(as.character(SCC) %in% vehicleSources$SCC) %>%
                      group_by(year,fips) %>% 
                      summarise(totalEmissions=sum(Emissions)) %>%
                      group_by(fips) %>%
                      mutate(normEmission=totalEmissions/totalEmissions[year=="1999"][1L])

ggplot(data = emissionsVehicleBaltLA, aes(x=year,y=normEmission,group=fips))+
  geom_line(aes(color=fips)) +
  labs(x="Year", y=expression("Emissions normalized to 1999 level")) + 
  labs(title="PM2.5 emissions from vehicles in Baltimore in Baltimore and LA")

