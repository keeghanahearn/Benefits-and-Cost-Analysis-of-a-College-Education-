# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).


#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())


setwd("C:/Users/kahea/Desktop/Homework/2021/Econ 103A/Paper/Actual Data")
library(ipumsr)
library(ggplot2)
library(knitr)
library(stargazer)
library(data.table)
library(formattable)
library(dplyr)
library(tidyr)
library(sandwich)
library(car)
library(plyr)
library(bit64)
library(doBy)
library(plm)
library(lfe)


## turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#==============================================================================
#   2. Data section
#==============================================================================
ddi <- read_ipums_ddi("usa_00005.xml")
data <- read_ipums_micro(ddi)

#Parent subset narrowed down through age of workforce
subsetAge = subset(data, AGE>15 & AGE<65 & MULTYEAR>2012 & MULTYEAR<2020)

#Recoding of Independent Variables

#Race
#subsetAge$WHITE <-recode(subsetAge$RACE, "1=1; else=0")
#might have to put for white people 1-RACE or something
subsetAge$ASIAN <-recode(subsetAge$RACE, "4:6=1; else=0")
subsetAge$BLACK <-recode(subsetAge$RACE, "2=1; else=0")
#subsetAge$HISPANIC <-recode(subsetAge$RACE, "0=0; 1:4=1")
subsetAge$OTHER <-recode(subsetAge$RACE, "7:9=1; else=0")

#Marital Status
subsetAge$MARRIED <-recode(subsetAge$MARST, "1:2=1; 3:6=0")

#Sex
subsetAge$MALE <-recode(subsetAge$SEX, "1=1; 2=0")

#Annual Wage
subsetAge$INCWAGE <- subsetAge$INCWAGE

#Age
subsetAge$AGE



#Major
subsetAge$BUSINESS <-recode(subsetAge$DEGFIELD, "62=1; else=0")
subsetAge$MED <-recode(subsetAge$DEGFIELD, "61=1; else=0")
subsetAge$EDUCATION <-recode(subsetAge$DEGFIELD, "23=1; else=0")
subsetAge$ENGINEERING <-recode(subsetAge$DEGFIELD, "24=1; else=0")
subsetAge$SOCIAL <-recode(subsetAge$DEGFIELD, "55=1; else=0")

#Highschool 2013-2019
subsetAge$HIGHSCHOOL <-recode(subsetAge$EDUCD, "63=1; else=0")
#Associates 2013-2019
subsetAge$ASSOCIATES <-recode(subsetAge$EDUCD, "81=1; else=0")
#Bachelors 2013-2019
subsetAge$BACHELORS <-recode(subsetAge$EDUCD, "101=1; else=0")
#save small file
subsetAgeselect = subset(subsetAge, select=c(OTHER, ASIAN, BLACK, MARRIED, MALE, MULTYEAR, INCWAGE, HIGHSCHOOL, ASSOCIATES, BACHELORS, AGE, BUSINESS, MED, SOCIAL, ENGINEERING, EDUCATION)) 
#save(subsetAgeselect, file = "subsetAgeselect.RData")

#test <- tapply(subsetAgeselect, INDEX=subsetAgeselect, FUN=mean)
#only thing that might need correcting here is a base group for education

#load("subsetAgeselect.RData")

##==============================================================================
#   3. Analysis section
#==============================================================================

#Basic Model = Wi = BXi + ei for each education group and year
#maybe have to recode the variables or list them or something
#Summary Statistics for Variables
stargazer(as.data.frame(subsetAgeselect), type = "text", summary.stat=c("n", "mean", "sd", "min", "max"), digits=2, title="Variable Summary Statistics")

#reg1=plm(INCWAGE ~ HIGHSCHOOL, 
# data=subset(subsetAgeselect, MULTYEAR==2019), index=c("PERNUM","SERIAL","SAMPLE","MULTYEAR"),model="pooling")

#Highschool Group 2019 Regression
HS2019=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2019,))
#Highschool Group 2018 Regression
HS2018=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2018,))
#Highschool Group 2017 Regression
HS2017=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2017,))
#Highschool Group 2016 Regression
HS2016=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2016,))
#Highschool Group 2015 Regression
HS2015=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2015,))
#Highschool Group 2014 Regression
HS2014=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2014,))
#Highschool Group 2013 Regression
HS2013=lm(INCWAGE ~ HIGHSCHOOL, data=subset(subsetAgeselect, MULTYEAR==2013,))


#Associates Group 2019 Regression
AD2019=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2019))
#Associates Group 2018 Regression
AD2018=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2018))
#Associates Group 2017 Regression
AD2017=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2017))
#Associates Group 2016 Regression
AD2016=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2016))
#Associates Group 2015 Regression
AD2015=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2015))
#Associates Group 2014 Regression
AD2014=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2014))
#Associates Group 2013 Regression
AD2013=lm(INCWAGE ~ ASSOCIATES, data=subset(subsetAgeselect, MULTYEAR==2013))

#Bachelors Group 2019 Regression
BD2019=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2019))
#Bachelors Group 2018 Regression
BD2018=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2018))
#Bachelors Group 2017 Regression
BD2017=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2017))
#Bachelors Group 2016 Regression
BD2016=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2016))
#Bachelors Group 2015
BD2015=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2015))
#Bachelors Group 2014
BD2014=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2014))
#Bachelors Group 2013
BD2013=lm(INCWAGE ~ BACHELORS, data=subset(subsetAgeselect, MULTYEAR==2013))

##Regression Output for Each Year
#2019
stargazer(HS2019, AD2019, BD2019,
          title="Annual Wages in 2019", type="text",
          df=FALSE, digits=5)
#2018
stargazer(HS2018, AD2018, BD2018,
          title="Annual Wages in 2018", type="text",
          df=FALSE, digits=5)
#2017
stargazer(HS2017, AD2017, BD2017,
          title="Annual Wages in 2017", type="text",
          df=FALSE, digits=5)
#2016
stargazer(HS2016, AD2016, BD2016,
          title="Annual Wages in 2016", type="text",
          df=FALSE, digits=5)
#2015
stargazer(HS2015, AD2015, BD2015,
          title="Annual Wages in 2015", type="text",
          df=FALSE, digits=5)
#2014
stargazer(HS2014, AD2014, BD2014,
          title="Annual Wages in 2014", type="text",
          df=FALSE, digits=5)
#2013
stargazer(HS2013, AD2013, BD2013,
          title="Annual Wages in 2013", type="text",
          df=FALSE, digits=5)

###Impact of Degree on INCWAGE
#Business Regression
regBUS=lm(INCWAGE ~ BACHELORS+BUSINESS, data=subset(subsetAgeselect, MULTYEAR==2019))
#Social Regression
regSOC=lm(INCWAGE ~ BACHELORS+SOCIAL, data=subset(subsetAgeselect, MULTYEAR==2019))
#Engineering Regression
regENG=lm(INCWAGE ~ BACHELORS+ENGINEERING, data=subset(subsetAgeselect, MULTYEAR==2019))
#Education Regression
regEDU=lm(INCWAGE ~ BACHELORS+EDUCATION, data=subset(subsetAgeselect, MULTYEAR==2019))
#Med Regression
regMED=lm(INCWAGE ~ BACHELORS+MED, data=subset(subsetAgeselect, MULTYEAR==2019))

stargazer(regBUS,regSOC,regENG,regEDU,regMED,
          title="Impact of Major on Annual Wages", type="text",
          df=FALSE, digits=5)