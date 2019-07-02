library(readxl)      #for excel, csv sheets manipulation
library(sdcMicro)    #sdcMicro package with functions for the SDC process 
library(tidyverse)   #optional #for data cleaning

#Import data
setwd("C:/Users/LENOVO T46OS/Desktop/dra-cesvi-palestine-microdata")
data <- read_excel("data.xlsx", skip=1)

#Xchange Bangladesh microdata
selectedKeyVars <- c('Q7','Q11',	'Q12',	'Q13'
                     )
#weightVar <- c('weight')

#Convert variables into factors
cols =  c('Q7','Q10',	'Q11',	'Q12',	'Q13')
data[,cols] <- lapply(data[,cols], factor)

#Convert the sub file into dataframe
fileRes<-data[,selectedKeyVars]
fileRes <- as.data.frame(fileRes)

#Assess the disclosure risk
objSDC <- createSdcObj(dat = fileRes, keyVars = selectedKeyVars)
print(objSDC, "risk")
report(objSDC, filename ="index",internal = T) 
