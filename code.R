library(readxl)      #for excel, csv sheets manipulation
library(sdcMicro)    #sdcMicro package with functions for the SDC process 
library(tidyverse)   #optional #for data cleaning

#Import data
setwd("C:/Users/LENOVO T46OS/Desktop/dra-cesvi-palestine-microdata")
data <- read_excel("data.xlsx", sheet = "Baseline", na = "\"\"", skip = 1)

#Xchange Bangladesh microdata
selectedKeyVars <- c('Q11',	'Q13')

#Weight variable - assuming it is 1 for each individual
weightVars <- c('weight')

#Convert variables into factors
cols =  c('Q7','Q10',	'Q11',	'Q12',	'Q13')
data[,cols] <- lapply(data[,cols], factor)

#Convert the sub file into dataframe
subVars <- c('ID', selectedKeyVars, weightVars)
fileRes<-data[,subVars]
fileRes <- as.data.frame(fileRes)

#Assess the disclosure risk
objSDC <- createSdcObj(dat = fileRes, keyVars = selectedKeyVars, weightVar = weightVars)

#SDC
objSDC<- localSupp(objSDC, threshold = 0.1, keyVar = 'Q11')
print(objSDC, "ls")
objSDC<- localSupp(objSDC, threshold = 0.1, keyVar = 'Q13')
print(objSDC, "ls")

#Extract and store anonymized data
dataAnon <- extractManipData(objSDC)
fileUnanonymized<-data
fileUnanonymized[,c('ID', 'Q11', 'Q13')]<-list(NULL)
fileCombined<-bind_cols(x=dataAnon, y=fileUnanonymized)
write.csv(fileCombined,'cesvi_anonymized_microdata.csv') 

#Report + risk prior to SDC
print(objSDC, 'risk')
report(objSDC, filename = "index",internal = T) 

#Utility loss measures
#Check the number of missing values (NA) after local suppression application
namesKeyVars<- names(objSDC@manipKeyVars) 
NAcount <- matrix(NA, nrow = 2, ncol = length(namesKeyVars)) 
colnames(NAcount)  <- c(paste0('NA', namesKeyVars))
rownames(NAcount)  <- c('initial', 'treated')
for(i in 1:length(namesKeyVars)) 
{
  NAcount[1, i] <- sum(is.na(objSDC@origData[,namesKeyVars[i]]))
  NAcount[2, i] <- sum(is.na(objSDC@manipKeyVars[,i]))
}   
NAcount