library(stringr)
library(ggplot2)
source("http://peterhaschke.com/Code/multiplot.R")

#Helper functions
# Define function to reverse scores on a dataframe with reverse labeled headers
reverseScale <- function(data,minVal,maxVal){
        for(i in 1:ncol(data)){
                if(str_sub(colnames(data)[i],-1) == 'R') {
                        for(j in 1:nrow(data)) {
                                data[j,i] <- (maxVal+minVal) - as.numeric(data[j,i])
                        }
                }
        }
        data
}

# change characters to numeric and find mean of row (ignoring NA)
meanRow <- function(char_mat) {
        num_out <- vector(mode="numeric", length=0)
        for(i in 1:nrow(char_mat)){
                num_out[i] <- mean(as.numeric(char_mat[i,]),na.rm=TRUE)
        }
        num_out
}

# Function to fix header of merged files so there is one header row with identifying key variable names
fixHeader <- function(dataset){
        dataset[1,1:5] <- colnames(dataset[1:5]) #fix column headers
        colnames(dataset) <- dataset[1,]
        dataset  <- dataset[-c(1),] #remove extra header row
        #         new_header <- dataset[!dataset$Name=="FullBattery",] #remove pilot data
        dataset
}

#Merging ballet adults and youth
youthwisdata <- read.csv('~/Documents/Projects/Ballet/DATA/Ballet_youths_wisdom.csv',
                         na.strings = c("", " ", "NA"), stringsAsFactors=F)
youthexpdata <- read.csv('~/Documents/Projects/Ballet/DATA/Ballet_youths_demoexp.csv',
                         na.strings = c("", " ", "NA"), stringsAsFactors=F)
youthtadata  <- read.csv('~/Documents/Projects/Ballet/DATA/Ballet_youths_TA.csv',
                         na.strings = c("", " ", "NA"), stringsAsFactors=F)
adultwisdata <- read.csv('~/Documents/Projects/Ballet/DATA/adults_ballet_wisdom_032315.csv',
                         na.strings = c("", " ", "NA"), stringsAsFactors=F)
adultexpdata <- read.csv('~/Documents/Projects/Ballet/DATA/adults_ballet_expdemo_032315.csv',
                         na.strings = c("", " ", "NA"), stringsAsFactors=F)
adulttadata  <- read.csv('~/Documents/Projects/Ballet/DATA/Ballet_adults_TA.csv',
                         na.strings = c("", " ", "NA","NaN"), stringsAsFactors=F)

## Process wisdom data
#reverse score wisdom
youthwisrev <- reverseScale(youthwisdata,1,5)
adultwisrev <- reverseScale(adultwisdata,1,5)
#extract wisdom subscales for youths
youth_cogW_cols  <- youthwisrev[,grep(pattern="C+\\d",colnames(youthwisrev))] #cognitive wisdom
youth_reflW_cols <- youthwisrev[,grep(pattern="R+\\d",colnames(youthwisrev))] #reflectiv wisdom
youth_affW_cols  <- youthwisrev[,grep(pattern="A+\\d",colnames(youthwisrev))] #affective wisdom
#extract wisdom subscales for adults
adult_cogW_cols  <- adultwisrev[,grep(pattern="C+\\d",colnames(adultwisrev))] #cognitive wisdom
adult_reflW_cols <- adultwisrev[,grep(pattern="R+\\d",colnames(adultwisrev))] #reflectiv wisdom
adult_affW_cols  <- adultwisrev[,grep(pattern="A+\\d",colnames(adultwisrev))] #affective wisdom
#compute average wisdom scores for youth
youth_cogW  <- meanRow(youth_cogW_cols)
youth_reflW <- meanRow(youth_reflW_cols)
youth_affW  <- meanRow(youth_affW_cols)
#compute average wisdom scores for adults
adult_cogW  <- meanRow(adult_cogW_cols)
adult_reflW <- meanRow(adult_reflW_cols)
adult_affW  <- meanRow(adult_affW_cols)
#Create dataframes of just wisdom scores and PID (key) for youths and adults
youthtidywis <- data.frame(cbind(youthwisdata$PID,youth_cogW,youth_reflW,youth_affW),stringsAsFactors=F)
youthtidywis[2:4] <- round(youthtidywis[2:4],digits=2)
colnames(youthtidywis)[1] <- "PID"

adulttidywis <- data.frame(cbind(adultwisdata$PID,adult_cogW,adult_reflW,adult_affW),stringsAsFactors=F)
adulttidywis[2:4] <- round(adulttidywis[2:4],digits=2)
colnames(adulttidywis)[1] <- "PID"

##Process experience and demographic data
youthhours <- youthexpdata$hours_experience_open_estimation
youthhours <- as.numeric(gsub("[[:punct:]]|[A-z]|[ ]", "", youthhours))
# youthhoursbin <- youthexpdata$hours_experience_
#Reverse score TA
youthta_cols <- youthtadata[,grep(pattern = "TA",colnames(youthtadata))]         #isolate TA data columns
youthtarev   <- reverseScale(youthta_cols,1,4)
youthta      <- meanRow(youthtarev)

adultta_cols <- adulttadata[,grep(pattern = "TA",colnames(adulttadata))]         #isolate TA data columns
adulttarev   <- reverseScale(adultta_cols,1,4)
adultta      <- meanRow(adulttarev)

#hours experience and age
adulthours <- adultexpdata$hours_experience_open_estimation
adulthours <- as.numeric(gsub("[[:punct:]]|[A-z]|[ ]", "", adulthours))

youthage <- youthexpdata$age
youthage <- as.numeric(gsub("[[:punct:]]|[A-z]|[ ]", "", youthage))

adultage <- as.numeric(adultexpdata$age)
adultgrp <- adultexpdata$expnov
#Create dataframes of hours exp and age
youthtidyexp <- cbind(youthexpdata$PID,youthhours,youthage,youthta)
youthtidyexp <- data.frame(youthtidyexp,stringsAsFactors = F)
youthtidyexp$youthta <- round(youthtidyexp$youthta, digits=2)
colnames(youthtidyexp)[1] <- "PID"

adulttidyexp <- cbind(adultexpdata$PID,adulthours,adultage,adultta,adultgrp)
adulttidyexp <- data.frame(adulttidyexp,stringsAsFactors = F)
adulttidyexp$adultta <- round(as.numeric(adulttidyexp$adultta), digit=2)
colnames(adulttidyexp)[1] <- "PID"
#bind wisdom and experience youths
youthtidy <- merge(youthtidywis,youthtidyexp,by="PID")
youthtidy <- cbind("youth",youthtidy)
youthtidy <- cbind(youthtidy,"NA")
youthtidy <- youthtidy[,c(2,1,3,4,5,6,7,8,9)]
names(youthtidy) <- c("PID","cohort","cogW","reflW","affW","hours","age","ta","expertise")
#bind wisdom and experience adults 
adulttidy <- merge(adulttidywis,adulttidyexp,by="PID")
adulttidy <- cbind("adult",adulttidy)
adulttidy <- adulttidy[,c(2,1,3,4,5,6,7,8,9)]
names(adulttidy) <- c("PID","cohort","cogW","reflW","affW","hours","age","ta","expertise")
#bind adults and youths
tidydata <- data.frame(rbind(youthtidy,adulttidy),stringsAsFactors=F)
tidydata <- tidydata[,c(1,2,9,6,7,3,4,5,8)]
tidydata$hours <- as.numeric(tidydata$hours)
tidydata$age <- as.numeric(tidydata$age)
# tidierdata <- tidydata[tidydata$expertise!='novice',]
# tidierdata <- tidierdata[complete.cases(tidierdata),]
# 
# tidierdata$hours <- as.numeric(tidierdata$hours)
# tidierdata$loghours <- log(tidierdata$hours)
# tidierdata$age <- as.numeric(tidierdata$age)
# 
# # balplot1 <- ggplot(tidierdata, aes(x=loghours, y=reflW, group=cohort))
# #         geom_point(alpha=.75) + geom_smooth(alpha=.2, size=1) + theme(legend.position="right") 
# 
# balplot_hours <- ggplot(tidierdata, aes(x=hours, y=cogW, colour=cohort)) +
#         geom_point(aes(shape=cohort, size=5))
# 
# balplot_loghours <- ggplot(tidierdata, aes(x=loghours, y=cogW, colour=cohort)) +
#         geom_point(aes(shape=cohort, size=5))
# 
# cogW_t_cohort <- aov(cogW~cohort + age, data=tidierdata)
# reflW_t_cohort <- aov(reflW~cohort + age, data=tidierdata)
# affW_t_cohort <- aov(affW~cohort + age, data=tidierdata)