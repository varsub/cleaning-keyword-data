# Varun Subramaniam
# UPDATED: Draft of Code for Scholar/SCOPUS Data

rm(list = ls())
setwd("~/Desktop/Varun/DDL")

#Loading Packages
install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
                   "broom", "tokenizers", "janeaustenr", 
                   "tidytext", "SemNetCleaner", "Hmisc"))

library (tidytext)
library(dplyr)
library(janeaustenr)
library(ggplot2)
library (SemNetCleaner)
library (Hmisc)

#Loading Data
data <- read.csv("Review Database.csv")
data <- data[, colSums(is.na(data)) < nrow(data)]
data <- tibble(data)

View(data)

#Checking Consistency
table(data$Term)
table(data$Database)

#Play: SCOPUS -> Scholar Extrapolation 
scholar <- subset(data, data$Database == "Scholar")
scopus <- subset(data, data$Database == "SCOPUS")

View(scopus)
View(scholar)


                  #----------------------FILLING IN DOC TYPES----------------------#

#Percent of data with missing DocTypes pre-cleaning = 43.84%
(sum(data$DocType == "")) / dim(data)[1] * 100

View(table(scholar$DocType)) #What the doctypes look like pre-matching for scholars
sum(scholar$DocType == "") #Number of missing DocTypes in Scholar = 3494

#ATTN All Scholars: If your title is somewhere in SCOPUS' title, make your DocType that 
#corresponding row's DocType (as characters, not numbers) in SCOPUS!
scholar$DocType <- ifelse (scholar$Title %nin% scopus$Title, 
                           as.character(scopus$DocType), 
                           as.character(scholar$DocType))

View(table(scholar$DocType)) #Checking doctypes post-matching for scholars
sum(scholar$DocType == "") #Re-checking number of missing DocTypes in Scholar = 767

#DONE: We've matched the DocTypes for Scholars that have titles in SCOPUS!!


#Matching DocTypes for Scholars with DOIs in SCOPUS:
scholar$DocType <- ifelse (scholar$DOI %in% scopus$DOI, 
                           as.character(scopus$DocType), 
                           as.character(scholar$DocType))

View(table(scholar$DocType)) #Checking doctypes post-matching for scholars
sum(scholar$DocType == "") #Re-checking number of missing DocTypes in Scholar =64

#Percent of scholar that's empty for DocType = 1.395%
((sum(scholar$DocType == ""))/(length(scholar$DocType)))*100



                #----------------------FILLING IN KEYWORDS----------------------#

sum(data$AutKW == "") #Number of scholars and scopuses in Data missing author keywords = 5268
sum(scholar$AutKW == "") #Number of scholars missing author keywords = 4588
sum(scholar$AutKW == "") / dim(data)[1] * 100 #Percent of data missing author keywords = 56.6%


scholar$AutKW <- ifelse (scholar$DOI %in% scopus$DOI, 
                         as.character(scopus$AutKW), 
                         as.character(scholar$AutKW))

sum(scholar$AutKW == "") #Re-checking number of scholars missing author keywords = 1328
sum(scholar$AutKW == "") / dim(data)[1] * 100 #Percent of data missing author keywords = 16.4%



scholar$IndKW <- ifelse (scholar$AutKW %in% scopus$AutKW, 
                         as.character(scopus$IndKW), 
                         as.character(scholar$IndKW))


sum(scholar$IndKW == "") #Re-checking number of scholars missing author keywords = 1141
sum(scholar$IndKW == "") / dim(data)[1] * 100 #Percent of data missing author keywords = 14.09%


                                      #SCHOLAR -> DATA#
#Now we can re-write all the scholars in data as our cleaned scholars
sum(data$DocType == "") #Number of missing DocTypes in Data = 3551 
sum(data$AutKW == "") #Number of missing AutKW in Data = 5268
sum(data$IndKW == "") #Number of missing IndKW in Data = 5268

View(table(data$DocType)) #Table of DocTypes with "dirty" scholars


#If you're a scholar with no DocType in data (dirty), be replaced with your corresponding 
#DocType (clean) from Scholar, if not: stay as your current DocType
data$DocType <- ifelse (data$Database == "Scholar" & data$DocType == "", 
                        as.character(scholar$DocType), as.character(data$DocType)) 

View(table(data$DocType)) #Types of DocTypes in Data post-import (Table)
head(table(data$DocType))

sum(data$DocType == "") #Re-checking number of missing DocTypes in Data = 100 

#Percent of data with missing DocTypes post-cleaning
(sum(data$DocType == "")) / dim(data)[1] * 100


#If you're a scholar with no AutKW in data (dirty), be replaced with your corresponding 
#AutKW from Scholar, if not: stay as your current AutKW

data$AutKW <- ifelse (data$Database=="Scholar" & data$AutKW == "",
                      as.character(scopus$AutKW),
                      as.character (scholar$AutKW))
sum(data$AutKW == "") #Number of missing AutKW in Data = 1849

sum(data$AutKW == "") / dim(data)[1]*100 #22.83% of scholars and scopuses are missing keywords

sum(data$IndKW == "") / dim(data)[1]*100 #67.8% of scholars and scopuses are missing IndKW

data$IndKW <- ifelse (data$Database=="Scholar" & data$IndKW == "",
                      as.character(scopus$IndKW),
                      as.character (scholar$IndKW))
sum(data$IndKW == "") #Number of missing IndKW in Data = 2042
sum(data$IndKW == "") / dim(data)[1]*100 #25.2% of scholars and scopuses are missing IndKW


#All duplicates removed from fully cleaned file
clean <- data[!duplicated(data$DOI) & !duplicated(data$Title),] 

#Now Data has full to show Scholar/SCOPUS separation and clean has
#no repeats for quantitative analysis

View(table(clean$DocType))
write.csv(data, file = "Duplicate Cleaned Review Data.csv")
write.csv(clean, file = "Single Cleaned Review Data.csv")

