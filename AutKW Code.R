# Varun Subramaniam
# AutKW Code (Review Data)

rm(list = ls())
setwd("~/Desktop/Varun/UNC/DDL")

library (tidytext)
library(dplyr)
library(janeaustenr)
library(ggplot2)
library (SemNetCleaner)
library (Hmisc)

#Loading Cleaned Data with Duplicates
double <- read.csv("Duplicate Cleaned Review Data.csv")
double <- tibble(double)
View(double)

#Loading Cleaned Data with Singles
single <- read.csv("Single Cleaned Review Data.csv")
single <- tibble(single)
single <- subset(single, single$Year > 2014)



#------------------------AUTHOR KEYWORD ANALYSIS (SINGLE)------------------------#

#Unnesting Author Keywords into row_AutKW
single$AutKW <- as.character(single$AutKW)
single <- unnest_tokens(single, row_AutKW, AutKW, to_lower=TRUE, token='regex', pattern="; ")
View(table(single$row_AutKW)) #Blockchain = 1912

#Cleaning Author Keywords for Grouping
single$row_AutKW <- gsub("-", " ", single$row_AutKW) #Removes all hyphens
single$row_AutKW <- gsub("and", "", single$row_AutKW) #Removes all ands
single$row_AutKW <- gsub("  ", " ", single$row_AutKW) #Replaces all double-spaces with singles
View(table(single$row_AutKW)) #Checking pre-singularized frequencies

#Merging like keywords
single[grep("*agri*", single$row_AutKW), "row_AutKW"] = "agriculture"
single[grep("*auction*", single$row_AutKW), "row_AutKW"] = "auction"
single[grep("*blo*", single$row_AutKW), "row_AutKW"] = "blockchain"
single[grep("*crypto*", single$row_AutKW), "row_AutKW"] = "crypto"
single[grep("*cross chain*", single$row_AutKW), "row_AutKW"] = "cross chain"
single[grep("*internet of thing*", single$row_AutKW), "row_AutKW"] = "iot"
single[grep("*iot*", single$row_AutKW), "row_AutKW"] = "iot"
single[grep("*5g*", single$row_AutKW), "row_AutKW"] = "5g"
single[grep("*smart contract*", single$row_AutKW), "row_AutKW"] = "smart contract"
single[grep("*smart grid*", single$row_AutKW), "row_AutKW"] = "smart grid"
single[grep("*distributed ledger*", single$row_AutKW), "row_AutKW"] = "dlt"
single[grep("*dlt*", single$row_AutKW), "row_AutKW"] = "dlt"
single[grep("*energy market*", single$row_AutKW), "row_AutKW"] = "energy market"
single[grep("*peer to peer*", single$row_AutKW), "row_AutKW"] = "p2p"
single[grep("*p2p*", single$row_AutKW), "row_AutKW"] = "p2p"
single[grep("*electric vehicle*", single$row_AutKW), "row_AutKW"] = "electric vehicle"
single[grep("*physical system*", single$row_AutKW), "row_AutKW"] = "cyber physical system"
single[grep("*physical system*", single$row_AutKW), "row_AutKW"] = "cyber physical system"
single[grep("*consensus*", single$row_AutKW), "row_AutKW"] = "consensus"
single[grep("*ethereum*", single$row_AutKW), "row_AutKW"] = "ethereum"
single[grep("*microgrid*", single$row_AutKW), "row_AutKW"] = "microgrid"
single[grep("*micro grid*", single$row_AutKW), "row_AutKW"] = "microgrid"

View(table(single$row_AutKW))
View(single)



#---TRENDS BY YEAR---#
df <- as.data.frame(table(single$row_AutKW))
df <-  df %>% arrange(desc(Freq))
df <- subset(df, df$Var1 != "blockchain")
df <- subset(df, df$Var1 != "dlt")
df <- rename(df,
            kw = Var1)
             
df <- (df[1:10,])
View(df)
test <- subset(single, single$row_AutKW %in% df$kw 
               & single$Year != "2022")
View(table(test$Year, test$row_AutKW))

test2 <- subset(test, test$Year == "2021")
View(table(test2$row_AutKW))
names(table(test$row_AutKW))






