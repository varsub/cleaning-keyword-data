# Varun Subramaniam
# Graphing Code for Scholar/SCOPUS Data

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

#Loading Cleaned Data with Duplicates
double <- read.csv("Duplicate Cleaned Review Data.csv")
double <- tibble(double)

#Loading Cleaned Data with Singles
single <- read.csv("Single Cleaned Review Data.csv")
single <- tibble(single)

#------------------------JOURNALS (SINGLE)------------------------#
single$AbbrevSource <- tolower(single$AbbrevSource)
View(table(single$AbbrevSource))


test <- single
test[grep("*ieee*", test$AbbrevSource), "AbbrevSource"] = "ieee"
test[grep("*lect. notes*", test$AbbrevSource), "AbbrevSource"] = "lecture notes"
View(table(test$AbbrevSource))

View(single)

sum(single$ISBN == "")/dim(single)[1]*100 #65.7% Blank ISBNs
sum(single$ISSN == "")/dim(single)[1]*100 #42.2% Blank ISSNs

#------------------------DOC TYPE (SINGLE)------------------------#
single$DocType <- as.character(single$DocType)
single$DocType <- tolower(single$DocType)
work <- subset(single, single$Year >= 2015 & single$DocType != "") #Removes blanks and Pre-2015
View(table(work$DocType)) #Top non-blank DocTypes 2015-2022, unmerged


#Merging DocTypes (PENDING)
work[grep("*review*", work$DocType), "DocType"] = "review" #Merge anything with "review"
work[grep("*book*", work$DocType), "DocType"] = "book" #Merge anything with "book"
work[grep("*article*", work$DocType), "DocType"] = "article" #Merge anything with "article"
work[grep("*html*", work$DocType), "DocType"] = "online"
work[grep("*pdf*", work$DocType), "DocType"] = "online"




View(table(work$DocType)) #Top non-blank DocTypes 2015-2022, merged

#Top non-blank DocTypes 2015-2022 (re-check), sort by frequency and store as Test
test <- sort(table(work$DocType), decreasing=TRUE)

#Trend Analysis for Top 5 Overall DocTypes (excl. 2022, incomplete)
df <- as.data.frame(table(work$Year, work$DocType))
df <- subset (df, df$Var2 %in% names(head(test, 5)) & df$Var1 != 2022)
df <- rename (df,
              Year = Var1,
              DocType = Var2)

write.csv(df, "test.csv")
        

#------------------------DOC TYPE (DOUBLE)------------------------#
double$DocType <- as.character(double$DocType)
double$DocType <- tolower(double$DocType)
Dwork <- subset(double, double$Year >= 2015 & double$DocType != "") #Removes blanks and Pre-2015

Dwork[grep("*review*", Dwork$DocType), "DocType"] = "review" #Merge anything with "review"
Dwork[grep("*book*", Dwork$DocType), "DocType"] = "book" #Merge anything with "book"
Dwork[grep("*article*", Dwork$DocType), "DocType"] = "article" #Merge anything with "article"

Ddf <- as.data.frame(table(Dwork$Database, Dwork$DocType)) #DocType by Database

#Take a subset of Ddf that contains only Top 5 Overall DocTypes
Ddf <- subset (Ddf, Ddf$Var2 %in% names(head(test, 5)) & Ddf$Var1 != 2022) #uses TEST from above
View(Ddf)

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
View((table(single$row_AutKW))/15344*100)

