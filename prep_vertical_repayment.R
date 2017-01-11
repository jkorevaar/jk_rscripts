##### set up ######
# clear environment and screen
rm(list = ls())
cat("\014")

# load libraries #

library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)


# set working directory
setwd("D:/My Documents/R/Pilots")

# load data #

vrp1 <- read.csv("Detailed_20161204-051246.csv", na.strings = "")
roster <- read.csv("Season Clients Detailed_20161204-055412.csv", sep = ",", header = T, stringsAsFactors = F)
#dropped <- # add dropped clients 


##### prep roster data #####
###-----------------------------------------------------------------------------------

# convert column type
roster$TotalCredit <- as.numeric(roster$TotalCredit)

# merge dropped clients



##### prep vertical repayment data #####
###-----------------------------------------------------------------------------------

# filter out dropped/deceased clients and for right repayment types
vrp1 <- vrp1 %>%
  filter(
    Dropped == "False"
    )

# convert column type #
vrp1$Amount = as.numeric(as.character(vrp1$Amount))
vrp1$RepaymentDate = as.Date(as.character(vrp1$RepaymentDate))





##### classify payments by week made #####
###-----------------------------------------------------------------------------------

# determine start day of repaymnet
st.date <- min(vrp1$RepaymentDate)
st.date <- "2015-10-12"

# create a variable for week
vrp1$week <-NULL

### classify each payment by which week it was made in ###

# first week

vrp1$week[vrp1$RepaymentDate < (as.Date(st.date)+0)] <- 0

# classify rest
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+0) 
  & vrp1$RepaymentDate < (as.Date(st.date)+7)] <- 1
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+7) 
  & vrp1$RepaymentDate < (as.Date(st.date)+14)] <- 2
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+14) 
  & vrp1$RepaymentDate < (as.Date(st.date)+21)] <- 3
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+21) 
  & vrp1$RepaymentDate < (as.Date(st.date)+28)] <- 4
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+28) 
  & vrp1$RepaymentDate < (as.Date(st.date)+35)] <- 5
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+35) 
  & vrp1$RepaymentDate < (as.Date(st.date)+42)] <- 6
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+42) 
  & vrp1$RepaymentDate < (as.Date(st.date)+49)] <- 7
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+49) 
  & vrp1$RepaymentDate < (as.Date(st.date)+56)] <- 8
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+56) 
  & vrp1$RepaymentDate < (as.Date(st.date)+63)] <- 9
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+63) 
  & vrp1$RepaymentDate < (as.Date(st.date)+70)] <- 10
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+70) 
  & vrp1$RepaymentDate < (as.Date(st.date)+77)] <- 11
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+77) 
  & vrp1$RepaymentDate < (as.Date(st.date)+84)] <- 12
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+84) 
  & vrp1$RepaymentDate < (as.Date(st.date)+91)] <- 13
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+91) 
  & vrp1$RepaymentDate < (as.Date(st.date)+98)] <- 14
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+98) 
  & vrp1$RepaymentDate < (as.Date(st.date)+105)] <- 15
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+105) 
  & vrp1$RepaymentDate < (as.Date(st.date)+112)] <- 16
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+112) 
  & vrp1$RepaymentDate < (as.Date(st.date)+119)] <- 17
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+119) 
  & vrp1$RepaymentDate < (as.Date(st.date)+126)] <- 18
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+126) 
  & vrp1$RepaymentDate < (as.Date(st.date)+133)] <- 19
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+133) 
  & vrp1$RepaymentDate < (as.Date(st.date)+140)] <- 20
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+140) 
  & vrp1$RepaymentDate < (as.Date(st.date)+147)] <- 21
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+147) 
  & vrp1$RepaymentDate < (as.Date(st.date)+154)] <- 22
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+154) 
  & vrp1$RepaymentDate < (as.Date(st.date)+161)] <- 23
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+161) 
  & vrp1$RepaymentDate < (as.Date(st.date)+168)] <- 24
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+168) 
  & vrp1$RepaymentDate < (as.Date(st.date)+175)] <- 25
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+175) 
  & vrp1$RepaymentDate < (as.Date(st.date)+182)] <- 26
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+182) 
  & vrp1$RepaymentDate < (as.Date(st.date)+189)] <- 27
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+189) 
  & vrp1$RepaymentDate < (as.Date(st.date)+196)] <- 28
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+196) 
  & vrp1$RepaymentDate < (as.Date(st.date)+203)] <- 29
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+203) 
  & vrp1$RepaymentDate < (as.Date(st.date)+210)] <- 30
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+210) 
  & vrp1$RepaymentDate < (as.Date(st.date)+217)] <- 31
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+217) 
  & vrp1$RepaymentDate < (as.Date(st.date)+224)] <- 32
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+224) 
  & vrp1$RepaymentDate < (as.Date(st.date)+231)] <- 33
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+231) 
  & vrp1$RepaymentDate < (as.Date(st.date)+238)] <- 34
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+238) 
  & vrp1$RepaymentDate < (as.Date(st.date)+245)] <- 35
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+245) 
  & vrp1$RepaymentDate < (as.Date(st.date)+252)] <- 36
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+252) 
  & vrp1$RepaymentDate < (as.Date(st.date)+259)] <- 37
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+259) 
  & vrp1$RepaymentDate < (as.Date(st.date)+266)] <- 38
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+266) 
  & vrp1$RepaymentDate < (as.Date(st.date)+273)] <- 39
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+273) 
  & vrp1$RepaymentDate < (as.Date(st.date)+280)] <- 40
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+280) 
  & vrp1$RepaymentDate < (as.Date(st.date)+287)] <- 41
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+287) 
  & vrp1$RepaymentDate < (as.Date(st.date)+294)] <- 42
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+294) 
  & vrp1$RepaymentDate < (as.Date(st.date)+301)] <- 43
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+301) 
  & vrp1$RepaymentDate < (as.Date(st.date)+308)] <- 44
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+308) 
  & vrp1$RepaymentDate < (as.Date(st.date)+315)] <- 45
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+315) 
  & vrp1$RepaymentDate < (as.Date(st.date)+322)] <- 46
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+322) 
  & vrp1$RepaymentDate < (as.Date(st.date)+329)] <- 47
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+329) 
  & vrp1$RepaymentDate < (as.Date(st.date)+336)] <- 48
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+336) 
  & vrp1$RepaymentDate < (as.Date(st.date)+343)] <- 49
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+343) 
  & vrp1$RepaymentDate < (as.Date(st.date)+350)] <- 50
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+350) 
  & vrp1$RepaymentDate < (as.Date(st.date)+357)] <- 51
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+357) 
  & vrp1$RepaymentDate < (as.Date(st.date)+364)] <- 52
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+364) 
  & vrp1$RepaymentDate < (as.Date(st.date)+371)] <- 53
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+371) 
  & vrp1$RepaymentDate < (as.Date(st.date)+378)] <- 54
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+378) 
  & vrp1$RepaymentDate < (as.Date(st.date)+385)] <- 55
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+385) 
  & vrp1$RepaymentDate < (as.Date(st.date)+392)] <- 56
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+392) 
  & vrp1$RepaymentDate < (as.Date(st.date)+399)] <- 57
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+399) 
  & vrp1$RepaymentDate < (as.Date(st.date)+406)] <- 58
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+406) 
  & vrp1$RepaymentDate < (as.Date(st.date)+413)] <- 59
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+413) 
  & vrp1$RepaymentDate < (as.Date(st.date)+420)] <- 60

# last date
vrp1$week[vrp1$RepaymentDate >= (as.Date(st.date)+420)] <- 61

 

#### spread out data in a weekly column ####
####------------------------------------------------------------------

# create a unique variable for GlobalClientID and Week and...
# summarize by total amount per client per week #
summary.table <- vrp1 %>%
    mutate(
      unique = paste(GlobalClientID,week)
      ) %>%
    group_by(
      unique
      ) %>%
    dplyr::summarize(
      GlobalClientID = first(GlobalClientID)
      ,Amount = sum(Amount)
      ,week = first(week)
      )

# remove 0 Amounts and blank Global Client ID 
summary.table <- summary.table[!(is.na(summary.table$GlobalClientID)),
    c("GlobalClientID","Amount","week")]

summary.table <- filter(summary.table, Amount != 0)

# spread out databy week 

weekly.spread <- spread(summary.table, key = week, value = Amount)

# match with roster
   
weekly.spread <- right_join(roster, weekly.spread)

# change NAs to 0s #
weekly.spread[is.na(weekly.spread)] <- 0



##### get variable for the number of weeks #####
###-----------------------------------------------------------------------------------

num.base <- ncol(roster)
num.base2 <- ncol(roster)+1
num.week1 <- ncol(weekly.spread)-num.base-1
num.week2 <- ncol(weekly.spread)-num.base

q <- paste("week.", as.character(as.Date(st.date)+c(0:num.week1)*7), sep = "")

x <- (num.week2+num.base)

colnames(weekly.spread)[num.base2:x] <- q


rm(vrp1)
rm(roster)




##### prep 3. create weekly cummulative % repaid table #####
###-----------------------------------------------------------------------------------

# table with unique global ids
cum.weekly.repaid <- summary.table %>%
  select(GlobalClientID) %>%
  unique()

# repeat the unique IDs N week times
cum.weekly.repaid <- do.call("rbind", replicate(num.week2, cum.weekly.repaid, simplify = FALSE))

# rolling total for num of weeks
cum.weekly.repaid$w <- 1
setDT(cum.weekly.repaid) #set the table as your dataset
setkey(cum.weekly.repaid, GlobalClientID) 
cum.weekly.repaid[,week := cumsum(w),by=c('GlobalClientID')] 

cum.weekly.repaid <- select(cum.weekly.repaid, GlobalClientID, week)

# create table with a row for every single week of every globalclient id

# change summary.table to data.table
summary.table <- data.table(summary.table)

cum.weekly.repaid<- left_join(cum.weekly.repaid, summary.table, 
  by = c("GlobalClientID" = "GlobalClientID", "week" = "week"))


cum.weekly.repaid[is.na(cum.weekly.repaid)] <- 0


# cumsum
setDT(cum.weekly.repaid) #set the table as your dataset
setkey(cum.weekly.repaid, GlobalClientID) 
cum.weekly.repaid[,csum := cumsum(Amount),by=c('GlobalClientID')] 

cum.weekly.repaid <- select(cum.weekly.repaid, GlobalClientID, week, csum)
cum.weekly.repaid$GlobalClientID <- as.character(cum.weekly.repaid$GlobalClientID)

# spread out databy week #

cum.weekly.repaid <- spread(cum.weekly.repaid, key = week, value = csum)

# match district, site, and OAF ID
   
cum.weekly.repaid <- right_join(weekly.spread[,1:num.base], cum.weekly.repaid) 

# change NAs to 0s #
cum.weekly.repaid[is.na(cum.weekly.repaid)] <- 0

# change weeks to include week in var name
q <- paste("week.", as.character(as.Date(st.date)+c(0:num.week1)*7), sep = "")

x <- (num.week2+num.base)
colnames(cum.weekly.repaid)[num.base2:x] <- q

# summarize table to cummulative % repaid
cum.weekly.repaid <- cum.weekly.repaid %>%
  mutate_each(
    funs(./TotalCredit), vars = starts_with("week")
    ) %>%
  select(
    -starts_with("week")
    )

# rename rows
q <- paste("cum.%.repaid.week", as.character(as.Date(st.date)+c(0:num.week1)*7), sep = "")

x <- (num.week2+num.base)
colnames(cum.weekly.repaid)[num.base2:x] <- q




# summarize on site level

rm(percent.weekly.repaid)
rm(summary.table)
rm(weekly.spread)

##### summary msl / non-msl clients % Repaid #####
###-----------------------------------------------------------------------------------

# change row labels
q <- paste("week.", as.character(c(0:num.week1)), sep = "")

x <- (num.week2+num.base)
colnames(cum.weekly.repaid)[num.base2:x] <- q


q <- paste(as.character(as.Date(st.date)+c(0:num.week1)*7), sep = "")

x <- (num.week2+num.base)
colnames(cum.weekly.repaid)[num.base2:x] <- q




# create variable to compare
x <- cum.weekly.repaid %>%
   mutate(
      type = ifelse(DistrictName == "Tinderet", "Tinderet", "Kenya")
   ) %>%
   filter() %>%
   select() %>% 
   summarise_each(funs(mean)) %>% 
   gather(Var, Val, -type)
   

# bar 

y <- ggplot(., aes(x=Var, y=Val, group = type))+
       stat_summary(geom="line", fun.y="mean") +

    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position= c(0,1),
      legend.justification=c(0,1))+
    labs(title = "Tinderet & Kenya Repayment Trajectory", x = "Week",
      y = "Percent Repaid")
    
plot(x)










