### prep Roster 5 ###
# outputs
#   Site-Export (Average Tx Size, Maize Clients, Enrolled Clients (N+R), Qualified Clients,
#               Total Site Credit, Qualified Maize Clients)
#   Group-Export (Average Tx Size, Maize Clients, Enrolled Clients (N+R), Qualified Clients,
#               Total Site Credit, Qualified Maize Clients)



##### set up ######
# clear environment and screen
rm(list = ls())
cat("\014")

# libraries
library(ggplot2)
library(dplyr)

# set wd
getwd()
setwd("D:/My Documents/R")

# load data
roster <- read.csv("Season Clients Detailed_LR15.csv", sep = ",", 
    header = T, stringsAsFactors = F)

# prep data

roster1 <- tbl_df(roster)

roster1 <- transform(roster1, TotalCredit = as.numeric(TotalCredit))
roster1 <- transform(roster1, TotalRepaid = as.numeric(TotalRepaid))

### change columns classes ###




### remove KENYA STAFF and create unique identifier per site
roster1 <- roster1 %>%
  filter(DistrictName != "KENYA STAFF") %>%
  
    
# data by client #  
  
  mutate(
      distinct.site = paste(DistrictName,SiteName, sep = ""),## unique identifier
      distinct.group = paste(DistrictName,SiteName,GroupName, sep = ""), ##unique group
      new.client = ifelse(NewMember == "True", 1, 0), ## add 1 for every new client
      ret.client = ifelse(NewMember == "True", 0,1 ), ## add 1 for every ret client
      new.trans = ifelse(NewMember == "False", TotalCredit, NA), ##new client transaction size
      ret.trans = ifelse(NewMember == "False", TotalCredit, NA),  ##ret client transaction size
       ## number of qualified clients with maize
      client.qualified = ifelse(TotalRepaid >=500, 1,0),
      clients.450_500 = ifelse(TotalRepaid >= 450 & TotalRepaid <500, 1,0),
      qualified.credit = ifelse(TotalRepaid >= 500, TotalCredit, 0))

# data by group # 


# site data 


## group level summary ##
  group.client <- roster1 %>%
      group_by(distinct.group) %>%
      dplyr::summarize(
          #identifiers
          district.name = first(DistrictName),
          site.name = first(SiteName),
          #enrollment
          groups = n_distinct(distinct.group),
          total.enrolled.clients = n_distinct(OAFID),
          total.qualified.clients = sum(client.qualified),
          total.enrolled.credit = sum(TotalCredit),
          total.qualified.credit =sum(qualified.credit),
          # credit
          new.trans.size = mean(new.trans, na.rm = T),
          ret.trans.size = mean(ret.trans, na.rm = T),
          new.clients = sum(new.client),
          ret.clients = sum(ret.client)
          # other info
          
      )

write.csv(group.client, file = "group.clients.5_LR15.csv", row.names = FALSE)

## site level summary ##
  site.client <- roster1 %>%
      group_by(distinct.site) %>%
      dplyr::summarize(
          #identifiers
          district.name = first(DistrictName),
          site.name = first(SiteName),
          #enrollment
          groups = n_distinct(distinct.group),
          total.enrolled.clients = n_distinct(OAFID),
          total.qualified.clients = sum(client.qualified),
          
          total.enrolled.credit = sum(TotalCredit),
          total.qualified.credit =sum(qualified.credit),
          # credit
          new.trans.size = mean(new.trans, na.rm = T),
          ret.trans.size = mean(ret.trans, na.rm = T),
          new.clients = sum(new.client),
          ret.clients = sum(ret.client)
          # other info
          
      )

write.csv(site.client, file = "site.clients.5_LR15s.csv", row.names = FALSE)
