# prep_ product adoption
# create a product adoption summary per district and site in 2017


# clear environment and screen
rm(list = ls())
cat("\014")

# load libraries #

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)


# set working directory
getwd()
setwd("D:/My Documents/R/Products")

#### load data #####
####-----------------------------------------------------------------------------------

scd <- read.csv("Season Clients Detailed_Nov_LR17.csv", sep = ",", header = T, stringsAsFactors = F)

fields <- colnames(scd[, 37:80])




#### define functions #####
####-----------------------------------------------------------------------------------

client_adopt <- function(x) {ifelse(x>0,1,0)}




#### prep data #####
####-----------------------------------------------------------------------------------

### prep roster fild

# remove staff
scd <- scd %>%
  filter(DistrictName != "KENYA STAFF")

# change data types
scd$Deceased <- as.logical(scd$Deceased)
scd$Dropped <- as.logical(scd$Dropped)
scd$NewMember <- as.logical(scd$NewMember)
scd$Facilitator <- as.logical(scd$Facilitator)

### create site level merge file

scd_m_s<-scd %>%
  mutate(
    DistinctSite = paste(DistrictName, SiteName, sep = "")
    ) %>%
  group_by(DistinctSite) %>%
  summarize(
    DistrictName = first(DistrictName),
    SiteName = first(SiteName),
    Clients = n_distinct(OAFID)
  )

### create district level merge file

scd_m_d <- scd %>%
  group_by(DistrictName) %>%
  summarize(
    Clients = n_distinct(OAFID)
  )

### create adoption data frame

adp <- as.data.frame(lapply(scd[, 37:80], FUN = function(x) {sapply(x, FUN = client_adopt)}))
adp <- bind_cols(scd[,c(3,6)], adp)

#### site summary average adoption per adopter #####
####-----------------------------------------------------------------------------------

### average product adoption per adopter

s_prod_adp <- scd 

s_prod_adp[s_prod_adp == 0] <- NA

s_prod_adp<- s_prod_adp %>%
  mutate(
    DistinctSite = paste(DistrictName, SiteName, sep = "")
  ) %>%
  group_by(
    DistinctSite
  ) %>%
  summarize_each(funs(mean(., na.rm=TRUE)), vars = 37:80)

q <- colnames(scd[, 37:80])
colnames(s_prod_adp)[2:45] <- q

s_prod_adp <- right_join(scd_m_s, s_prod_adp,  by = "DistinctSite")

### average client adoption

S_client_adp <- adp %>%
  mutate(
    DistinctSite = paste(DistrictName, SiteName, sep = "")
  ) %>%
  group_by(
    DistinctSite
  ) %>%
  summarize_each(
    funs(sum), vars = 3:46
    )

S_client_adp <- right_join(scd_m_s, S_client_adp, by = "DistinctSite"
    )
  
S_client_adp <- mutate_each(S_client_adp, funs(./Clients), var = 5:48
  ) %>%
  select(1:4,49:92)


q <- colnames(scd[, 37:80])
colnames(S_client_adp)[5:48] <- q

#### district summary #####
####-----------------------------------------------------------------------------------

### average product adoption per adopter


d_prod_adop <- scd 

d_prod_adop[d_prod_adop == 0] <- NA

d_prod_adop <- d_prod_adop %>%
  mutate(
    DistinctSite = paste(DistrictName, SiteName, sep = "")
  ) %>%
  group_by(
    DistrictName
  ) %>%
    summarize_each(funs(mean(., na.rm = TRUE)), vars = 37:80)

d_prod_adop <- right_join(scd_m_d, d_prod_adop, by = "DistrictName")

q <- colnames(scd[, 37:80])
colnames(d_prod_adop)[3:46] <- q

### average client adoption

d_client_adp <- adp %>%
  group_by(
    DistrictName
  ) %>%
  summarize_each(
    funs(sum), vars = 3:46
  )

d_client_adp <- right_join(scd_m_d, d_client_adp, by = "DistrictName")


d_client_adp <- mutate_each(d_client_adp,
      funs(./Clients), var = 3:46
      ) %>%
  select(1:2,47:90)


q <- colnames(scd[, 37:80])
colnames(d_client_adp)[3:46] <- q



#### wriet csv's #####
####-----------------------------------------------------------------------------------

write.csv(d_client_adp, file = "d_client_adp.csv", row.names = FALSE)

write.csv(d_prod_adop, file = "d_prod_adp.csv", row.names = FALSE)

write.csv(S_client_adp, file = "s_client_adp.csv", row.names = FALSE)

write.csv(s_prod_adp, file = "s_prod_adp.csv", row.names = FALSE)


