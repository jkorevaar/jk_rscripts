### Quick Analysis Tool ###
### For: KE-PI M&E Team ###
### Prepared by: Jan Korevaar (jan.korevaar@oneacrefund.org) ###
### Last Edited: 05-OCt-2016

# clear environment and screen
rm(list = ls())
cat("\014")

# libraries
options(java.parameters = "-Xmx8000m")

library(tidyr)
library(printr)
library(knitr)
library(ggplot2)
library(lubridate)
library(GGally)
library(dplyr)
library(xlsx)
library(openxlsx)
library(lsmeans)
library(broom)
library(stringr)


# setw

setwd("D:/Google Drive/02. Market Access/Market Access Research/Maize Marketing Practice Survey/Result & Analysis/1. Data")

#### load data ####
#### ----------------------------------------------------------------------------------

## post-harvest survey
s <- read.csv("forms.csv", sep = "," , header = T, stringsAsFactors = F)


#### functions ####
#### ----------------------------------------------------------------------------------

ctgrz_oth <- function(data, oth.col, oth.pat, r.col, replace.pat) {
    
   x <- which(grepl(oth.pat, data[,oth.col]))
   y <- data[x,oth.col]
   data[x,r.col] <- paste(data[x,r.col], replace.pat, sep = " ")
   data[x,oth.col] <- NA
   data[x,r.col] <- gsub("Other ", "", data[x,r.col])
   data[x,r.col] <- gsub("other ", "", data[x,r.col])
   print("These are the strings that were replaced:")
   print(y)

   return(data)
   }

ql_create <- function(data, rep.vars){
      
      
   c_list <- data.frame(paste(colnames(data), "00COL", sep = "_"))
   colnames(c_list)[1] <- "col_var"
   
   
   m <- data.frame(data[,rep.vars])
   colnames(m) <- colnames(data)[rep.vars]
   
   for(i in 1:ncol(m)){
   
      if(exists("a_list")){
         
         q <- data.frame(v = m[,i])
         q[,1] <- as.character(q[,1])
   
         q <- q %>%
            mutate(v = strsplit(v, "\\s+")) %>%
            unnest(v) %>%
            distinct() %>% 
            filter( !is.na(v)) %>%
            transmute(v = paste(colnames(m)[i], v, "ANS", sep = "_"))
         
         colnames(q)[1] <- "col_var"
         
         a_list <- bind_rows(a_list, q)
      }
      
      if (!exists("a_list")){
         
         q <- data.frame(v = m[,i])
         q[,1] <- as.character(q[,1])
   
         q <- q %>%
            mutate(v = strsplit(v, "\\s+")) %>%
            unnest(v) %>%
            distinct() %>% 
            filter( !is.na(v)) %>%
            transmute(v = paste(colnames(m)[i], v, "ANS", sep = "_"))
         
         colnames(q)[1] <- "col_var"
         
         a_list <- q
      }
      
   }
   
   ret <- rbind(a_list, c_list)
   ret <- arrange(ret, col_var)
   ret$new_name <- ""
   ret$analysis1 <- ""
   ret$analysis2 <- ""
   ret$analysis3 <- ""
   ret$analysis4 <- ""
   
   return(ret)
}

looksy <- function(data, x){ 
   y <- data[,x]
   y <- y[!is.na(y)]
   y <- sort(y)
   print(y)
}

#### prep maize quality survey data ####
#### ----------------------------------------------------------------------------------

### change --- to NA
s[s == "---"] <- NA


## create new columns

s$trust_goro_goro <- "Other"
s$why_sell_at_location <- "Other"
s$selling_strategy_what <- "Other"


## catergorize other columns
# col 6,7
unique(looksy(s,6))
#"Upcoming_large_investment_or_fee"
#"Emergency"                       
#"Other"                           
#"The_current_market_price"

looksy(s,7)

s <- ctgrz_oth(s, 7, "n/a", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "consumption", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "food", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "not", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "don't", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "doesn", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "cant", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "never", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "won't", 6, "No_Sales")
s <- ctgrz_oth(s, 7, "dont", 6, "No_Sales")


s <- ctgrz_oth(s, 7, "chool", 6, "Upcoming_large_investment_or_fee")
s <- ctgrz_oth(s, 7, "loan", 6, "Upcoming_large_investment_or_fee")
s <- ctgrz_oth(s, 7, "fees", 6, "Upcoming_large_investment_or_fee")
s <- ctgrz_oth(s, 7, "OAF", 6, "Upcoming_large_investment_or_fee")


s <- ctgrz_oth(s, 7, "price", 6, "The_current_market_price")
s <- ctgrz_oth(s, 7, "market", 6, "The_current_market_price")
s <- ctgrz_oth(s, 7, "markrt", 6, "The_current_market_price")


s <- ctgrz_oth(s, 7, "rgency", 6, "Emergency")


# col 8,9

unique(looksy(s,8))
looksy(s,9)

s <- ctgrz_oth(s, 9, "sell", 8, "No_Sales")
s <- ctgrz_oth(s, 9, "n/a", 8, "No_Sales")
s <- ctgrz_oth(s, 9, "don't", 8, "No_Sales")
s <- ctgrz_oth(s, 9, "sumption", 8, "No_Sales")
s <- ctgrz_oth(s, 9, "food", 8, "No_Sales")
s <- ctgrz_oth(s, 9, "not", 8, "No_Sales")



s <- ctgrz_oth(s, 9, "fees", 8, "School_Fees")
s <- ctgrz_oth(s, 9, "school", 8, "School_Fees")
s <- ctgrz_oth(s, 9, "repay", 8, "OAF_Loan")
s <- ctgrz_oth(s, 9, "acre", 8, "OAF_Loan")

s <- ctgrz_oth(s, 9, "cow", 8, "Investments:_More_than_4,000_(e.g._renting_land,_putting_a_roof_on_your_house,_etc)
")
s <- ctgrz_oth(s, 9, "business", 8, "Investments:_More_than_4,000_(e.g._renting_land,_putting_a_roof_on_your_house,_etc)
")

# col 11, 12
unique(looksy(s,11))
#"Other"         "local_trader"  "Trader_market" "Local_Farmer" 
looksy(s,12)

s <- ctgrz_oth(s, 12, "school", 11, "School_Fees")
s <- ctgrz_oth(s, 12, "n/a", 11, "N/A")
s <- ctgrz_oth(s, 12, "n /a", 11, "N/A")
s <- ctgrz_oth(s, 12, "does", 11, "No_Sales")
s <- ctgrz_oth(s, 12, "sale", 11, "No_Sales")
s <- ctgrz_oth(s, 12, "emergency", 11, "N/A")
s <- ctgrz_oth(s, 12, "arsen", 11, "N/A")
s <- ctgrz_oth(s, 12, "don", 11, "No_Sales")
s <- ctgrz_oth(s, 12, "buyer", 11, "N/A")
s <- ctgrz_oth(s, 12, "acre", 11, "N/A")
s <- ctgrz_oth(s, 12, "price", 11, "N/A")
s <- ctgrz_oth(s, 12, "neigh", 11, "Local_Farmer" )
s <- ctgrz_oth(s, 12, "traders", 11, "local_trader" )
s <- ctgrz_oth(s, 12, "clients", 11, "N/A" )


# col 14, 15
unique(looksy(s,14))
#[1] "Other"                                                     
#[2] "The_buyer_is_someone_I_trust"                              
#[3] "The_buyer_is_the_person_who_comes_the_quickest_to_my_house"
#[4] "The_buyer_is_the_person_who_gives_me_the_highest_price"
looksy(s,15)
s <- ctgrz_oth(s, 15, "a", 14, "N/A" )
s <- ctgrz_oth(s, 15, "e", 14, "N/A" )

# col 41, 20
looksy(s,20)
#"Use_Traders"
#"Use_Farmers"
#"Use_standard"
#"Sell_Other_Units"
#"We_Compare_Tins"
#"Use_farmers_experience_or_look"
s <- ctgrz_oth(s, 20, "dard", 41, "Use_Standard" )
s <- ctgrz_oth(s, 20, "normal", 41, "Use_Standard" )


s <- ctgrz_oth(s, 20, "compar", 41, "We_Compare_Tins" )
s <- ctgrz_oth(s, 20, "quivalent", 41, "We_Compare_Tins" )
s <- ctgrz_oth(s, 20, "equal to", 41, "We_Compare_Tins" )


s <- ctgrz_oth(s, 20, "own", 41, "Use_Farmers" )
s <- ctgrz_oth(s, 20, "uses his", 41, "Use_Farmers" )
s <- ctgrz_oth(s, 20, "uses my", 41, "Use_Farmers" )


s <- ctgrz_oth(s, 20, "compare", 41, "We_Compare_Tins" )

s <- ctgrz_oth(s, 20, "look", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "knows", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "seeing", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "obser", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "see", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "quality", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "exper", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "measur", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "gorogoro", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "varies", 41, "Use_farmers_experience_or_look")
s <- ctgrz_oth(s, 20, "comper", 41, "Use_farmers_experience_or_look")



s <- ctgrz_oth(s, 20, "none", 41, "Farmer_Does_Not_know" )
s <- ctgrz_oth(s, 20, "doesn", 41, "Farmer_Does_Not_know" )
s <- ctgrz_oth(s, 20, "hard", 41, "Farmer_Does_Not_know" )



s <- ctgrz_oth(s, 20, "bag", 41, "Sell_Other_Units" )
s <- ctgrz_oth(s, 20, "ilo", 41, "Sell_Other_Units" )
s <- ctgrz_oth(s, 20, "sack", 41, "Sell_Other_Units" )



# col 42, 25 (concatenate with 24)
looksy(s,25)

s <- ctgrz_oth(s, 25, "conv", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "time", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "acces", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "quick", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "conv", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "only", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "reaches", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "place", 42, "Convienince" )
s <- ctgrz_oth(s, 25, "emergency", 42, "Convienince" )


s <- ctgrz_oth(s, 25, "bulk", 42, "Bulk" )
s <- ctgrz_oth(s, 25, "sell more", 42, "Bulk" )



s <- ctgrz_oth(s, 25, "high", 42, "Better_Price" )
s <- ctgrz_oth(s, 25, "good", 42, "Better_Price" )
s <- ctgrz_oth(s, 25, "flexible", 42, "Better_Price" )
s <- ctgrz_oth(s, 25, "Negot", 42, "Better_Price" )
s <- ctgrz_oth(s, 25, "better", 42, "Better_Price" )
s <- ctgrz_oth(s, 25, "Good", 42, "Better_Price" )


s <- ctgrz_oth(s, 25, "cheap", 42, "Cheaper_Costs" )
s <- ctgrz_oth(s, 25, "costs", 42, "Cheaper_Costs" )
s <- ctgrz_oth(s, 25, "cheap", 42, "Cheaper_Costs" )
s <- ctgrz_oth(s, 25, "home", 42, "Cheaper_Costs" )
s <- ctgrz_oth(s, 25, "transport", 42, "Cheaper_Costs" )


s <- ctgrz_oth(s, 25, "none", 42, "N/A" )
s <- ctgrz_oth(s, 25, "N/A", 42, "N/A" )
s <- ctgrz_oth(s, 25, "n.a", 42, "N/A" )
s <- ctgrz_oth(s, 25, "n/a", 42, "N/A" )


s <- ctgrz_oth(s, 25, "school", 42, "School_Fees" )


looksy(s,25)

# col 43, 26
   
## rm other


## select




data <- s %>%
   filter(consent == "Yes") %>%
   select(36, 37,35,4:6,8,10,11,13,14,16,17,18,19,41,21,22,23,24,42,43)


#### print question labels file ####
#### ----------------------------------------------------------------------------------

write.csv(data, file = "data.csv", row.names = FALSE)


question_labels <- ql_create(data, c(5:11,14:17,19:22))

write.csv(question_labels, file = "question_labels.csv", row.names = FALSE)

   
   




