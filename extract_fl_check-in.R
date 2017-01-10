# FL Check-In Extractor #
## Prepared by Jan Korevaar ###

# clear environment and screen
rm(list = ls())
cat("\014")

# libraries
options(java.parameters = "-Xmx8000m")
library(xlsx)
library(openxlsx)

# setwd

setwd("C:/Users/Jan/Documents")

FileNames <- list.files("Field Leader Check-Ins", pattern = "*.xl*", all.files = FALSE)
FileNames <- FileNames[!grepl("[~$]", FileNames)]

### set Parameters

st.row <- 6  # start row
st.column <- 2   #start column
end.column <- 4   #end column

#FileNames <- FileNames[2:4]

setwd("Field Leader Check-Ins")

for (i in 1:length(FileNames)){
 
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
       
     # read data
    check.in <- read.xlsx(FileNames[i], sheet = 1, startRow = st.row, skipEmptyRows = TRUE,
        cols = c(st.column:end.column))
    other.info <- read.xlsx(FileNames[i], sheet = 1, rows = c(2,3), colNames = FALSE)

  # extract date and district
    date <- as.Date(as.numeric(other.info[1,2]), origin = "1899-12-30")
    district <- other.info[2,2]

    # add to check in dataframe
    check.in$date <- date
    check.in$district <- district
    
    dataset<- bind_rows(dataset, check.in)
    
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    
     # read data
    check.in <- read.xlsx(FileNames[i], sheet = 1, startRow = st.row, skipEmptyRows = TRUE,
        cols = c(st.column:end.column))
    other.info <- read.xlsx(FileNames[i], sheet = 1, rows = c(2,3), colNames = FALSE)

  # extract date and district
    date <- as.Date(as.numeric(other.info[1,2]), origin = "1899-12-30")
    district <- other.info[2,2]

    # add to check in dataframe
    check.in$date <- date
    check.in$district <- district
    
    dataset <- check.in
    
      }
  
}

write.csv(dataset, file = "test.check-in.csv", row.names = FALSE)
#rm(dataset)


