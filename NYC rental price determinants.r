
library(readr)
library(stringr)

List_of_file_paths <- list.files(path ="C:/Users/tuncay/Documents/NYC raw data/", pattern = ".csv", all.files = TRUE, full.names = TRUE)
List_of_file_paths

df <- list.files(full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

head(df)

library(tor)

list_csv()

load_csv()

head(subboroughareacarfreecommuteofcommuters)

head(subboroughareahousingunits)

head(subboroughareaunemploymentrate)

fulldata <- read.csv("C:/Users/tuncay/Documents/NYC raw data/nyc_pluto_18v2_1_csv/pluto_18v2_1.csv")

colnames(fulldata)

head(fulldata["bldgarea"])
nrow(fulldata["bldgarea"])

partialdata <- fulldata[]

fulldata %>% filter(bldgclass==c("A4","R1","R2","R3","R4","R6"))
