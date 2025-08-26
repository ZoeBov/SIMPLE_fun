## Main SIMPLE
rm(list=ls())

#### set working directory + file directory and load data ####

data_file <- "Global_Input.txt"
land_use <- "Land_use.txt"
LAI_model <- "LAI_model.txt"
Soil <- "Soil_physics.txt"
output <- 'SIMPLE_bucket_model.xlsx'

# import Global Input data
Input <-  read.table(file =paste0(data_file), sep = "", header = T)
Landuse <- read.table(file =paste0(land_use), sep = "", header = T)
LAI_model <- read.table(file =paste0(LAI_model), sep = "", header = F)
Soil <- read.table(file =paste0(Soil), sep = ";", header = F)

# print status
print("done: load input parameters")


#### run bucket-model ####
source("SIMPLE_fun.R")
SIMPLE_function(as.data.frame(Input), as.data.frame(Landuse),
             as.data.frame(LAI_model), as.data.frame(Soil))

# print status
print("done: running bucket-model")


#### save the solution ####
print(output)
write_xlsx(bucket_model, output)
