## Main SIMPLE
#### set working directory + file directory and load data ####

data_file <- "R/Meteo_Input.txt"
land_use <- "R/Land_use.txt"
LAI_model <- "R/LAI_model.txt"
Soil <- "R/Soil_physics.txt"
output <- 'R/SIMPLE_bucket_model.xlsx'

# import Global Input data
Input <-  read.table(file =paste0(data_file), sep = "", header = T)
Landuse <- read.table(file =paste0(land_use), sep = "", header = T)
LAI_model <- read.table(file =paste0(LAI_model), sep = "", header = F)
Soil <- read.table(file =paste0(Soil), sep = ";", header = F)

# print status
print("done: load input parameters")

# Ensure SIMPLE_fun.R is sourced from the correct location
# source("R/SIMPLE_fun.R")

#### run bucket-model ####
bucket_model <- SIMPLE_function(as.data.frame(Input), as.data.frame(Landuse),
             as.data.frame(LAI_model), as.data.frame(Soil))

# print status
print("done: running bucket-model")


#### save the solution ####
print(output)
write_xlsx(bucket_model, output)
