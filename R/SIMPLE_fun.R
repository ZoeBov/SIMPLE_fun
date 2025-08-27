# SIMPLE_function
# original Excel spreadsheet developed by Georg Hörmann
# extended (snow, surface runoff) by Kristian Förster 2022
# translated to R by Zoe Bovermann 2023

SIMPLE_function <- function(Input, Landuse, LAI_model, Soil) {

  # Load libraries
  suppressMessages(library(data.table))
  suppressMessages(library(lubridate))
  suppressMessages(library(writexl))
  # prepare bucket model matrix
  bucket_model <- as.data.frame(matrix(nrow = nrow(Input), ncol = 29))
  colnames(bucket_model) <- c("Date", "Precipitation", "Snow_Water_Equi", "Snow_melt+rain",
                              "ETP_Coeff", "ETP_Input", "LAI", "I-Cap", "Int. ETi(leaf)","I-Bal",
                              "I-Prec","I-Rem", "I_ETi(litter)", "Bilanz", "Content",
                              "S-REstn", "Inf-Limit", "P-linf", "Rest-ETA", "Balance_soil",
                              "ETa", "ET-Balance", "Seepage",	"Storage",	"Surface_runoff",
                              "Runoff_total", "I-Leaf",	"I-Litter",	"ETaTotal")

  # copy Input into bucket-model
  #col 1: A Date
  bucket_model$Date <- Input[1]
  # col 2: B Precipitation
  bucket_model$Precipitation <-  Input[,3]

  
  # Parameter numbers in soil_physics.txt
  N_LAYERTHICK <-1
  N_FIELD_CAP <- 2 #6
  N_PWP <- 3 # 7 
  N_STARTRED <-4 #9
  N_INITSTOR <- 5 #10
  N_LANDUSE <- 6 #12
  N_GWR <- 7 #17
  #N_LITTERCAP <- 7 #18
  N_LITTERRED <- 8

  # initialize model
  dz <- as.numeric(Soil[N_LAYERTHICK,2])
  fc <- dz*as.numeric(Soil[N_FIELD_CAP,2])/100
  pwp <- dz*as.numeric(Soil[N_PWP,2])/100
  start_of_red <- dz*as.numeric(Soil[N_STARTRED,2])/100
  init_stor  <- dz*as.numeric(Soil[N_INITSTOR,2])/100
  glugla_c <- as.numeric(Soil[N_GWR,2])
  lambda <- glugla_c / dz^2
  
  # water balance check
  sum_prec   <- 0.
  sum_etr    <- 0.
  sum_runoff <- 0.
  init_swe   <- 0.
  
  # read LAI from landuse (and not soil physics)
  LAI_model[1,'V3'] = Landuse[14,which(colnames(Landuse)==Soil[N_LANDUSE,2])] # LAI min
  LAI_model[2,'V3'] = Landuse[15,which(colnames(Landuse)==Soil[N_LANDUSE,2])] # LAI max
  LAI_model[3,'V3'] = Landuse[15,which(colnames(Landuse)==Soil[N_LANDUSE,2])] # LAI max
  LAI_model[4,'V3'] = Landuse[14,which(colnames(Landuse)==Soil[N_LANDUSE,2])] # LAI min
  #browser()
  # update Litter according to land use table
  # Soil[18,2] = Landuse[16,which(colnames(Landuse)==Soil[12,2])] # Litter capacity

  # calculate the rest
  for(krow in 1:nrow(bucket_model)){

    # col 3+4:
    # first row
    if(krow==1){
      # col 3: C snow water equi
      bucket_model[krow,3] <- 0 # col 3

      # col 4: D Snow melt + rain
      temp <- c(0, Landuse[17,which(colnames(Landuse)==Soil[N_LANDUSE,2])]*Input[krow,4])
      if(Input[krow,4] >= 0){
        bucket_model[krow,4] <- min(0,c(max(temp))) + bucket_model[krow,2]}
      else{
        bucket_model[krow,4] <- min(0,c(max(temp))) + 0}

      # col 3 (dependent on col 4)
      bucket_model[krow,3] <- bucket_model[(krow),3] + bucket_model[krow,2] - bucket_model[krow,4]
    }

    # rest of the rows
    if(krow > 1){

      temp <- c(0, Landuse[17,which(colnames(Landuse)==Soil[N_LANDUSE,2])]*Input[krow,4])
      if(Input[krow,4] < 0){
        # col 4: Snow melt + rain
        bucket_model[krow,4] <- min(c(max(temp), (bucket_model[(krow-1),3])+(bucket_model[krow,2])))

        # col 3 (dependent on col 4)
        bucket_model[krow,3] <- bucket_model[(krow-1),3] + bucket_model[krow,2] - bucket_model[krow,4]+0
      }
      else{
        bucket_model[krow,4] <- min(c(max(temp),(bucket_model[(krow-1),3])+0)) + bucket_model[krow,2]
        bucket_model[krow,3] <- bucket_model[(krow-1),3] + 0 - bucket_model[krow,4] +  bucket_model[krow,2]
      }
    }

    # col 5: E ETP Coeff.Landuse[17,which(colnames(Landuse)==Soil[12,2])]
    bucket_model[krow,5] <- Landuse[which(Landuse[,1] == as.character(month(dmy(bucket_model[krow,1])))),
                                    which(colnames(Landuse)==Soil[N_LANDUSE,2])]

    # col 6: F ETP Input
    if(ncol(Input) >= 6){   # check if input variable is given
      # if yes, use given ETP0
      bucket_model[krow,6] <- Input[krow,6]
      if(krow==2)print('Reading external ETP from file...')
    }
    else { # if not calculate ETP
      bucket_model[krow,6] <- min(7, bucket_model[krow,5]*6.11*10^((7.5*Input[krow,4])/
                                                                     (237.3+Input[krow,4]))*(1-(Input[krow,5]/100)))
    }

    # col 7: G LAI
    if(Input[krow,2] <= LAI_model[1,2]){
      bucket_model[krow,7] <- LAI_model[1,3]
    }
    else if(Input[krow,2] <= LAI_model[2,2]){
      bucket_model[krow,7] <- Landuse[14,which(colnames(Landuse)==Soil[N_LANDUSE,2])]+(LAI_model[2,3]-LAI_model[1,3])*((Input[krow,2]-LAI_model[1,2])/
                                                                                        (LAI_model[2,2]-LAI_model[1,2]))
    }
    else if(Input[krow,2] <= LAI_model[3,2]){
      bucket_model[krow,7] <- LAI_model[3,3]
    }
    else if(Input[krow,2] <= LAI_model[4,2]){
      bucket_model[krow,7] <- Landuse[15,which(colnames(Landuse)==Soil[N_LANDUSE,2])]+(LAI_model[4,3]-LAI_model[3,3])*((Input[krow,2]-LAI_model[3,2])/
                                                                                        (LAI_model[4,2]-LAI_model[3,2]))
    }
    else{
      bucket_model[krow,7] <- LAI_model[4,3]
    }

    # col 8: H I-Cap
    bucket_model[krow,8] <- 0.35*bucket_model[krow,7]

    # col 9: I ETi
    bucket_model[krow,9] <- min(c(bucket_model[krow,8], bucket_model[krow,6]))

    # col 10: J I-Bal.
    bucket_model[krow,10] <- bucket_model[krow,4] - bucket_model[krow,9]

    # col 11: K I-Prec.
    bucket_model[krow,11] <- max(c(bucket_model[krow,10],0))

    # col 12: L I-Rem.
    bucket_model[krow,12] <- -min(c(bucket_model[krow,10],0))-bucket_model[krow,9]+bucket_model[krow,6]

    # col 13+14+15
    # first row
    litter_cap<-Landuse[16,which(colnames(Landuse)==Soil[N_LANDUSE,2])]
    if(krow==1){
      # col 13: M ETi Litter
      temp <- c(litter_cap, (0+bucket_model[krow,11])/as.numeric(Soil[N_LITTERRED,2]))
      bucket_model[krow,13] <- min(c(bucket_model[krow,12],min(temp)))

      # col 14: N Bilanz (needed for col 13 & 15)
      bucket_model[krow,14] <- 0 + bucket_model[krow,11] - bucket_model[krow,13]

      # col 15: O Content (needed for col 13 & 14)
      if(bucket_model[krow,14] > litter_cap){
        bucket_model[krow,15] <- litter_cap
      }
      else bucket_model[krow,15] <- max(c(0,bucket_model[krow,14]))

    }
    else{# rest of the rows
      # col 13: M ETi Litter
      litter_cap = Landuse[16,which(colnames(Landuse)==Soil[N_LANDUSE,2])]
      temp <- c(litter_cap, (bucket_model[(krow-1),15]+bucket_model[krow,11])/as.numeric(Soil[N_LITTERRED,2]))
      bucket_model[krow,13] <- min(c(bucket_model[krow,12],min(temp)))

      # col 14: N Bilanz (needed for col 13 & 15)
      bucket_model[krow,14] <- bucket_model[(krow-1),15]  + bucket_model[krow,11] - bucket_model[krow,13]

      # col 15: O Content (needed for col 13 & 14)
      if(bucket_model[krow,14] > litter_cap){
        bucket_model[krow,15] <- litter_cap
      }
      else bucket_model[krow,15] <- max(c(0,bucket_model[krow,14]))
    }

    # col 16: P S-REstn
    bucket_model[krow,16] <- max(c(0, (bucket_model[krow,14]-bucket_model[krow,15])))

    # col 17-24:
    # first row
    if(krow==1){
      # col 17: Q Inf-Limit
      bucket_model[krow,17] <- (fc-init_stor)*0.25*(1-Landuse[13,which(colnames(Landuse)==Soil[N_LANDUSE,2])]/100)

      # col 18: R P-Inf
      bucket_model[krow,18] <- min(c(bucket_model[krow,16],bucket_model[krow,17]))*(1-Landuse[13,which(colnames(Landuse)==Soil[N_LANDUSE,2])]/100)

      # col 19: S S-Rest
      bucket_model[krow,19] <- -min(c(0,bucket_model[krow,14]))+bucket_model[krow,12]-bucket_model[krow,13]

      # col 20: T Balance soil
      bucket_model[krow,20] <- init_stor + bucket_model[krow,18]

      # col 21: U ETa
      if(bucket_model[krow,20] > start_of_red){
        bucket_model[krow,21] <-  bucket_model[krow,19]
      }
      else{
        bucket_model[krow,21] <-  bucket_model[krow,19]*(bucket_model[krow,20]-pwp)/
          (start_of_red - pwp)
      }

      # col 22: V ET-Balance
      bucket_model[krow,22] <- bucket_model[krow,20] - bucket_model[krow,21]

      # col 23: W Seepage
      if(bucket_model[krow,22] <= fc){
        bucket_model[krow,23] <-  lambda*(bucket_model[krow,22]-pwp)^2
      }
      else{
        bucket_model[krow,23] <-  lambda*(fc-pwp)^2
      }

      # col 24: X Storage Init.-Value
      if(bucket_model[krow,22] > fc){
        bucket_model[krow,24] <-  fc
      }
      else{
        bucket_model[krow,24] <- bucket_model[krow,22] - bucket_model[krow,23]
      }

    }
    else{
      # col 17: Q Inf-Limit
      bucket_model[krow,17] <- (fc-bucket_model[(krow-1),24])*0.25

      # col 18: R P-Inf
      direct_runoff <- Landuse[13,which(colnames(Landuse)==Soil[N_LANDUSE,2])]
      bucket_model[krow,18] <- min(c(bucket_model[krow,16],bucket_model[krow,17]))*(1-direct_runoff/100)

      # col 19: S S-Rest
      bucket_model[krow,19] <- -min(c(0,bucket_model[krow,14]))+bucket_model[krow,12]-bucket_model[krow,13]

      # col 20: T Balance soil
      bucket_model[krow,20] <- bucket_model[(krow-1),24] + bucket_model[krow,18]

      # col 21: U ETa
      if(bucket_model[krow,20] > start_of_red){
        bucket_model[krow,21] <-  bucket_model[krow,19]
      }
      else{
        bucket_model[krow,21] <-  bucket_model[krow,19]*(bucket_model[krow,20]-pwp)/
          (start_of_red - pwp)
      }

      # col 22: V ET-Balance
      bucket_model[krow,22] <- bucket_model[krow,20] - bucket_model[krow,21]

      # col 23: W Seepage
      if(bucket_model[krow,22] <= fc){
        bucket_model[krow,23] <-  lambda*(bucket_model[krow,22]-pwp)^2
      }
      else{
        bucket_model[krow,23] <-  lambda*(fc-pwp)^2
      }

      # col 24: X Storage Init.-Value
      if(bucket_model[krow,22] > fc){
        bucket_model[krow,24] <-  fc
      }
      else{
        bucket_model[krow,24] <- bucket_model[krow,22] - bucket_model[krow,23]
      }
    }
    # col 25: Y surface runoff
    if(bucket_model[krow,22] > fc){
      bucket_model[krow,25] <- bucket_model[krow,22]-fc+bucket_model[krow,16]-bucket_model[krow,18]
    }
    else
    {
      bucket_model[krow,25] <- bucket_model[krow,16]-bucket_model[krow,18]
    }


    # col 26: Z Runofftotal
    bucket_model[krow,26] <- bucket_model[krow,25] + bucket_model[krow,23]

    # col 27: AA I-Leaf
    bucket_model[krow,27] <- bucket_model[krow,6] -  bucket_model[krow,12]

    # col 28: AB I-Litter
    bucket_model[krow,28] <- bucket_model[krow,12] -  bucket_model[krow,19]

    # col 29: AC ETa Total
    bucket_model[krow,29] <- bucket_model[krow,28] + bucket_model[krow,27] + bucket_model[krow,21]

    sum_prec <- sum_prec + Input[krow,3]
    sum_etr  <- sum_etr  + bucket_model[krow,29]
    sum_runoff <- sum_runoff + bucket_model[krow,26]
    #assign("bucket_model","bucket_model",envir = globalenv())
    # return(bucket_model)
  }
  .GlobalEnv$bucket_model <- bucket_model
  # water balance check
  water_balance = sum_prec - sum_etr - sum_runoff + init_swe + init_stor - bucket_model[krow,24] - bucket_model[krow,3] - bucket_model[krow,15]
  print('water balance check')
  print(water_balance)
}
