\name{SIMPLE_fun}
\alias{SIMPLE_function}
\title{
Run the SIMPLE bucket model
}
\description{
This function simulates water flow through a bucket model based on meteorological and geographical input parameters.
}
\usage{
SIMPLE_function(Input, Landuse, LAI_model, Soil, ETP0 = NULL)
}
\arguments{
    \item{Input}{
    Meteo_Input.txt is a sample meteorological forcing file. Column meanings are: Date, Day of year, daily precipitation total (mm/d), 
    Temperature in the afternoon (°C), relative humidity in the afternoon (%). An optional 6th column includes daily potential evapotranspiration totals (mm/d),
    e.g., derived from crop reference evapotranspiration. If this column does not exist, evapotranspiration is computed internally using the Haude approach.
  }
  \item{Landuse}{
    Land_Use.txt provides relevant land use-dependent parameters for some typical land use classifications. Columns represent land use classes and rows list relevant parameters
    (names in first column). The first 12 rows include multipliers for the Haude potential evapotranspiration model (1 value per month).
  }
  \item{LAI_model}{
    LAI_model.txt includes a table, first column is an index, second column represents Day of Year values and the third column indicates 
    whether a winter (low) or summer (high) value for LAI is applied (see values in Land_Use.txt.
  }
  \item{Soil}{
    Soil_physics.txt is a csv file with the most relevant model parameters of the (soil) model. First column is a short descriptor, 
    second column represents corresponding values used by the model, while the third column shows the units.
  }
}
\details{
The function initializes parameters and computes each row iteratively to simulate water balance components, including snowmelt, precipitation, evapotranspiration, and runoff.
}
\value{
A data frame (in the global environment) named \code{bucket_model} summarizing model results with columns for each water balance component, such as precipitation, evapotranspiration, storage, and runoff.
}
\references{
Original Excel spreadsheet developed by Georg Hörmann; extended by Kristian Förster and translated to R by Zoe E. Bovermann.
}
\author{
Kristian Förster, Zoe E. Bovermann
}
\note{
Ensure that all input data frames are formatted correctly, as incorrect inputs can result in errors or inaccurate calculations.
}
\seealso{
\code{\link{read.table}}, \code{\link{as.data.frame}}, \code{\link{lubridate}}
}
\examples{
# Example input data frames
Input <- data.frame(Date = as.Date("2023-01-01"), Precipitation = 10, Temp = 5, ...)

# Run the SIMPLE model function
SIMPLE_function(Input, Landuse, LAI_model, Soil)
}
\keyword{hydrology}
\keyword{modeling}
\concept{water balance}
\concept{bucket model}

