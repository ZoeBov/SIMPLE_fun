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
  A data frame containing meteorological input parameters required by the model, with the following columns:
  \describe{
    \item{Date}{A date column formatted as \code{YYYY-MM-DD}.}
    \item{DOY}{Day of the year as an integer (1 to 365 or 366).}
    \item{P}{Precipitation in millimeters (\code{mm}).}
    \item{T14}{Temperature in degrees Celsius.}
    \item{R14}{Relative humidity.}
    \item{ET0}{Potential evapotranspiration in (\code{mm}).}
  }
  }
  \item{Landuse}{
  A data frame specifying land use parameters necessary for model predictions.
  }
  \item{LAI_model}{
  A data frame containing Leaf Area Index model parameters used to calculate evapotranspiration and other processes.
  }
  \item{Soil}{
  A data frame with soil physics parameters required for the simulation of infiltration, storage, and runoff.
  }
  \item{ETP0}{
  (Optional) A numeric value specifying the initial evapotranspiration. If not provided, it is calculated within the function.
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
