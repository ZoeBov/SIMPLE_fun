# SIMPEL Water Balance model

## The model at a glance

This is an R implementation of the SIMPEL Water Balance Model (Hörmann, 1996, Hörmann et al., 2007).

The SIMPEL model is - as the title suggest - a simple, one dimensional soil water models. It can be used  to calculate the soil water balance (evapotranspiration, groundwater recharge, soil water content) of a soil column. The SIMPEL model covers the low end of hydrological model. 

The version in this repository is an Excel to R translation, which is based on the most basic version of the model. Snow accumulation and melt have been added too. Evapotranspiration is computed either by the internal Haude approach or by external time series (e.g., crop reference evapotranspiration).

List of files in subfolder `R`:

* `SIMPLE_Fun.R` inlcudes the `SIMPLE_function()`, which includes the full model. It solves the water balance on daily time steps and requires meteorological input, landuse parameters, information about the annual LAI cycle, and model parameters (see below).
* `Main_SIMPLE.R` is a sample file which demonstrates how to run the model (including function call). It creates a file `SIMPLE_bucket_model.xlsx` with a relevant results upon completion.
* `LAI_model.txt` includes a table, first column is an index, second column represents Day of Year values and the third column indicates whether a winter (low) or summer (high) value for LAI is applied (see values in `Land_Use.txt`.
* `Land_Use.txt` provides relevant land use-dependent parameters for some typical land use classifications. Columns represent land use classes and rows list relevant parameters (names in first column). The first 12 rows include multipliers for the Haude potential evapotranspiration model (1 value per month).
* `Meteo_Input.txt` is a sample meteorological forcing file. Column meanings are: Date, Day of year, daily precipitation total (mm/d), Temperature in the afternoon (°C), relative humidity in the afternoon (%). An optional 6th column includes daily potential evapotranspiration totals (mm/d), e.g., derived from crop reference evapotranspiration. If this column does not exist, evapotranspiration is computed internally using the Haude approach.
* `Soil_physics.txt` is a csv file with the most relevant model parameters of the (soil) model. First column is a short descriptor, second column represents corresponding values used by the model, while the third column shows the units.

The model is published under GNU copyleft and is provided as is without any warranty.

## References
* Hörmann, G., 1997: SIMPEL - ein einfaches, benutzerfreundliches Bodenwassermodell zum Einsatz in der Ausbildung. *Deutsche Gewässerkundliche Mitteilungen* 41(2):67-72
*  Hörmann, G., X. Zhang and N. Fohrer (2007): Comparison of a simple and a spatially distributed hydrologic model for the simulation of a lowland catchment in Northern Germany. *Ecological Modelling*, 209 (1): 21-28.