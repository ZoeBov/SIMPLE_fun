# SIMPEL Water Balance model

## The model at a glance

This is an R implementation of the SIMPEL Water Balance Model (Hörmann, 1996, Hörmann et al., 2007).

The SIMPEL model is - as the title suggest - a simple, one dimensional soil water models. It can be used  to calculate the soil water balance (evapotranspiration, groundwater recharge, soil water content) of a soil column. The SIMPEL model covers the low end of hydrological model. 

The version in this repository is an Excel to R translation, which is based on the most basic version of the model. Snow accumulation and melt have been added too. Evapotranspiration is computed either by the internal Haude approach or by external time series (e.g., crop reference evapotranspiration).

The model is published under GNU copyleft and is provided as is without any warranty.

## References
* Hörmann, G., 1997: SIMPEL - ein einfaches, benutzerfreundliches Bodenwassermodell zum Einsatz in der Ausbildung. *Deutsche Gewässerkundliche Mitteilungen* 41(2):67-72
*  Hörmann, G., X. Zhang and N. Fohrer (2007): Comparison of a simple and a spatially distributed hydrologic model for the simulation of a lowland catchment in Northern Germany. *Ecological Modelling*, 209 (1): 21-28.