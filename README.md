# weather-fetchr

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)


Fetch NOAA weather data using coordinates in R

---

## Overview

This collection of functions enables users to pull weather data using the ACIS RCC query builder.
Designed for researchers who want to pair participant GPS coordinates with NOAA weather data (e.g., for those interested in how weather may impact movement patterns).
Coordinates (latitude, longitude) and a list of NOAA stations (which can be derived using [FluMoDL](https://github.com/thlytras/FluMoDL)) are required as input.

## Acronyms

 - **ACIS** = Applied Climate Information System
 - **RCC** = Regional Climate Center, part of NOAA
 - **NOAA** = National Oceanic and Atmospheric Administration
 - **WBAN** = Weather Bureau Army Navy identifier, five digit numeric code that identifies a weather station
 - **ICAO** =  International Civil Aviation Organization identifier, four digit alphanumeric identifier that identifies a weather station
 
## Links & References

- [Weather station code abbreviations](https://mrcc.purdue.edu/CLIMATE/stnchooserIdDescrip.jsp)  
- [RCC ACIS documentation](https://www.rcc-acis.org/docs_webservices.html)  
- [RCC ACIS query builder](https://builder.rcc-acis.org/)  


## Units of Measurement

 - All temperatures are denoted *by default* in **Fahrenheit**
 - Snow, snow depth, and precipitation are all denoted *by default* in **inches**


## Example Usage


## Citation

If you use this code, please cite it as described in [CITATION.cff](./CITATION.cff).

