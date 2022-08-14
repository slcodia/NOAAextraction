# NOAAextraction
Extracting NOAA data

This repository shows how I extract daily weather data from NOAA.
Data is extracted from ftp://ftp.ncdc.noaa.gov/pub/data/gsod/. Copy this link to your file explorer to access station data from 1950 to present.

Custom functions:
1. `download_NOAA.R` : used to download files to a local folder. Only to be run once
2. `read_NOAA.R`: used to read and store data to R. Will create a data frame with complete dates, even those that are not in the dataset, as long as within the range of the pre-determined start and end years.
