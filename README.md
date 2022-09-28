# Extracting NOAA data using R and Python


This repository shows how I extract daily weather data from NOAA using R and Python. Another repository will be made in the future where I will be using Python.

Data is extracted from ftp://ftp.ncdc.noaa.gov/pub/data/gsod/. Copy this [link](ftp://ftp.ncdc.noaa.gov/pub/data/gsod/) to your file explorer to access station data from 1950 to present.

Station numbers can be found here:  
    ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
For example, Legazpi weather station's USAF ID is 984440

Custom functions:
1. `download_NOAA.R` : used to download files to a local folder. Only to be run once, except if data needs to be updated.
2. `read_NOAA.R`: used to read and store data to R. This will create a data frame with complete dates, even those that are not in the dataset, as long as within the range of the pre-determined start and end years.

Sample data cleaning and analyses are also featured here.

`Data for LARS-WG.R` shows the interpolation I did for the Legaspi data in preparation to downscaling using LARS-WG.
Final data can be found in the `Legaspi_stationary.dat` file.
