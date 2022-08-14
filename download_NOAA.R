download_NOAA <- function(station, dest.path, start = 1973, end = 2021){
    # used to download data
    # only to be run once
    
    # Station is USAF ID
    # station numbers can be found here:  
    #   ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
    # for example, Legazpi weather station's USAF ID is 984440
    
    # dest.path is where you want to store the files
    # for example: 
    ## dest.path <- "C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/NOAA/daily extracted"
    
    require(RCurl) # for locating a url path
    url <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/"

    
    for (year in start:end){
        # locating source
        filename <- paste0(station,"-99999-",year,".op.gz")
        loc <- paste0(url,year,"/",filename)
        
        # creating destination
        dest <- file.path(dest.path,year,filename)
        
        # downloading
        download.file(loc,dest)
    } 
}