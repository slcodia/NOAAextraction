

read_NOAA <- function(station, path, start = 1950, end = 2021, verbose = FALSE){
    
    if (verbose){begin.tiid <- Sys.time()}
    
    # `station` is USAF ID
    # station numbers can be found here:  
    #   ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
    # for example, Legazpi weather station's USAF ID is 984440
    
    # `path` is the location of the data
    # for example: 
    ## path <- "C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/NOAA/daily extracted"
    
    
    # Initialize and dataframe
    require(tidyverse)
    
    data0 <- dplyr::tibble(
        `STN`       = character(),
        `YEAR`      = character(), 
        `MONTH`     = character(), 
        `DAY`       = character(), 
        `PRCP`      = numeric(), 
        `PRCP.Flag` = character(),
        `MIN`       = numeric(), 
        `TEMP`      = numeric(), 
        `MAX`       = numeric(), 
        `DEWP`      = numeric(), 
        `SLP`       = numeric(), 
        `VISIB`     = numeric(), 
        `WDSP`      = numeric(),
        `DATE`      = date() 
    )
    
    for (year in start:end){
        path0 <- file.path(path,year)
        file <- paste0(station,"-99999-",year,".op.gz")
        data <- read.fwf(gzfile(file.path(path0,file)),
                         widths = c(6, #STN
                                    6, #WBAN
                                    6, #YEAR
                                    2, #MONTH
                                    2, #DAY
                                    8, #TEMP
                                    3, #Count
                                    8, #DEWP
                                    3, #Count
                                    8, #SLP
                                    3, #Count
                                    8, #STP
                                    3, #Count
                                    7, #VISIB
                                    3, #Count
                                    7, #WDSP
                                    3, #Count
                                    7, #MXSPD
                                    7, #GUST
                                    8, #MAX
                                    1, #Flag
                                    7, #MIN,
                                    1, #Flag
                                    6, #PRCP
                                    1, #Flag
                                    6, #SNDP
                                    8  #FRSHTT
                         )
                         , header = FALSE, skip = 1)
        # renaming columns
        names(data) <- c("STN", "WBAN","YEAR", "MONTH","DAY", 
                         "TEMP", "TEMP.Count", 
                         "DEWP", "DEWP.Count", 
                         "SLP", "SLP.Count", 
                         "STP", "STP.Count", 
                         "VISIB", "VISIB.Count", 
                         "WDSP", "WDSP.Count", 
                         "MXSPD", "GUST", 
                         "MAX", "MAX.Flag",
                         "MIN", "MIN.Flag",
                         "PRCP","PRCP.Flag",
                         "SNDP", 
                         "FRSHTT")
        # creating date column
        
        
        # getting the required columns only
        data <- data[,c("STN","YEAR", "MONTH", "DAY", 
                        "PRCP", "PRCP.Flag",
                        "MIN", "TEMP", "MAX", 
                        "DEWP", "SLP", "VISIB", "WDSP")]
        
        # creating Date column
        # disabling warning temporarily
        defaultW <- getOption("warn") 
        options(warn = -1) 
        data <- mutate(data, DATE = lubridate::make_date(YEAR,MONTH,DAY))
        options(warn = defaultW)
        
        
        data0 <- rbind(data0,data)
        
        if (verbose){
            print(paste(year,"done!"))
        }
    }
    
    ## Date: Creating a sequence
    
    min_year <- as.Date(paste0(start, "-01-01"))
    max_year <- as.Date(paste0(end, "-12-31"))
    sequence <- data.frame(DATE = seq(min_year, max_year, by = 1))
    
    data0 <- merge(x = sequence,
                   y = data0,
                   by = 'DATE',
                   all = TRUE)
    
    # counting missing dates
    
    if (verbose){
        missing <- sum(is.na(data0$STN))
        print(paste("A total of",missing,"days are missing from the data."))
    }
    
    #filling station and date columns
    
    data0$STN   <- station
    data0$YEAR  <- lubridate::year(data0$DATE)
    data0$MONTH <- as.factor(lubridate::month(data0$DATE))
    data0$DAY   <- lubridate::day(data0$DATE)
    
    
    
    # Cleaning invalid data and converting
    
    ## Precipitation: inches to mm
    data0$PRCP<- replace(data0$PRCP, data0$PRCP==99.99|data0$PRCP.Flag=="I",NA)*25.4
    data0$PRCP<- replace(data0$PRCP, data0$PRCP==0&data0$PRCP.Flag=="H",NA)
    
    ## Temperature: Farenheit to Celsius
    data0$MIN  <- replace(data0$MIN,  data0$MIN ==9999.9, NA)
    data0$MIN  <- (data0$MIN-32)*5/9
    
    data0$TEMP <- replace(data0$TEMP, data0$TEMP==9999.9, NA)
    data0$TEMP <- (data0$TEMP-32)*5/9
    
    data0$MAX  <- replace(data0$MAX,  data0$MAX ==9999.9, NA)
    data0$MAX  <- (data0$MAX-32)*5/9
    
    ## Dew point: Farenheit to Celsius
    data0$DEWP <- replace(data0$DEWP,data0$DEWP==9999.9, NA)
    data0$DEWP <- (data0$DEWP-32)*5/9
    
    ## Sea Level Pressure: millibars
    data0$SLP <- replace(data0$SLP,data0$SLP==9999.9, NA)
    
    ## Visibility: miles to kilometer
    data0$VISIB <- replace(data0$VISIB,data0$VISIB==999.9, NA)
    data0$VISIB <- data0$VISIB*1.609344
    
    ## Wind Speed
    data0$WDSP <- replace(data0$WDSP,data0$WDSP==999.9, NA)
    
    print("DATA EXTRACTION SUCCESSFUL!")
    if (verbose) {
        time <- Sys.time() - begin.tiid
        print(time)
        
    }
    return(data0)
}
