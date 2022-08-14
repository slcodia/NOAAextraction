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
        
        file <- paste0(station,"-99999-",year,".op.gz") # file name
        file.path0 <- file.path(path,year,file) # complete file path
        
        if (file.exists(file.path0)){
            data <- read.fwf(gzfile(file.path0),
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
                message(year," done!")
            }   
        }else{
            if (verbose){
                message(year," data does not exist.")
            }   
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
    
    # Filling date columns
    data0$YEAR  <- lubridate::year(data0$DATE)
    data0$MONTH <- as.factor(lubridate::month(data0$DATE))
    data0$DAY   <- lubridate::day(data0$DATE)
    
    message("DATA EXTRACTION SUCCESSFUL!")
    
    if (verbose) {
        # running time
        time <- Sys.time() - begin.tiid
        print(time)
        
        if (!require("beepr",character.only = TRUE)==FALSE){
            # for the sound effect. Only run if beepr is installed
            require(beepr)
            beep()
        }
        
        
        # counting missing dates
        missing <- sum(is.na(data0$STN))
        message("A total of ",missing," days are missing from the data for the years ",start," to ",end,".")
        
        # count missing per variable
        q <- menu(title = "Show number of missing values per year?",
                  choices = c("Yes", "No"))
        
        if (q==1){
            missing.data <- data0 %>% 
                group_by(YEAR) %>%
                summarize(DAYS = sum(is.na(STN)),
                          PRCP = sum(is.na(PRCP)),
                          MIN = sum(is.na(MIN)),
                          TEMP = sum(is.na(TEMP)),
                          MAX = sum(is.na(MAX)),
                          DEWP = sum(is.na(DEWP)),
                          SLP = sum(is.na(SLP)),
                          VISIB = sum(is.na(VISIB)),
                          WDSP = sum(is.na(WDSP)))
            View(missing.data)
        }

    }
    
    #filling station columns
    data0$STN   <- station
    
    # final data
    return(data0)
}
