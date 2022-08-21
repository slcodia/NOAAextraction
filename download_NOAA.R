getwd()
download_NOAA <- function(station, dest.path =NULL, start = 1950, end = 2021, 
                          timeout = 60, overwrite = F,verbose = FALSE){
    # used to download data
    # only to be run once
    
    # Station is USAF ID
    # station numbers can be found here:  
    #   ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
    # for example, Legazpi weather station's USAF ID is 984440
    
    # dest.path is where you want to store the files
    # for example: 
    ## dest.path <- "C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/NOAA/daily extracted"

    
    if (is.null(dest.path)){
        q <- menu(c(paste("Yes. Download data to",getwd()),
                          "No. I will provide another destination path."),
                    title = paste("dest.path is not specified. Extract to current directory?"))
        if(q==1){
            dest.path <- getwd()
        }else{stop("Specify your destination path before proceeding.")}
    }
    
    if (verbose){begin.tiid <- Sys.time()}
    
    require(RCurl) # for locating a url path, for url.exists
    require(curl)
    
    
    ftp_base <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/%s/"
    
    dir_list_handle <- new_handle(ftp_use_epsv=FALSE, dirlistonly=TRUE, crlf=TRUE,
                                  ssl_verifypeer=FALSE, ftp_response_timeout=30)
    
    
    
    # creating a safe function for curl_fetch_memory
    
    s_curl_fetch_memory <- safely(curl_fetch_memory)
    
    for (year in start:end){
        # success indicator: s = 0 fail, s = 1 success
        s = 0
        
        # fetch the year
        year_url <- sprintf(ftp_base, year)
        
        # FETCHING FILE NAMES
        time.start <- Sys.time()
        repeat {
            
            
            # curl_fetch_memory write data from URL into a memory (using the safe cfm function)
            res <- s_curl_fetch_memory(year_url, handle=dir_list_handle)
            
            # if result of fetching fails, process will repeat
            if (!is.null(res$result)){
                tmp <- res$result
                con <- rawConnection(tmp$content) # create connection
                files <- readLines(con) # list of the files in the folder
                close(con) # close connection to reduce memory
                s = 1
                
                break # break repeat if connection has been found
            }
            
            # until when will the repeat loop run?
            if (difftime(Sys.time(), time.start, units = "secs")>timeout){ 
                
                message("Fetching memory failed at year ", year," due to timeout.")
                break
            }
        }
        
        # if connection to ftp has been found
        if (s == 1){
            if (verbose){
                message(paste0("Successfuly connected to year folder ",year, ". Will now find file..."))
            }
            
            
            # location of destination
            year_loc <- file.path(dest.path0,year)
            
            # file name to extract
            filename <- paste0(station,"-99999-",year,".op.gz")
            
            # file to download
            loc <- file.path(year_url,filename)
            
                
            
            
            # locating if file exists
            if (filename %in% files){
                if(verbose){
                    message(paste("File",filename,"found. Proceeding to download..."))
                }
                # Proceeding to download...
                
                # put file on cache
                cache_file <- file.path(year_loc,filename)
                
                #skip download if cache_file already exists (depending on the value of `overwrite`)
                if(file.exists(cache_file)&(overwrite==T)){
                    message(paste(filename,"already exists in local folder. Overwriting..."))
                    
                    d <- tryCatch(download.file(loc,cache_file), 
                                  error = function(e) print(paste("Download failed for year",year)))
                    if(d == 0){
                        if(verbose){print(paste(year, "successfuly overwritten!"),quote = F)}
                    }
                    
                }else if(file.exists(cache_file)&(overwrite==F)){
                    if(verbose){message(paste(filename,"already exists in local folder. Skipping download..."))}
                    next
                    
                }else if(!file.exists(cache_file)){
                    d <- tryCatch(download.file(loc,cache_file), 
                                  error = function(e) print(paste("Download failed for year",year)))
                    if(d == 0){
                        if(verbose){print(paste(year, "successfuly downloaded!"), quote = F)}
                    }
                }
                
                
            }else{
                print(paste("Data for year", year,"does not exist."), quote = F)
            } 
        }else{
            
            stop("Connection to ftp server lost. Server may be under loaded.")
        }
        
        
    }
    
    if (verbose) {
        runtime <- difftime(Sys.time(),begin.tiid, units = "secs")
        if (runtime < 120){
            print(paste("Running time:",round(runtime,2),"seconds."), quote = F)
        }else{
            print(paste("Running time:",round(runtime/60,2),"minutes."), quote = F)
        }
    }
        
}



