
download_NOAA <- function(station, dest.path =NULL, years, 
                          timeout = 60, overwrite = F,verbose = FALSE){
    
    #' @description 
    #' This functions automates downloading data from Global Surface Summary of the Day (GSOD) data
    #' provided by National Oceanic and Atmospheric Administration (NOAA)
    #' 
    #' Data is extracted from their FTP server: `ftp://ftp.ncdc.noaa.gov/pub/data/gsod/`
    #' 
    #' Additional notes:
    #' Only to be run once, unless you want to update your local data
    #' So far, this function only extracts ONE STATION at a time
    #' 
    #' @author Siegfred Roi L. Codia, \email{slcodia@@up.edu.ph}
        

    #' @param station is USAF ID
    #' station numbers can be found here: ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv
    #' for example, Legazpi weather station's USAF ID is 984440
    
    #' @param dest.path is where you want to store the files
    #' if `dest.path` is not specified, data will be downloaded to current working directory
    #' for example: 
    #' `dest.path <- "C:/Users/Siegfred Codia/Documents/Acads/MS Stat/Thesis/Data/NOAA/daily extracted"`
    
    #' @param years is a single number or vector of years
    #' for example, `years = 1973:2021`, `years = c(1995,2005,2015)`
    #' 
    #' @param timeout is the time in seconds until download attempt is considered failed 
    #' 
    #' @param overwrite determines if we should overwrite current files, or we skip
    #' tip: setting overwrite = T reduces download time and conserves bandwidth, 
    #' if we are sure that current files are updated
    #' 
    #' @param verbose tracks download progress
     
    
    #' @example 
    #' # Download data from Iloilo Weather station for years 1973-2021
    #' download_NOAA(station = 986460, dest.path = dest.path0, start = 1991, overwrite = F, timeout = 60,verbose = TRUE)
    
    # Clarifying download destination
    if (is.null(dest.path)){
        q <- menu(c(paste("Yes. Download data to",getwd()),
                    "No. I will provide another destination path."),
                  title = paste("dest.path is not specified. Extract to current directory?"))
        if(q==1){
            dest.path <- getwd()
        }else{stop("Specify your destination path before proceeding.")}
    }
    
    # Initializing Download time
    if (verbose){
        #running time
        begin.tiid <- Sys.time()
        # file download counter
        count <- 0
    }
    
    require(RCurl) # for locating a url path, for url.exists
    require(curl)
    
    
    ftp_base <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/%s/"
    
    dir_list_handle <- new_handle(ftp_use_epsv=FALSE, dirlistonly=TRUE, crlf=TRUE,
                                  ssl_verifypeer=FALSE, ftp_response_timeout=30)
    
    
    
    # creating a safe function for curl_fetch_memory
    s_curl_fetch_memory <- safely(curl_fetch_memory)
    
    # iterating through the years
    for (year in years){
        
        # file name to extract
        filename <- paste0(station,"-99999-",year,".op.gz")
        
        # Server files
            # fetch the year url
            year_url <- sprintf(ftp_base, year)
            # file to download
            file_url <- file.path(year_url,filename)
        
        # Local files
            # location of destination
            year_loc <- file.path(dest.path0,year)
            # put file on cache
            cache_loc <- file.path(year_loc,filename)
        
        
        #checking existence of local files helps us conserve bandwidth, instead of checking server then download...
        
        if(file.exists(cache_loc)&(overwrite==F)){
        # skip download if cache_file already exists (depending on the value of `overwrite`)
            if(verbose){
                message(paste(filename,"already exists in local folder. Skipping download..."))
            }
            next
            
            
        }else if(file.exists(cache_loc)&(overwrite==T)){
        # if it exists in local folder, it is assumed that it also exists in the server.
            message(paste(filename,"already exists in local folder. Overwriting..."))
            
            d <- tryCatch(download.file(file_url,cache_loc), 
                          error = function(e) print(paste("Download failed for year",year)))
            if(d == 0){
                if(verbose){
                    print(paste(year, "successfuly overwritten!"),quote = F)
                    count = count+1
                }
            }
        
             
        }else if(!file.exists(cache_loc)){
        # if file DOES NOT exist in our local folder YET, we need to then confirm if it exists in the server  
            
            # creating a safe curl fetch memory function
            s_curl_fetch_memory <- safely(curl_fetch_memory)
            
            # FETCHING FILE NAMES
            time.start <- Sys.time()
            repeat {
                
                # curl_fetch_memory write data from URL into a memory (using the safe cfm function)
                res <- s_curl_fetch_memory(year_url, handle=dir_list_handle)
                
                # if result of fetching is successful,  repeat will end
                
                if (!is.null(res$result)){
                    tmp <- res$result
                    con <- rawConnection(tmp$content) # create connection
                    files <- readLines(con) # list of the files in the folder
                    if (verbose){message(paste0("Successfuly connected to year folder ",year))}
                    close(con) # close connection to reduce memory
                    break # break repeat if connection has been found
                }
                
                # until when will the repeat loop run?
                if (difftime(Sys.time(), time.start, units = "secs")>timeout){ 
                    
                    if (verbose) {
                        # error sound
                        system("rundll32 user32.dll,MessageBeep -1")
                        runtime <- difftime(Sys.time(),begin.tiid, units = "mins")
                        print(paste("Succesfully downloaded", count, "files"), quote = F)
                        print(paste("Running time:",round(runtime,2),"minutes."), quote = F)
                        
                    }
                    stop(paste("Fetching memory failed at year ", year," due to timeout."), call. = F)
                    
                }
            }# end repeat
            
            if (filename %in% files){
                if(verbose){
                    message(paste("File",filename,"found. Proceeding to download..."))
                }
                
                d <- tryCatch(download.file(file_url,cache_loc), 
                              error = function(e) print(paste("Download failed for year",year)))
                
                if(d == 0){
                    if(verbose){
                        print(paste(year, "successfuly downloaded!"), quote = F)
                        count = count+1
                    }
                }else{next}# proceed to next year if download fails.
                
            }else{
                print(paste("Data for year", year,"does not exist."), quote = F)
                
            } 
            
        }
        
            
        
        
    }
    
    if (verbose) {
        beepr::beep(sound = 1, expr = NULL)
        runtime <- difftime(Sys.time(),begin.tiid, units = "mins")
        print(paste("Succesfully downloaded", count, "files"), quote = F)
        print(paste("Running time:",round(runtime,2),"minutes."), quote = F)
        
    }
    
}


