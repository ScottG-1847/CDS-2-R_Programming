pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## Create vector to hold values
    vals <- vector(mode = "numeric", length=0)
    
    for (i in id){
        ## Get data file name
        iStr <- toString(i)
        iLen <- nchar(iStr)
        switch(iLen,
               pad <- "00",
               pad <- "0",
               pad <- ""    
        )
        
        fileName <- paste(directory, "/", pad, i, ".csv", sep="")
        
        ## Open data file
        df <- read.csv(fileName)
        
        ## Save pollutant values
        vals <- c(vals, df[ , pollutant])

        ## Unload data frame
        rm(df)
    }
    
    ## Calculate, format & return result
    result <-mean(vals, na.rm=TRUE)
    result <-round(result, digits=3)
    result
}
