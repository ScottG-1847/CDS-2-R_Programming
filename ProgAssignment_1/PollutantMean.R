complete <- function(directory, pollutant, id = 1:332) {
    iVec <- vector(mode="numeric", length=0)
    nVec <- vector(mode="numeric", length=0)
    
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
        
        ## Count complete cases
        n <- sum(complete.cases(df))
        
        ## Append to holding vectors
        iVec <- c(iVec, i)
        nVec <- c(nVec, n)

        ## Unload data frame
        rm(df)
    }    
    
    result <- data.frame(iVec, nVec)
    
}



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
    
    ## Calculate & Return data
    m <-mean(vals, na.rm=TRUE)
    round(m, digits = 3)
}
