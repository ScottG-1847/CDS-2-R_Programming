rl <- function(){
    source("f_PolMean.R")
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    for (i in id){
        ## Load a data file
        fn <- fileName(directory, i)
        df <- read.csv(fn)
        
        ## Do something with the file
        str <- paste(i, ".csv - ", nrow(df))
        print(str)
        
        ## Unload DataFrame
        rm(df)
    }
}

fileName <- function(directory, i){
## Returns a string that is the directory/file to open
## Needed because i may need leading 0's added
    
    iStr <- toString(i)
    iLen <- nchar(iStr)
    switch(iLen,
        slashPad <- "/00",
        slashPad <- "/0",
        slashPad <- "/"    
    )
    
    result <- paste(directory, slashPad, str, ".csv", sep="")
    result

}