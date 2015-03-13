complete <- function(directory, fid = 1:332) {
	id <- vector(mode="numeric", length=0)
	nobs <- vector(mode="numeric", length=0)
	
	for (i in fid){
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
		
		id <- c(id, i)
		nobs <- c(nobs, n)
		
		## Unload source data
		rm(df)
		
	}
	
	result <- data.frame(id, nobs)
	print(result)
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
