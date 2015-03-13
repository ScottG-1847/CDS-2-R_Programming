corr <- function(directory, threshold = 0) {
	nitrates <- vector(mode="numeric", length=0)
	sulfates <- vector(mode="numeric", length=0)
	correlations <- vector(mode="numeric", length=0)
	
	for (i in 1:332){
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
		cc <- sum(complete.cases(df))

		if (cc > threshold) {
		## save the data
			df2 <- na.omit(df)
			
			n <- df2$nitrate
			s <- df2$sulfate
			
			c <- cor(s, n)
			correlations <- c(correlations, c)
		}
		
		## Unload source data
		rm(df)
	}
	
	correlations
}
