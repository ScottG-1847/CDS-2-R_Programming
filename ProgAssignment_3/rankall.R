rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE) stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    ## Create the subset
    if (outcome == "heart attack")  {
        ss <- subset(data, select = c(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) 
    }
    if (outcome == "heart failure"){
        ss <- subset(data, select = c(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    }
    if (outcome == "pneumonia"){
        ss <- subset(data, select = c(State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    }
    
    colnames(ss)[1] <- "state"
    colnames(ss)[2] <- "name"
    colnames(ss)[3] <- "value"
    
    ss$value <- suppressWarnings(as.numeric(ss$value)) #convert value column to numeric
    ss <- na.omit(ss)                                  #delete incomplete cases
    ss <- ss[order(ss$state, ss$value, ss$name),]      #sort data
    
    spl <- split(ss, ss$state)
    if (num == "best" ) {
        r2 <- lapply(spl, function(x) c(head(x$name,1), head(x$state,1)))
    } else if ( num == "worst" ) {
        r2 <- lapply(spl, function(x) c(tail(x$name,1), head(x$state,1)))
    } else {
        r2 <- lapply(spl, function(x) c(x[num, "name"], head(x$state,1)))
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    df <- data.frame(matrix(unlist(r2), nrow=length(r2), byrow=T),stringsAsFactors=FALSE)
    colnames(df)[1] <- "name"
    colnames(df)[2] <- "state"
    rownames(df) <- df$state
    return(df)

    ## Cleanup
    rm(r2)
    rm(ss)
    rm(data)
    rm(df)

}