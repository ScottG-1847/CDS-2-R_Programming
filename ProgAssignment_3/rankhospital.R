rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (state   %in% data$State                                     == FALSE) stop("invalid state")
    if (outcome %in% c("heart attack","heart failure", "pneumonia") == FALSE) stop("invalid outcome")
    
    ## Create the subset
    if (outcome == "heart attack")  {
        ss <- subset(data, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) 
    }
    if (outcome == "heart failure"){
        ss <- subset(data, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    }
    if (outcome == "pneumonia"){
        ss <- subset(data, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    }
    
    colnames(ss)[1] <- "name"
    colnames(ss)[2] <- "value"
    
    ss$value <- suppressWarnings(as.numeric(ss$value)) #convert value column to numeric
    ss <- na.omit(ss)                                  #delete incomplete cases
    ss <- ss[order(ss$value, ss$name),]                #sort data
    
    if (rank == "best" ) {
        result <- head(ss$name, 1)
    } else if ( rank == "worst" ) {
        result <- tail(ss$name, 1)
    } else {
        result <- ss[rank, 1]
    }
    
    print(result)
    
    rm(ss)
    rm(data)
}