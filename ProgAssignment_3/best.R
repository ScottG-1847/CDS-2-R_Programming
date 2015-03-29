best <- function(state, outcome) {
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
    
    ss[, 2] <- suppressWarnings(as.numeric(ss[, 2]))
    
    ## Return hospital name in that state with lowest 30-day death rate
    name <- subset(ss, ss[,2] == min(ss[,2], na.rm=TRUE), select=Hospital.Name)$Hospital.Name
    print(name)
    
    rm(data)
    rm(ss)
    rm(name)
}
