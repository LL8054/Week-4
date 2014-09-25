## Ranks hospitals in the requested state according to the inputted outcome 
## and returns the requested rank.  

rankhospital <- function(state, outcome, num = "best") {
    
    ## Reads data.
    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", colClasses = "character")
    
    ## Checks validity of state input.
    state_check <- match(state,unique(data[,7]))
    if (is.na(state_check == TRUE)) {
        stop("invalid state")
    }
    
    ## Checks validity of outcome input.
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
    outcome_check <- match(outcome,possible_outcomes)
    if (is.na(outcome_check == TRUE)) {
        stop("invalid outcome")
    }
    if (outcome == "heart attack") outcome_col <- 11
    if (outcome == "heart failure") outcome_col <- 17
    if (outcome == "pneumonia") outcome_col <- 23
    
    ## If both state and output input are valid, returns char vector of name of
    ## hospital with the corresponding ranking in that state according to outcome.
    master <- data.frame()
    master <- subset(data, data[,7]==state)
    master[,outcome_col] <- as.numeric(master[,outcome_col])
    master$Rank <- ave(master[,outcome_col], master$State, FUN=rank)
    master_sorted <- master[with(master, order(master[,47], master[,2])),]
    if (num == "best") num <- 1
    if (num == "worst") {
        max_val <- max(master_sorted[,outcome_col], na.rm=TRUE)
        master_max <- subset(master_sorted, master_sorted[,outcome_col]==max_val)
        num <- nrow(master_max)
        print(master_max[num,2])
    } else {
    hospital_name <- master_sorted[num,2]
    hospital_name
    }
}