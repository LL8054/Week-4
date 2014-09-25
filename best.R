## Finds the best hospital in a state for a named outcome

best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        
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
        ## hospital with the best 30-day mortality rate in that state.
        master <- data.frame()
        for (i in 1:nrow(data)) {
                if (state == data[i,7]) {
                        master <- rbind(master,data[i,])
                }
        }
        x <- as.numeric(as.character(master[,outcome_col]))
        smallest <- min(x, na.rm=TRUE)
        smallest_list <- match(x, smallest)
        final <- data.frame()
        for(i in 1:length(smallest_list)) {
                if (!is.na(smallest_list[i])) {
                        final <- rbind(final,master[i,])
                }
        }
        sort.final <- with(final, final[order(2),])
        hospital_name <- sort.final[1,2]
        hospital_name
        
} 

