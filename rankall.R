##Returns the hospital in each state that has the specified ranking.

rankall <- function(outcome, num = "best") {
        ## Reads data.
        data <- read.csv("outcome-of-care-measures.csv", na.string="Not Available", colClasses = "character")
        
        ## Checks validity of outcome input.
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        outcome_check <- match(outcome,possible_outcomes)
        if (is.na(outcome_check == TRUE)) {
                stop("invalid outcome")
        }
        if (outcome == "heart attack") outcome_col <- 11
        if (outcome == "heart failure") outcome_col <- 17
        if (outcome == "pneumonia") outcome_col <- 23
        
        ## Finds the hospital of given rank per state.
        master <- data.frame()
        data[,outcome_col] <- as.numeric(data[,outcome_col])
        master <- data[,c(2,7,outcome_col)] ## strips out unnecessary columns
        master <- master[with(master, order(master[,2], master[1])),]  ## alphabetizes hospitals w/i states
        master$Rank <- ave(master[,3], master$State, FUN= function(x) rank(x, ties.method="first")) ## creates rank column
        master_sorted <- master[with(master, order(master[,2], master[,4], master[1])),] ## creates ranked dataset
        master_subset <- subset(master_sorted, master_sorted[,3]!="NA") ## strips out NAs
        
        states <- unique(master_subset[,2])
        results <- data.frame()        
                
        master_split <- split(master_subset, master_subset[,2]) ## splits data sets by states
        unique_states <- length(master_split) 
        
        hosp <- rep(NA, unique_states)
        state <- character(length = unique_states)
        
        if (num == "best") num <- 1
                
        for (i in 1:unique_states) {
                temp_state <- unlist(master_split[i])
                count <- length(temp_state)/4
                temp_df <- data.frame(temp_state[1:count], temp_state[(count+1):(2*count)], temp_state[(2*count+1):(3*count)], temp_state[(3*count+1):(4*count)])
                state[i] <- as.character(temp_df[1,2])
                long_count <- count*4
                if (num == "worst") {
                        max_val <- max(as.integer(temp_df[,4]))
                        hosp[i] <- as.character(temp_df[max_val,1])
                        
                } else{  
                        for (j in 1:long_count) {
                                if(identical(as.character(temp_df[j,4]), as.character(num))) {
                                hosp[i] <- as.character(temp_df[j,1])
                                } 
                        }
                }
        }
        results <- data.frame(hosp, state, stringsAsFactors=FALSE)
        colnames(results) <- c('hospital', 'state')
        results
}