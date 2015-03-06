#setwd('C:\Users\HMGSYS\Documents\GitHub\Classes\R-Programming\rprog-data-ProgAssignment3-data')
best <- function(state, outcome){
        #Read outcome data
        data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # Determine which column in data to look at by the user input on outcome
        if (outcome == "heart attack"){
                pointer <- 11
        } else if (outcome == "heart failure"){
                pointer <- 17
        } else if (outcome == "pneumonia"){
                pointer <- 23
        } else{
                stop ("invalid outcome")
        }
        #Convert that data to be numeric
        suppressWarnings(data[,pointer] <- as.numeric(data[,pointer]))

        #Check if the state is valid:
        if (!state %in% data$State){
                stop("invalid state")
        }
        
        #Get a subset of the data that contains the state you are interested in
        correct_state <- data[data[, 7] == state, ]
        outcome_arr <- correct_state[, pointer]
        min <- min(outcome_arr, na.rm=T)
        index <- which(outcome_arr == min)
        print(correct_state[index, 2])

        ## check that state and outcome are valid
}

#best("TX", "heart attack")
#"CYPRESS FAIRBANKS MEDICAL CENTER"
#best("TX", "heart failure")
#"FORT DUNCAN MEDICAL CENTER"
#best("MD", "heart attack")
#"JOHNS HOPKINS HOSPITAL, THE"
#best("MD", "pneumonia")
#"GREATER BALTIMORE MEDICAL CENTER"
#best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome


