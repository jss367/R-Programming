
rankhospital <- function(state, outcome, num = "best"){
        
        ## Read outcome data
        file <- "outcome-of-care-measures.csv"
        data <- read.csv(file, colClasses = "character")
        
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        correct_state = data[data[,7] == state, ]
        #Change header of pointer to "Rate"
        names(correct_state)[pointer] <-"Rate"
        
        index = order(correct_state$Rate, correct_state$Hospital.Name, decreasing = F)
        ordered_state = correct_state[index, ]
        ordered_state <- na.omit(ordered_state)
        hosps <- ordered_state$Hospital.Name
        if (num == "best"){
                num <- 1
        } else if (num == "worst"){
                num <- length(hosps)
        }
        print(ordered_state$Hospital.Name[num])
}