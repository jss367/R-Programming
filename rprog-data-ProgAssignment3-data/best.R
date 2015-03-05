#setwd('~/R/rprog-data-ProgAssignment3-data')
best <- function(state, outcome){
        #Read outcome data
        data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        if (outcome == "heart attack"){
                pointer <- 11
        } else if (outcome == "heart failure"){
                pointer <- 17
        } else if (outcome == "pneumonia"){
                pointer <- 23
        } else{
                stop ("invalid outcome")
        }
        data[,pointer] <- as.numeric(data[,pointer])
        str(data)
        data[,pointer]<-na.omit(data[,pointer])
        str(data)
        
        data[,pointer] <- as.numeric(data[,pointer])
                x <- cbind(data[,2], data[,7], data[,pointer]) #hospital name, state, numba
        y <- na.omit(x)
        na.omit(x)
        set<-FALSE
        end <- 1
        tops <- 100
        valid <- FALSE
        #Check if the state is valid:
        for (k in 1:nrow(y)){
                if (y[k,2] == state){
                        valid<-TRUE
                }
        }
        if (!valid){
                stop('invalid state')
        }
        
        #Find the beginning of the state you are interested in
        for (j in 1:nrow(y)){
                if (y[j,2] == state){
                        if (set == FALSE){
                                beginning <- j
                                set <- TRUE
                        }
                        end <- j
                }
                if (set == TRUE){
                }
        }
        for (k in beginning:end){
                if (as.numeric(y[k,3]) < as.numeric(tops)){
                      #  browser()
                        tops <- y[k,3]
                        ans <- y[k,1]
                }
        }
        i<-1
        print(paste0("The highest number we found is ", tops))
        print(paste0("We believe the top institution in the state is ", ans))

        ## check that state and outcome are valid
}