best<-function(state,outcome) {
        

#read outcome 
      data<-read.table("outcome-of-care-measures.csv",dec = ".",header = TRUE,sep = ",",colClasses = "character")

#Check that state and outcome are valid
if (nrow(data[data$State== state,]) == 0) 
        stop("invalid state")

validoutcome=c("heart attack","heart failure","pneumonia")
if (!outcome %in% validoutcome) stop ("invalid outcome")
                  
#Return outcome for selected state
        statedata<-data[data$State==state, c(2,7,11,17,23)]

#Convert classes
        suppressWarnings(statedata[, 4] <- as.numeric(statedata[,4]))
        suppressWarnings(statedata[, 5] <- as.numeric(statedata[,5]))
        suppressWarnings(statedata[, 3] <- as.numeric(statedata[,3]))
        
#Change column names
        colnames(statedata)[3]<-"heart attack"
        colnames(statedata)[4]<-"heart failure"
        colnames(statedata)[5]<-"pneumonia"

#New data frame with ordered data
newstatedata<-statedata[order(statedata[,outcome]),]
#Result
newstatedata[1,1]
}