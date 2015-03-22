rankhospital<-function(state,outcome, num) {
        
#read outcome 
data<-read.table("outcome-of-care-measures.csv",dec = ".",header = TRUE,sep = ",",colClasses = "character")

ranknum<-as.numeric()

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

#take only name and 

statedata<-statedata[,c("Hospital.Name", outcome)]

#remove NAs
statedata<-na.omit(statedata)

#define rank value
if (num=="best") ranknum<-1
else if (num=="worst") ranknum<-nrow(statedata)
else if (num > nrow(statedata)) return ("NA")
else ranknum<-num

#order data
newstatedata<-statedata[order(statedata[,outcome],statedata$Hospital.Name), ]

#return hospital name according to rank
return (newstatedata[ranknum,"Hospital.Name"])
}

