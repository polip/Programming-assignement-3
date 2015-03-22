rankall<-function (outcome, num) { 
        
ranknum<-as.numeric()
hospital<-as.character()
state<-as.character()

#read outcome 
data<-read.table("outcome-of-care-measures.csv",dec = ".",header = TRUE,sep = ",",colClasses = "character")

#Check that outcome is valid
validoutcome=c("heart attack","heart failure","pneumonia")
if (!outcome %in% validoutcome) stop ("invalid outcome")

#create dataframe with neccessary columns
allstatedata<-data[, c(2,7,11,17,23)]

#Convert classes
suppressWarnings(allstatedata[, 4] <- as.numeric(allstatedata[,4]))
suppressWarnings(allstatedata[, 5] <- as.numeric(allstatedata[,5]))
suppressWarnings(allstatedata[, 3] <- as.numeric(allstatedata[,3]))

#Change column names
colnames(allstatedata)[3]<-"heart attack"
colnames(allstatedata)[4]<-"heart failure"
colnames(allstatedata)[5]<-"pneumonia"

#create list of valid states
validstate = sort(unique(allstatedata[,2]))

for (i in seq_along(validstate)) {
        
        #take data for each state
        statedata<-allstatedata[allstatedata$State==validstate[i],c("Hospital.Name", outcome, "State")]
        
        #remove NAs
        statedata<-na.omit(statedata)
        
        #define rank value
        if (num=="best") ranknum<-1
        else if (num=="worst") ranknum<-nrow(statedata)
        else ranknum<-num
        
        #order data
        newstatedata<-statedata[order(statedata[,outcome],statedata$Hospital.Name), ]
                
        #put result for each state in new dataframe
        hospital[i]<-newstatedata[ranknum,"Hospital.Name"] 
        state[i]<-validstate        
        }

#return dataframa with results
data.frame(hospital = hospital, state=validstate)
        
}