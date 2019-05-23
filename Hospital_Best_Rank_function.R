#Wendong Xie
#1 Plot the 30-day mortality rates for heart attack
setwd("/Users/wendongxie/Documents/my_projects")
outcome = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)
outcome[,11] = as.numeric(outcome[,11])
hist(outcome[,11])
x = lapply(outcome[,c(11,17,23)],as.numeric)

#2 Finding the best hospital in a state
best = function(State, outcome){
    data = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
    #11 is heart attack 
    #17 is heart failier
    #23 is Pneumonia
    cdata = cbind(data[,c(2,7)],data.frame(lapply(data[,c(11,17,23)],as.numeric)))
    # 1 name 2 state 3 attack 4 failure, 5 pneumonia in cdata
    #State name is in 7
    #if(!(State !%in% cdata$State)) stop("State Wrong")
    #if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("outcome wrong")
    
    data_state = cdata[which(cdata$State==State),]
    if(outcome == 'heart attack'){ #3
        heart_attack = min(data_state[,3],na.rm = TRUE)
        # using which to chose the right match
        hos_name = data_state[which(data_state[,3]==heart_attack),]$Hospital.Name
        name = hos_name[order(hos_name)]# order the name using alphabet order
        return(name[1])
    }
    else if (outcome == "heart failure"){
        heart_failure = min(data_state[,4],na.rm = TRUE)
        hos_name2 = data_state[which(data_state[,4]==heart_failure),]$Hospital.Name
        name2 = hos_name2[order(hos_name2)]
        return(name2[1])
    }
    else{
        pneumonia = min(data_state[,5],na.rm=TRUE)
        hos_name3 = data_state[which(data_state[,5]==pneumonia),]$Hospital.Name
        name3 = hos_name3[order(hos_name3)]
        return(name3[1])
    }
}


