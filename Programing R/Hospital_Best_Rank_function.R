#Wendong Xie
#1 Plot the 30-day mortality rates for heart attack ---------------------------------------------------
setwd("/Users/wendongxie/Documents/my_projects")
outcome = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)
outcome[,11] = as.numeric(outcome[,11])
hist(outcome[,11])
x = lapply(outcome[,c(11,17,23)],as.numeric)




#2 Finding the best hospital in a state ----------------------------------------------------------------

best = function(State, outcome){
    data = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
    #11 is heart attack 
    #17 is heart failier
    #23 is Pneumonia
    cdata = cbind(data[,c(2,7)],data.frame(lapply(data[,c(11,17,23)],as.numeric)))
    if(!(State %in% cdata[,2])) stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("wrong outcome")
    # 1 name 2 state 3 attack 4 failure, 5 pneumonia in cdata
    #State name is in 7
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

#Test Data
best("TX", "heart attack")#"CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")# "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")# "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")# "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")#Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")#Error in best("NY", "hert attack") : invalid outcome


# Ranking hospitals by outcome in a state----------------------------------------------------------------

rankhospital = function(State,outcome,num){
    data = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
    #11 is heart attack 17 is heart failier 23 is Pneumonia
    #cut data and clean data ----
    cdata = cbind(data[,c(2,7)],data.frame(lapply(data[,c(11,17,23)],as.numeric)))
    if(!(State %in% cdata[,2])) stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("wrong outcome")
    # 1 name 2 state 3 attack 4 failure, 5 pneumonia in cdata
    #State name is in 7 
    data_state = cdata[which(cdata$State==State),]   #clean data with state
    if(outcome == 'heart attack'){ #3
        order_by_HA = data_state[order(data_state[,3],data_state[,1]),]# sort by heart attack rate and name
        HA_data = order_by_HA[!is.na(order_by_HA[,3]),]# remove NA
        
        if(is.numeric(num) & num <= nrow(data_state)) return(HA_data[num,]$Hospital.Name)
        else if (num == 'best') return(HA_data[1,]$Hospital.Name)
        else if(num =='worst') return(HA_data[nrow(HA_data),]$Hospital.Name)
        else return('NA')
    }
    else if (outcome == "heart failure"){
        # sort and create new DF by heart attack and name of hospital
        order_by_HF = data_state[order(data_state[,4],data_state[,1]),]
        #remove the heart attack column's NA in column 4
        HF_data = order_by_HF[!is.na(order_by_HF[,4]),]
        
        if(is.numeric(num) & num <= nrow(data_state)) return(HF_data[num,]$Hospital.Name)
        else if (num == 'best') return(HF_data[1,]$Hospital.Name)
        else if(num =='worst') return(HF_data[nrow(HF_data),]$Hospital.Name)
        else return('NA')
    }
    else{
        # sort and create new DF by heart attack and name of hospital
        order_by_P = data_state[order(data_state[,5],data_state[,1]),]
        #remove the heart attack column's NA in column 5
        P_data = order_by_P[!is.na(order_by_HF[,5]),]
        
        if(is.numeric(num) & num <= nrow(data_state)) return(P_data[num,]$Hospital.Name)
        else if (num == 'best') return(P_data[,]$Hospital.Name)
        else if(num =='worst') return(P_data[nrow(P_data),]$Hospital.Name)
        else return('NA')
    }
}


#TEST DATA
rankhospital("TX", "heart failure", 4)#"DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") #"HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) # NA




#Problem 4 Ranking hospitals in all states----

rankall = function(outcome,num){
    #Clean data ----
    data = read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
    #11 is heart attack 17 is heart failier 23 is Pneumonia
    cdata = cbind(data[,c(2,7)],data.frame(lapply(data[,c(11,17,23)],as.numeric)))
    if(!(State %in% cdata[,2])) stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("wrong outcome")
    #Rank them ----
    if (outcome == "heart attack"){
        
    }
}
