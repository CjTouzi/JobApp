#import data---------------- 

lf <- list.files("./finsents Data/")
path.lf <- paste0("./finsents Data/", lf )


# divide data into training data and test data--------------------
# 51 data in total

train.ind <- sample.int(length(path.lf),30)
test.data <- path.lf[-train.ind]
train.data <- path.lf[train.ind]


# choose one strategy with best median sharps------------------

sharps.train <- get_all_Sharpe(train.data)
best_overall <- select_strategy(sharps.train)[1]
best_buyonly <- select_strategy(sharps.train[,grepl(".BO",colnames(sharps.train))])[1]
best_buyonly

summary(sharps.train[,names(best_overall)])
summary(sharps.train[,names(best_buyonly)])

# performace on test data--------------------------
sharps.test <- get_all_Sharpe(test.data)
summary(sharps.test[,names(best_overall)])
summary(sharps.test[,names(best_buyonly)])


# interpretation of the rules-----------------------------
rules <- get_trading_rules(4,
                           c("bear.low","bear.high","bull.low","bull.high"))

names(best_overall) # "ret1.BS"
best_rule <- ifelse(rules[1,]==0,-1,1)
best_rule 

names(best_buyonly) # "ret1.BO"
rules[1,]





# =================================================================================================

boxplot(sharps.train, ylim=c(-5,5))


# select the one statistically better than bechmark
select_strategy<- function(sharps.train) {  
    boxplot(sharps.train, ylim=c(-5,5))
    abline(h=1)
    # select the strategy with best median sharp ratio     
    re <- sort(apply(sharps.train,2,median,na.rm=TRUE),decreasing=TRUE)
    re
    
}



get_all_Sharpe <- function(data){
    
    file.name <- data.frame(file.name = data)
    sharps<- NA
    
    for(i in 1: nrow(file.name)){

        d <- read.csv(as.character(file.name$file.name[i]))
        # print(head(d))
        # d_part <- d[d$pOpen!=0,c("pClose","sentiment","sVolume")]
        d_part <- d[d$pOpen!=0,]
        
        # remove the error file 
        if (nrow(d_part)<20){
            next
        }
        x <- main(d_part)
        # matplot(1+cumsum(x),type="l")
        # get_annualised_Sharpe(x,x[,1])
        sharps <- rbind(sharps, get_annualised_Sharpe(x,x[,1]))
    }
    
    
    sharps <- sharps[-1,]
    sharps[sharps==Inf | sharps==-Inf] <- NA     
    sharps

}


# Annualized Sharp vs benchmark in daily base
get_annualised_Sharpe <- function(ret.m, benchmark,n=252){  
    
    # ret.m[ret.m==Inf | ret.m==-Inf | ret.m==NaN] <- NA
    # benchmark[benchmark==Inf | benchmark==-Inf | benchmark==NaN] <- NA
    x <- ret.m-benchmark   
    
    re.sd <- apply(x,2,sd,na.rm=TRUE)
    re <- apply(ret.m,2,mean,na.rm=TRUE)
    re <- re/re.sd*sqrt(n)
    re
    
 }


# core strategy
main <- function(d1){
    
    # sentiment and sVolume as position indicator
    
    # construct decision rules matrix
    #  --------------- sentiment --------------------
    #  ---------------- bull-----bear---------------
    #  Vol  High ------- B/S -----B/S-------------
    #       Low -------- B/S -----B/S-------------
    
    
    # 16 rules in total for buy-only or buy-short 
    
    # d1 <- read.csv(as.character(file.name$file.name[i]))
    
    sent <- ifelse(d1$sentiment>5,"bull","bear")
    # head(sent)
    # sent
    # high or low impact sentiment 
    vol <- ifelse(d1$sVolume>5 ,"high","low")
    # head(vol)


    # 4 market states model    
    conditions <- paste(sent,vol,sep=".")
    
    # adjust by sVolume
    # sentiment information is discontinous
    # here we assume the market states doesn't change unless new sentiment comes. 
    conditions[which(d1$sVolume==0)] <-NA
    require(zoo)
    conditions <- na.locf(conditions, na.rm = FALSE)    
    
    # generate rules:-----------------------------------
    # return all the possible perputations of trading rules 
    # condition.names refer the possible market states as defined
    rules <- get_trading_rules(4,
                               c("bear.low","bear.high","bull.low","bull.high"))
    head(rules)
    
    # return the matrix of trading signals 
    # according to market condition series and the rules provided
    signals<-get_trading_signals(conditions,rules)
    
    
    # signal lagging
    signals <- rbind(rep(0,ncol(signals)),signals)
    signals <- signals[1:length(diff(d1$pVolume)),]
    
    
    # return of 16 differnt rules
    # buy only 
    ret.m <- get_return(signals,d1$pClose)
    names(ret.m) <- paste0(names(ret.m), ".BO")
    
    # return of 16 differnt rules
    # buy and short     
    signals_sl <- ifelse(signals==0,-1,1)
    ret.m.sl<- get_return(signals_sl,d1$pClose)
    names(ret.m.sl) <- paste0(names(ret.m.sl), ".BS")
    
    # return total 32 rules
    ret.total <- cbind(ret.m,ret.m.sl)
    # matplot(1+cumsum(ret.total), type="l")
    ret.total
    
}



# generate rules:-----------------------------------
# return all the possible perputations of trading rules 
# condition.names refer the possible market states as defined
get_trading_rules<- function(num.conditions=4, condition.names){
    
    decimals <- seq(1:2^num.conditions)
    rules <- t(sapply(decimals,function(x){as.integer(intToBits(x))})[1:num.conditions,])
    colnames(rules) <- condition.names
    row.names(rules) <- paste0("r",seq(1:2^num.conditions))
    rules    
} 


# return the matrix of trading signals 
# according to market condition series and the rules provided
get_trading_signals <- function(con.real, rules){

    signals <- data.frame(con=con.real) 
    num.rules <- nrow(rules)
    series.len<- length(con.real)

    
    for (rule in 1: num.rules){
        
        temp <- 1:series.len
        for (i in 1: series.len){
            # print(i)
            
            # print(con.real[i])
            # con.real <- conditions
            # which(colnames(rules)==con.real[1])

            if(!all(is.na(colnames(rules)==con.real[i]))){
                temp[i] <- rules[rule,colnames(rules)==con.real[i]]
                
            }else{
                temp[i] <- 0
            }
            
            
        }
        signals <- cbind(signals,temp)
        
    }
    signals <- signals[,-1]
    colnames(signals) <- paste0("sig",seq(1:num.rules))
    signals<- signals[-nrow(signals),]
    
    signals

}

# return series
get_return <- function(signals, p_series){
    
    ret.m <- data.frame(buyhold=diff(p_series))
    
    ret.m$buyhold <-  ret.m$buyhold/p_series[1:nrow(ret.m)]

    for (i in 1: ncol(signals)){
        ret <- ret.m$buyhold*signals[,i]
        ret.m <- cbind(ret.m, ret)        
    }
    colnames(ret.m) <- c("holdbuy.ret",paste0("ret",seq(1:ncol(signals))))
    ret.m
}







