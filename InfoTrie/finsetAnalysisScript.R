#import data---------------- 

lf <- list.files("./finsents Data/")
path.lf <- paste0("./finsents Data/", lf )

d1 <- read.csv(path.lf[1])
d1$company <- sub(".csv","",lf[1])
head(d1)


# clean datd
# remove weekend

d1 <- d1[d1$pOpen!=0,]

x <- main(d1)

main <- function(d1){
    
    # sentiment and sVolume as position indicator
    
    # construct decision rules matrix
    #  --------------- sentiment --------------------
    #  ---------------- bull-----bear---------------
    #  Vol  High ------- B/S -----B/S-------------
    #       Low -------- B/S -----B/S-------------
    # 16 rules in total 
      
    # sentiment 
    sent <- ifelse(d1$sentiment>5,"bull","bear")
    # head(sent)
    
    # sentiment influence 
    vol <- ifelse(d1$sVolume>5,"high","low")
    # head(vol)
    
    # 4 market states model
    
    conditions <- paste(sent,vol,sep=".")
    
    # generate rules:-----------------------------------
    # return all the possible perputations of trading rules 
    # condition.names refer the possible market states as defined
    rules <- get_trading_rules(4,
                               c("bear.low","bear.high","bull.low","bull.high"))
    head(rules)
    # return the matrix of trading signals 
    # according to market condition series and the rules provided
    signals<-get_trading_signals(conditions,rules,conditions)
    
    
    # signal lagging
    signals <- rbind(rep(0,ncol(signals)),signals)
    signals <- signals[1:length(diff(d1$pVolume)),]
    
    
    # accumulated return of 16 differnt rules
    # buy only 
    ret.m <- get_return(signals,d1$pClose)
    
    # accumulated return of 16 differnt rules
    # buy and short 
    
    signals_sl <- ifelse(signals==0,-1,1)
    ret.m.sl<- get_return(signals_sl,d1$pClose)
    
    # return total 32 rules
    ret.total <- cbind(ret.m,ret.m.sl)
    matplot(1+cumsum(ret.total), type="l")
    ret.total
}











get_trading_rules<- function(num.conditions=4, condition.names){
    
    decimals <- seq(1:2^num.conditions)
    rules <- t(sapply(decimals,function(x){as.integer(intToBits(x))})[1:num.conditions,])
    colnames(rules) <- condition.names
    row.names(rules) <- paste0("r",seq(1:2^num.conditions))
    rules    
} 



get_trading_signals <- function(con.real, rules, conditions){

    signals <- data.frame(con=con.real) 
    num.rules <- nrow(rules)
    series.len<- length(con.real)
    
    for (rule in 1: num.rules){
        
        temp <- 1:series.len
        for (i in 1: series.len){
            # print(i)
            # print(conditions[i])
            temp[i] <- rules[rule,colnames(rules)==conditions[i]]
        }
        signals <- cbind(signals,temp)
        
    }
    signals <- signals[,-1]
    colnames(signals) <- paste0("sig",seq(1:num.rules))
    signals<- signals[-nrow(signals),]
    
    signals

}


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







