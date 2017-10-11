###### Libraries ######
setwd("/Users/ewa/Desktop/RTB-simulation/RTB-simulation-framework")
library("fst")
library("data.table")
library("tidyverse")
library("purrr")
library("R.utils")

library(profvis)
profvis({
  
auction_engine <- function(timeSpan, Impression_target, spend_budget){
    
  set.seed(10)
  source("bid_request_generator.R")
  source("bidder_base.R")
  source("data_generator.R")
  
  generate_bidRequests(timeSpan)
  g <- length(bid_requests$idfa)
  uniques <- length(unique(bid_requests$idfa))

  auction_number <<- 1L
  
  
  bidders <- list(default1=bidder_base(spend_budget, Impression_target, 1, g, uniques, default(10, 0.3, 1.3), "default1"), 
                  default2=bidder_base(spend_budget, Impression_target, 1, g, uniques, default(5, 0.4, 1.3), "default2"),
                  bidder1=bidder_base(spend_budget, Impression_target, 1, g, uniques, randBid(g, 0, 0.5), "bidder1"), 
                  PI = bidder_base(spend_budget, Impression_target, 1, g, uniques, PID_base(payout = 1.2, margin = 0.4, k_1 = 0.9, k_2 = 0.0001, bid_requests = bid_requests, g=g, target=Impression_target), "PI1"))

  won_bids <- rep(0, g)
  CPM <- rep(0, g)
  winningBidder <- rep(0, g)
    
  for (x in seq_len(g)){
    id <- bid_requests$idfa[x]
    floor <- bid_requests$bidFloor[x]

    bids <- map_dbl(bidders, function(f) f(id, floor, auction_number))
    wonBid <- max(bids)
    winner <- names(which.max(bids))
    bids <- bids[-max(bids)]
    #eCPM <- max(bids[-wonBid])
    eCPM <- max(bids)
    
    if (eCPM < floor){
      if (wonBid<floor){
        eCPM = wonBid
      }
      else {eCPM = floor}
    }
  
    #win_notification(environment(bidders[[winner]]), auction_number, eCPM, id)  
    environment(bidders[[winner]])$win_notification(auction_number, eCPM, id)

    won_bids[x] <- wonBid
    CPM[x] <- eCPM
    winningBidder[x] <- winner 

    auction_number <<- auction_number+1L
  }
  auctions_results <- list(wonBid=won_bids, CPM=CPM, winner=winningBidder)
  
  return(list(Bidders=bidders, Auctions=auctions_results, Requests=bid_requests))
}
results <- auction_engine(1, 3000, 2.1)
data <- data_generator(results)
})
  

timeSpan<- 1
Impression_target <- 3000
spend_budget <- 2



n <- 100
win_rate <- aggregate(fancy_bidder,list(rep(1:(nrow(fancy_bidder)%/%n+1),each=n,len=nrow(fancy_bidder))),mean)[-1]
win_price <- aggregate(fancy_bidder,list(rep(1:(nrow(fancy_bidder)%/%n+1),each=n,len=nrow(fancy_bidder))),sum)[-1]
win_price <- win_price$CPM/win_price$result
df <- data.frame(Bidder_responses$fancy_bidder)
bid_price <- aggregate(df,list(rep(1:(nrow(df)%/%n+1),each=n,len=nrow(df))),mean)[-1]
df <- data.frame(winRate=win_rate$result, CPM=win_price, bidPrice=bid_price$Bidder_responses.fancy_bidder)
df[is.na(df)] <- 0
library(ggplot2)
ggplot2::ggplot(df, aes(x=c(1:nrow(df)), y=winRate, color="Win Rate"))+geom_line()+geom_line(aes(y=CPM, color='CPM'))+
  geom_line(aes(y=bidPrice, color='Bid'))+ggtitle(paste("PI Performance; Desired Win rate", optim_winRate))+xlab("time slot")+ylab("Value")+theme_minimal()

ggplot(d[1:112, ], aes(c(1:112), winRate, color="WinRate"))+geom_line()+geom_line(aes(y=CPM, color='CPM'))+geom_line(aes(y=bidPrice, color='bid'))

Rprof(tmp <- tempfile())
example(auction_engine)
Rprof()
summaryRprof(tmp)
