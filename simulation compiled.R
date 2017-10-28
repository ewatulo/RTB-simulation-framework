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
    source("visualization.R")
    source("2dof PID.R")
    
    generate_bidRequests(timeSpan)
    g <- length(bid_requests$idfa)
    uniques <- length(unique(bid_requests$idfa))
    
    auction_number <<- 1L
    
    
    bidders <- list(default1=bidder_base(spend_budget, Impression_target, 1, g, uniques, default(10, 0.4, 1.2), "default1"), 
                    #bidder1=random_base(g, randBid(g, 0, 0.5), "bidder1"), 
                    static1 = bidder_base(500, Impression_target, 1, g, uniques, static(0.5, 1.2), "static1"),
                    static2 = bidder_base(500, Impression_target, 1, g, uniques, static(0.84, 1.2), "static2"),
                    static3 = bidder_base(500, Impression_target, 1, g, uniques, static(0.96, 1.2), "static3"),
                    static4 = bidder_base(500, Impression_target, 1, g, uniques, static(1.08, 1.2), "static4"),
                    PI1 = bidder_base(500, Impression_target, 1, g, uniques, PI_base(payout = 1.2, margin = 0.4, k_1 = 0.0001, k_2 = 0.01, bid_requests = bid_requests, g=g, target=Impression_target), "PI1"),
                    PI2 = bidder_base(500, Impression_target, 1, g, uniques, PI_base(payout = 1.2, margin = 0.4, k_1 = 0.002, k_2 = 0.001, bid_requests = bid_requests, g=g, target=Impression_target), "PI2"), 
                    PID1 = bidder_base(500, Impression_target, 1, g, uniques, PID_base(payout = 1.2, margin = 0.4, k_1=0.0001, k_2 = 0.001, k_3=0.02, bid_requests, g=g, target=Impression_target), "PID1"),
                    PID2 = bidder_base(500, Impression_target, 1, g, uniques, PID_base(payout = 1.2, margin = 0.4, k_1=0.0001, k_2 = 0.01, k_3=0.05, bid_requests, g=g, target=Impression_target), "PID2"), 
                    MIPID = bidder_base(500, Impression_target, 1, g, uniques, MIPID(payout = 1.2, margin = 0.4, k_1=0.0001, k_2 = 0.01, k_3=0.05, bid_requests, g=g, target=Impression_target), "MIPID"))
    
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
  
results <- auction_engine(1, 100000, 105)
results <- auction_engine(3, 450000, 315)
outcome <- data_generator(results)
plots <- visualize(TRUE, grouping=10000, outcome)
})



timeSpan<- 1
Impression_target <- 3000
spend_budget <- 2

