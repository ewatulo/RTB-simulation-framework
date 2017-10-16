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
  
  
  bidders <- list(default1=bidder_base(spend_budget, Impression_target, 1, g, uniques, default(10, 0.4, 1.2), "default1"), 
                  default2=bidder_base(spend_budget, Impression_target, 1, g, uniques, default(5, 0.4, 1.2), "default2"),
                  bidder1=bidder_base(spend_budget, Impression_target, 1, g, uniques, randBid(g, 0, 0.5), "bidder1"), 
                  PI1 = bidder_base(spend_budget, Impression_target, 1, g, uniques, PI_base(payout = 1.2, margin = 0.4, k_1 = 0.5, k_2 = 0.0001, bid_requests = bid_requests, g=g, target=Impression_target), "PI1"),
                  PI2 = bidder_base(spend_budget, Impression_target, 1, g, uniques, PI_base(payout = 1.2, margin = 0.4, k_1 = 0.1, k_2 = 0.0001, bid_requests = bid_requests, g=g, target=Impression_target), "PI2"))

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

results <- auction_engine(3, 900000, 6300)
outcome <- data_generator(results)
})



timeSpan<- 1
Impression_target <- 3000
spend_budget <- 2

