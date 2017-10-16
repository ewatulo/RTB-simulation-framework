data_generator <- function(l){
  responses <- data.frame(lapply(l$Bidders, function(x) environment(x)$bid_responses))
  
  auctions <- bind_cols(l$Requests, l$Auctions)
  
  impressions <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins))
  
  wins <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins*auctions$CPM))
  
  performance_summary <- auctions %>% group_by(bidder=winner) %>% summarise(Impressions=n(), Spend=sum(CPM)/1000, avgCPM = Spend/Impressions * 1000)
  
  payouts <- lapply(l$Bidders, function(x) environment(environment(x)$bidding_algo)$payout)
  payouts <- lapply(payouts, function(x) if(is.null(x)) {0} else{x})
  
  return(list(responses=responses, impressions=impressions, wins=wins, auctions=auctions, performance=performance_summary, payouts=payouts))
}
