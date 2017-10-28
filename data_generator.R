data_generator <- function(l){
  responses <- data.frame(lapply(l$Bidders, function(x) environment(x)$bid_responses))
  
  auctions <- bind_cols(l$Requests, l$Auctions)
  
  #impressions <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins))
  impressions <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins>1))
  
  #wins <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins*auctions$CPM))
  wins <- data.frame(lapply(l$Bidders, function(x) as.integer(environment(x)$wins>0)*auctions$CPM))
  
  payouts <- lapply(l$Bidders, function(x) environment(environment(x)$bidding_algo)$payout)
  payouts <- lapply(payouts, function(x) if(is.null(x)) {0} else{x})
  p <- data.frame(bidder=names(payouts), payout=unlist(payouts))
  p$bidder <- as.character(p$bidder)
  
  performance_summary <- auctions %>% group_by(bidder=winner) %>% summarise(Impressions=n(), Spend=sum(CPM)/1000, avgCPM = Spend/Impressions * 1000) %>%
    inner_join(., p, by="bidder") %>% mutate(Profit = Impressions*0.001*payout - Spend)
  
  return(list(responses=responses, impressions=impressions, wins=wins, auctions=auctions, performance=performance_summary, payouts=payouts))
}
