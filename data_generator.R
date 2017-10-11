data_generator <- function(l){
  responses <- data.frame(lapply(l$Bidders, function(x) environment(x)$bid_responses))
  
  auctions <- bind_cols(l$Requests, l$Auctions)
  
  wins <- data.frame(lapply(l$Bidders, function(x) environment(x)$wins*auctions$CPM))
  
  performance_summary <- auctions %>% group_by(bidder=winner) %>% summarise(Impressions=n(), Spend=sum(CPM)/1000, avgCPM = Spend/Impressions * 1000)
  
  return(list(responses=responses, wins=wins, auctions=auctions, performance=performance_summary))
}
