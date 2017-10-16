visualize <- function(timeSlots, grouping=10000, dataset, engine_output){
  #### if simulation is run only for one hour -> timeSlots = TRUE, otherwise the data aggregation will be based on hourly time resolution
  if (timeSlots=TRUE){
    nr <- nrow(dataset$impressions)
    n <- grouping
    impressions1 <- aggregate(dataset$impressions, by=list(rep(1:((nr)%/%n+1),each=n,len=nr)),sum)[-1] 
    impressions <- impressions1 %>% mutate(timestamp=1:nrow(impressions1)) %>% gather(key=bidder, value=auctions, names(impressions1))

    spend1 <- aggregate(dataset$wins, by=list(rep(1:((nr)%/%n+1),each=n,len=nr)),sum)[-1]/1000
    spend <- spend1 %>% mutate(timestamp=1:nrow(spend1)) %>% gather(key=bidder, value=Spend, names(spend1)) 

    win_rate <- aggregate(dataset$impressions, by=list(rep(1:((nr)%/%n+1),each=n,len=nr)),sum)[-1]/n
    win_rate <- win_rate %>% mutate(timestamp=1:nrow(win_rate)) %>% gather(key=bidder, value=winRate, names(win_rate))

    revenue1 <- data.frame(lapply(seq_along(dataset$payouts), function(x) dataset$payouts[x]*dataset$impressions[x]/1000))
    revenue1 <- aggregate(revenue1, by=list(rep(1:((nr)%/%n+1),each=n,len=nr)),sum)[-1]
    revenue <- revenue1 %>% mutate(timestamp=1:nrow(revenue1)) %>% gather(key=bidder, value=revenue, names(revenue1))

    profit1 <- revenue1-spend1
    profit <- profit1 %>% mutate(timestamp=1:nrow(profit1)) %>% gather(key=bidder, value=profit, names(profit1)) 

    margin <- profit1/revenue1
    margin <- margin %>% mutate(timestamp=1:nrow(margin)) %>% gather(key=bidder, value=margin, names(margin))

    meanCPM <- spend1/impressions1*1000
    meanCPM <- meanCPM %>% mutate(timestamp=1:nrow(meanCPM)) %>% gather(key=bidder, value=meanCPM, names(meanCPM)) 

    averageBid <- aggregate(dataset$responses, by=list(rep(1:((nr)%/%n+1),each=n,len=nr)),mean)[-1]
    averageBid <- averageBid %>% mutate(timestamp=1:nrow(averageBid)) %>% gather(key=bidder, value=averageBid, names(averageBid))
  }
  else {
    
    impressions1 <- outcome$auctions %>% group_by(timestamp, bidder=winner) %>% summarise(auctions=n())

    spend1 <- outcome$auctions %>% group_by(timestamp, bidder=winner) %>% summarise(Spend=sum(CPM)/1000)

    win_rate <- outcome$auctions %>% group_by(timestamp) %>% summarize(count=n()) %>% 
      right_join(outcome$auctions %>% group_by(timestamp, bidder=winner) %>% summarise(auctions=n()), by="timestamp") %>%
      group_by(timestamp, bidder) %>% summarize(winRate=auctions/count)

    revenue1 <- data.frame(lapply(seq_along(outcome$payouts), function(x) outcome$payouts[x]*outcome$impressions[x]/1000))
    revenue1 <- bind_cols(revenue1, timestamp=outcome$auctions$timestamp) %>% gather(key=bidder, value=revenue, names(revenue1)) %>% 
      group_by(timestamp, bidder) %>% summarise(revenue=sum(revenue))

    profit1 <- merge(revenue1, spend1, by=c('timestamp', 'bidder'), all.x = TRUE, all.y = TRUE) %>% mutate(profit=revenue-Spend)
    profit1 <- profit1[-which(profit1$bidder=='bidder1'),]

    margin <- data.frame(profit1, margin=profit1$profit/profit1$revenue)

    meanCPM <- data.frame(spend1, meanCPM=spend1$Spend/impressions1$auctions*1000)

    averageBid <- cbind(outcome$responses, timestamp=outcome$auctions$timestamp) %>% 
      gather(key=bidder, value=averageBid, names(.[,-length(.)])) %>% group_by(timestamp, bidder) %>% summarise(averageBid=mean(averageBid))
  }
  
  impressions <- impressions1 %>% ggplot(., aes(x=timestamp, y=auctions, color=bidder))+geom_line()+theme_bw() + ggtitle("Bidders' Impressions over time")
  
  spend <- spend1 %>% ggplot(., aes(x=timestamp, y=Spend, color=bidder))+geom_line()+theme_bw() + ggtitle("Bidders' Spend over time")
  
  win_rate <- win_rate %>%  ggplot(., aes(x=timestamp, y=winRate, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' Win Rate over time")+ylab("Win Rate") + scale_y_continuous(labels = scales::percent)
  
  revenue <- revenue1 %>% ggplot(., aes(x=timestamp, y=revenue, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' revenue over time")+ylab("revenue")
  
  profit <- profit1 %>% ggplot(., aes(x=timestamp, y=profit, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' profit over time")+ylab("revenue")
  
  margin <- margin %>% ggplot(., aes(x=timestamp, y=margin, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' margin over time")+ylab("margin") + scale_y_continuous(labels = scales::percent)
  
  meanCPM <- meanCPM %>% ggplot(., aes(x=timestamp, y=meanCPM, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' average CPM over time")+ylab("average CPM")
  
  averageBid <- averageBid %>% ggplot(., aes(x=timestamp, y=averageBid, color=bidder))+geom_line()+theme_bw() + 
    ggtitle("Bidders' average bid price over time")+ylab("Average Bid Price")
  
  return(list(impressions=impressions, spend=spend, winRate=win_rate, revenue=revenue, profit=profit, margin=margin, 
              meanCPM=meanCPM, meanBid=averageBid))
}  
