###### Libraries ######
setwd("/Users/ewa/Desktop/bidding simulation")
if(any(c("fst", "data.table") %in% rownames(installed.packages()) == FALSE)) {install.packages("R.utils")}
library("fst")
library("data.table")
library("compiler")
library("R.utils")

defaultBidder_function <- function(constant, margin, payout, hashkey){
  bookkeep <- function(hashkey){
    bidder_bookkeepers[.(hashkey)]$leftBudget>0 && bidder_bookkeepers[.(hashkey)]$leftImpressions>=0
  }
    bidder <- function(floor, userid){
    price = min(constant*floor, (1-margin)*payout*bookkeep(hashkey))
    return(price)
    }
    return(bidder)
}

default1_function <- defaultBidder_function(10, 0.3, 1.30, "default1")
default2_function <- defaultBidder_function(5, 0.4, 1.2, "default2")
default3_function <- defaultBidder_function(2, 0.45, 1.1, "default3")

default1_function_c <- cmpfun(default1_function)
default2_function_c <- cmpfun(default2_function)
default3_function_c <- cmpfun(default3_function)

bidder1_function <- function(floor, userid){
  #tryCatch(
  #expr = {evalWithTimeout({
  bookkeep <- function(){
    bidder_bookkeepers[.("bidder1")]$leftBudget>0 && bidder_bookkeepers[.("bidder1")]$leftImpressions>=0
  }
  bid = (floor+runif(1, 0, 0.5))#*bookkeep()
  bid
  #},
  #timeout = 0.3)},
  #TimeoutException = function(ex) return(floor-0.2)
  #)
}
bidder1_function_c <- cmpfun(bidder1_function)

### PI Controller based bidding algorithm
fancy_bidder_function <- function(floor, userid){
  t <- function(){time_slot <<- auction_number%/%100}
  e <- function(){evaluation <<- auction_number%%100}
  
  t()
  e()
  payout <- 1.2
  margin <- 0.4
  price <- payout*(1-margin)
  k_1 <- 0.9
  k_2 <- 0.0001
  new_alpha <- 0
  rate_gap <- 0
  gamma_par <- 0

  evaluation_function <- function(){
    evaluation==0 & time_slot>0
  }
  
  update_function <- function(){
    if (time_slot==1){
      no_slots <- l/100
      slot_impr_target <<- target/no_slots
      optim_winRate <<- slot_impr_target/100
      
      set <- bid_requests[1:100]
      set <- set[, sort(bidFloor)]
      selected <- set[1:slot_impr_target]
      
      alpha = (1-margin)*payout-max(selected)
      if (alpha<0){alpha=alpha*(-1)}
      new_alpha <<- alpha
    }
    else{
      rate_gap_function()
      gamma_function(time_slot)
      new_alpha <<- new_alpha - k_1*rate_gap - k_2*gamma_par
    }
  }
  
  rate_gap_function <- function(){
    ex_n <- (time_slot-1)*100 + 1
    n <- time_slot*100
    currentWinrate <- sum(fancy_bidder$result[ex_n:n])/100
    rate_gap <<- optim_winRate - currentWinrate
  }
  
  trapezoid_composite <- function(f, a, b, n) {
    points <- seq(a, b, length = n + 1)
    h <- (b - a) / n
    
    area <- 0
    for (i in seq_len(n)) {
      area <- area + h / 2 * (f(points[i]) + f(points[i + 1]))
    }
    area
  }
  
  gamma_function <- function(x){
    integrand <-  function(x) {sum(fancy_bidder$result)/(slot_impr_target * x)}
    #gamma_par <<- trapezoid_composite(integrand, 0, x, 10)
    gamma_par <<- integrand(x)
  }
  
  bookkeep <- function(){
    bidder_bookkeepers[.("fancy_bidder")]$leftBudget>0 && bidder_bookkeepers[.("fancy_bidder")]$leftImpressions>=0
  }
  
  bidder <- function(){
    if (time_slot>0){
      if (evaluation_function()){
        update_function()}
      return(bookkeep()*(price - new_alpha))
    }
    else {
      return(bid=budget/target*1000*0.3)}
  }
  return(bidder())
}
fancy_bidder_function_c <- cmpfun(fancy_bidder_function)

generate_bidRequests <- function(hours){
  set.seed(100)
  all_uniques <- sum(pois$idfas[seq_len(hours)])
  nCol <<- as.list(seq_len(hours))
  
  repeated_idfas <- sum(sapply(seq_len(hours), function(x) {
    nCol[[x]] <<-rpois(pois$Pois[x]/10000, 1)+2
    sum(nCol[[x]])}
  ))
  
  s <- sum(all_uniques, repeated_idfas)
  data_set <- data.table(idfa=as.character(rep(NA, s)), timestamp=as.integer(rep(0, s)), bidFloor=as.double(rep(0, s)))
  
  lapply(seq_len(hours), function(x){
    unique_idfas <- idfas$idfa[sample(1:nrow(idfas), pois$idfas[x]/60, replace = TRUE)]

    dup_idfas <- rep(idfa$idfa[1:length(nCol[[x]])], as.vector(nCol[[x]]))
    bids <- c(unique_idfas, dup_idfas)
    floorPrices <- as.double(set_floor(length(bids)))
    hour <- as.integer(rep(x-1L, length(bids)))
    b <- which(is.na(data_set$idfa))[1]
    e <- b+length(bids)-1
    r_indices <- as.integer(c(b:e))
    set(data_set, i=r_indices, j=1L, bids)
    set(data_set, i=r_indices, j=2L, hour)
    set(data_set, i=r_indices, j=3L, floorPrices)
  })
  bid_requests <<- data_set[complete.cases(data_set)]
  NULL
}
generate_bidRequests_c <- cmpfun(generate_bidRequests)

set_floor <- function(x){
  sample(floors$bidfloor, x, replace = TRUE, prob = floors$count)
}
set_floor_c <- cmpfun(set_floor)

auction_engine <- function(timeSpan, Impression_target, spend_budget){
  ### Files upload and data frame preparation ###############
  idfa <<- as.data.table(read.fst("idfa.fst"))
  idfas <<- as.data.table(read.fst("idfas.fst"))
  floors <<- as.data.table(read.fst("floors.fst"))
  pois <<- as.data.table(read.fst("pois.fst"))
  
  ## Generate Bid Requests
  #bid_requests <- generate_bidRequests(timeSpan)
  generate_bidRequests_c(timeSpan)
  l <<- nrow(bid_requests)
  
  Bidder_responses <<- data.table(bidder1=rep(0, l), fancy_bidder=rep(0, l), default1=rep(0, l), default2=rep(0, l), default3=rep(0, l), waterlevel=rep(0, l))
  system_bidders <<- c('bidder1', 'fancy_bidder', 'default1', 'default2', 'default3', 'waterlevel')
  #system_bidders <<- c('bidder1', 'fancy_bidder', 'default1', 'default2', 'default3')
  target <<- NULL
  budget <<- NULL
  
  bidder1 <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  fancy_bidder <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  default1 <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  default2 <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  default3 <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  waterlevel <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  
  auction_number <<- 1L
  ######################################################################################
  
  set_bidders_targets_c(Impression_target, spend_budget)
  
  
  bid_responses <<- data.table(CPM=rep(0, l), winner=rep("", l), wonBid=rep(0, l))
  mapply(secondPrice_auction_c, bid_requests$bidFloor, bid_requests$idfa)
  
  data_set <<- cbind(bid_requests, bid_responses)
  write.fst(data_set, 'simulationresult.fst')
  print('game over')
}
auction_engine_c <- cmpfun(auction_engine)

secondPrice_auction <- function(bidfloor, user_id){
  bids <- get_Bidresponses_c(bidfloor, user_id)
  names(bids) <- system_bidders
  
  if ((!is.na(any(bids>bidfloor))) & (any(bids>bidfloor))){
    if (length(bids[bids>bidfloor])>1){
      Winning_bid = max(bids)
      Winning_bidder_name = names(bids[which.max(bids)])
      bids <- bids[-which.max(bids)]
      eCPM <- max(bids)
    }
    else{
      eCPM = bidfloor
      Winning_bid = max(bids)
      Winning_bidder_name = names(bids[which.max(bids)])
    }
  }
  else{
    eCPM = max(bids)
    Winning_bid= max(bids)
    Winning_bidder_name = names(bids[which.max(bids)])
  }
  send_auction_results_c(Winning_bidder_name, eCPM)
  set(bid_responses, as.integer(auction_number), 1L, eCPM)
  set(bid_responses, as.integer(auction_number), 2L, Winning_bidder_name)
  set(bid_responses, as.integer(auction_number), 3L, Winning_bid)
  auction_number <<- auction_number+1L
}
secondPrice_auction_c <- cmpfun(secondPrice_auction)

send_auction_results <- function(winner, CPM){
  for (i in system_bidders){
    if(i==winner){
      set(get(i), as.integer(auction_number), 1L, 1)
      set(get(i), as.integer(auction_number), 2L, CPM)
      bidder_bookkeepers[.(i), leftBudget:=leftBudget-(CPM/1000)]
      bidder_bookkeepers[.(i), leftImpressions:=leftImpressions-1]
    }
  }
}
send_auction_results_c <- cmpfun(send_auction_results)

set_bidders_targets <- function(target_Impressions, Bidder_budget){
  target <<- target_Impressions
  budget <<- Bidder_budget
  n <- length(system_bidders)
  bidder_bookkeepers <<- data.table(bidder=system_bidders, leftBudget=rep(budget, n), leftImpressions=rep(target, n))
  setkey(bidder_bookkeepers, bidder)
}
set_bidders_targets_c <- cmpfun(set_bidders_targets)

get_Bidresponses <- function(floor, device_id){
  a <- bidder1_function_c(floor, device_id)
  b <- fancy_bidder_function_c(floor, device_id)
  c <- default1_function_c(floor, device_id)
  d <- default2_function_c(floor, device_id)
  e <- default3_function_c(floor, device_id)
  f <- waterlevel_bidder_function_c(floor, device_id)
  
  combine <- c(a, b, c, d, e, f)
  #combine <- c(a, b, c, d, e)
  set(Bidder_responses, as.integer(auction_number), 1L, a)
  set(Bidder_responses, as.integer(auction_number), 2L, b)
  set(Bidder_responses, as.integer(auction_number), 3L, c)
  set(Bidder_responses, as.integer(auction_number), 4L, d)
  set(Bidder_responses, as.integer(auction_number), 5L, e)
  set(Bidder_responses, as.integer(auction_number), 6L, f)
  
  ### the option for large number of bidders
  #Bidder_responses[auction_number, (names(Bidder_responses)) := lapply(combine, function(x) x)]
  combine
}
get_Bidresponses_c <- cmpfun(get_Bidresponses)


# library(pryr)
# mem_used(auction_engine(1, 5000, 4))
# mem_change(auction_engine(1, 3000, 2))
# system.time(auction_engine_c(1, 3000, 2))
# system.time(auction_engine(1, 10, 10))

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
