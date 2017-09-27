
library(compiler)
f <- function(n, x) for (i in 1:n) x = (1 + x)^(-1)
g <- cmpfun(f)

bidder1_function <- function(floor, userid){
  #tryCatch(
    #expr = {evalWithTimeout({
      if (bidder_bookkeepers[.("bidder1")]$leftBudget<0 || bidder_bookkeepers[.("bidder1")]$leftImpressions==0){
        0
      }
      bid = floor+runif(1, -0.1, 0.5)
      bid
      #},
      #timeout = 0.3)},
    #TimeoutException = function(ex) return(floor-0.2)
  #)
}


fancy_bidder_function <- function(floor, userid){
  # tryCatch(
  #   expr = {evalWithTimeout({
      if (bidder_bookkeepers[.("fancy_bidder")]$leftBudget<0 || bidder_bookkeepers[.("bidder1")]$leftImpressions==0){
        0
      }
      bid = floor+runif(1, -0.07, 0.5)
      bid
  #    },
  #     timeout = 0.3)},
  #   TimeoutException = function(ex) return(floor-0.05)
  # )
}

## Not used closure for auction count
# increment <- function() {
#   auction_count <- 1
#   list(nextID=function() { 
#     r <- auction_count
#     auction_count <<- auction_count + 1
#     r
#   })
# }
# source <- increment()

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

  for (i in seq_len(hours)){
    unique_idfas <- idfas$idfa[sample(1:nrow(idfas), pois$idfas[i]/60, replace = TRUE)]
    dup_idfas <- rep(idfa$idfa[1:length(nCol[[i]])], as.vector(nCol[[i]]))
    bids <- c(unique_idfas, dup_idfas)
    floorPrices <- as.double(set_floor(length(bids)))
    hour <- as.integer(rep(i-1L, length(bids)))
    b <- which(is.na(data_set$idfa))[1]
    e <- b+length(bids)-1
    r_indices <- as.integer(c(b:e))
    set(data_set, i=r_indices, j=1L, bids)
    set(data_set, i=r_indices, j=2L, hour)
    set(data_set, i=r_indices, j=3L, floorPrices)
  }
  data_set <- data_set[complete.cases(data_set)]
  return(data_set)
}

set_floor <- function(x){
  return(sample(floors$bidfloor, x, replace = TRUE, prob = floors$count))
}

auction_engine <- function(timeSpan, Impression_target, spend_budget){
  
  ###### Libraries ######
  if(any(c("fst", "data.table") %in% rownames(installed.packages()) == FALSE)) {install.packages("R.utils")}
  library("fst")
  library("data.table")
  library("compiler")
  library("R.utils")
  
  ### Files upload and data frame preparation ###############
  idfa <<- as.data.table(read.fst("idfa.fst"))
  idfas <<- as.data.table(read.fst("idfas.fst"))
  floors <<- as.data.table(read.fst("floors.fst"))
  pois <<- as.data.table(read.fst("pois.fst"))
  
  ## Generate Bid Requests
  bid_requests <- generate_bidRequests(timeSpan)
  l <- nrow(bid_requests)
  
  Bidder_responses <<- data.table(bidder1=rep(0, l), fancy_bidder=rep(0, l))
  system_bidders <<- c('bidder1', 'fancy_bidder')
  target <<- NULL
  budget <<- NULL
  
  bidder1 <<- data.table(result=rep(0L, l), CPM=rep(0, l))
  fancy_bidder <<- data.table(result=rep(0L, l), CPM=rep(0, l))

  auction_number <<- 1L
  ######################################################################################
  
  set_bidders_targets(Impression_target, spend_budget)
  
  
  bid_responses <<- data.table(CPM=rep(0, l), winner=rep("", l), wonBid=rep(0, l))
  x <- mapply(secondPrice_auction, bid_requests$bidFloor, bid_requests$idfa)

  data_set <<- cbind(bid_requests, bid_responses)
  write.csv(data_set, 'simulationresult.csv')
  print('game over')
}

secondPrice_auction <- function(bidfloor, user_id){
  bids <- get_Bidresponses(bidfloor, user_id)
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
  send_auction_results(Winning_bidder_name, eCPM)
  set(bid_responses, as.integer(auction_number), 1L, eCPM)
  set(bid_responses, as.integer(auction_number), 2L, Winning_bidder_name)
  set(bid_responses, as.integer(auction_number), 3L, Winning_bid)
  auction_number <<- auction_number+1L
  NULL
}

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

set_bidders_targets <- function(target_Impressions, Bidder_budget){
  target <<- target_Impressions
  budget <<- Bidder_budget
  n <- length(system_bidders)
  bidder_bookkeepers <<- data.table(bidder=system_bidders, leftBudget=rep(budget, n), leftImpressions=rep(target, n))
  setkey(bidder_bookkeepers, bidder)
}

get_Bidresponses <- function(floor, device_id){
  a <- bidder1_function(floor, device_id)
  b <- fancy_bidder_function(floor, device_id)
  combine <- c(a, b)
  set(Bidder_responses, as.integer(auction_number), 1L, a)
  set(Bidder_responses, as.integer(auction_number), 2L, b)
  
  ### the option for large number of bidders
  #Bidder_responses[auction_number, (names(Bidder_responses)) := lapply(combine, function(x) x)]
  return(combine)
}


library("profmem")
p <- profmem({
  y <- auction_engine(1, 5000, 4)
})

library(pryr)
mem_used(y<- auction_engine(1, 5000, 4))
mem_change(y<- auction_engine(1, 5000, 4))