bidder_base <- function(budget, impression_cap, frequency_cap, bid_requests_count, unique_idfas, bidding_algo, name){
  idfa_count <- new.env(hash = TRUE, size = unique_idfas)
  
  leftBudget <- budget
  leftImpressions <- impression_cap
  
  bid_responses <- rep(0, bid_requests_count)
  wins <- rep(0, bid_requests_count)
  
  win_notification <- function(auction_count, CPM, userID){
    wins[auction_count] <<- 1L #CPM
    leftImpressions <<- leftImpressions - 1L
    leftBudget <<- leftBudget - CPM/1000
    idfa_count[[userID]] <<- c(idfa_count[[userID]], 1L)
  }

  bookkeeper_check <- function(idfa, ...){
    leftBudget>0 & length(idfa_count[[idfa]])<frequency_cap ## && leftImpressions>=0
  }
  
  bidder <- function(idfa, bidfloor, auction_number){
    if (bookkeeper_check(idfa)){
      bid <- max(bidding_algo(userid=idfa, floor=bidfloor, auction_number=auction_number, wins=wins), 0)
    }
    else {bid <- 0 }
    bid_responses[auction_number] <<- bid
    bid
  }
  return(bidder)
}

#### default bidder base and random one
default <- function(constant, margin, payout){
  function(userid, floor, ...){
    min(constant*floor, (1-margin)*payout)
  }
}

randBid <- function(bidsNo, b, e){
  rands <- runif(bidsNo, b, e)
  function(userid, floor, auction_number, ...){
    floor + rands[[auction_number]]
  }
}  

PID_base <- function(payout, margin, k_1, k_2, bid_requests, g, target){
  gamma_par <- 0
  no_slots <- g/100
  slot_impr_target <- target/no_slots
  optim_winRate <- slot_impr_target/100
  price <- payout*(1-margin)
  rate_gap <- 0
  gamma_par <- 0
  
  set <- bid_requests$bidFloor[1:100]
  set <- set[sort(set)]
  set <- set[1:slot_impr_target]
  new_alpha <- (1-margin)*payout-max(set)
  if (new_alpha<0){new_alpha=new_alpha*(-1)}
  
  rate_gap_function <- function(wins){
    ex_n <- (time_slot-1)*100 + 1
    n <- time_slot*100
    currentWinrate <- sum(wins[ex_n:n])/100
    rate_gap <<- optim_winRate - currentWinrate
  }
  
  gamma_function <- function(wins){
    gamma_par <<- sum(wins)/(slot_impr_target * time_slot)
  }
  
  function(userid, floor, auction_number, wins){
    time_slot <<- auction_number%/%100>0
    evaluation <<- auction_number%%100
    #wins <- wins>0
    if (time_slot>0 & evaluation==0){
      rate_gap_function(wins)
      gamma_function(wins)
      new_alpha <<- new_alpha - k_1*rate_gap - k_2*gamma_par
      print(new_alpha)
    }
    price - new_alpha
  }
}

# bidder_base1 <- function(budget, impression_cap, frequency_cap, bid_requests_count, unique_idfas, bidding_algo, name){
#   
#   e <- new.env()
#   e$idfa_count <- new.env(hash = TRUE, size = unique_idfas)
# 
#   e$leftBudget <- budget
#   e$leftImpressions <- impression_cap
# 
#   e$bid_responses <- rep(0, bid_requests_count)
#   e$wins <- rep(0, bid_requests_count)
# 
#   e$win_notification <- function(auction_count, CPM, userID){
#     e$wins[auction_count] <<- CPM
#     e$leftImpressions <<- e$leftImpressions - 1
#     e$leftBudget <<- e$leftBudget - CPM/1000
#     e$idfa_count[[userID]] <<- c(e$idfa_count[[userID]], 1L)
#   }
# 
#   e$bookkeeper_check <- function(idfa, ...){
#     e$leftBudget>0 & length(e$idfa_count[[idfa]])<frequency_cap #&& leftImpressions>=0
#   }
# 
#   e$bidder <- function(idfa, bidfloor, auctionNo){
#     if (e$bookkeeper_check(idfa)){
#       bid <- max(bidding_algo(idfa, bidfloor, auctionNo), 0)
#     }
#     else {bid <- 0 }
#     e$bid_responses[auctionNo] <<- bid
#     return(bid)
#   }
#   return(e$bidder)
# }
# 

# win_notification <- function(x, auction_count, CPM, userID){
#   x$wins[auction_count] <- CPM
#   x$leftImpressions <- x$leftImpressions - 1
#   x$leftBudget <- x$leftBudget - CPM/1000
#   x$idfa_count[[userID]] <- c(x$idfa_count[[userID]], 1L)
#   invisible()
# }
