#############Bidder constructors #############
## Bidder base 1
bidder_base <- function(budget, impression_cap, frequency_cap, bid_requests_count, unique_idfas, bidding_algo, name){
  idfa_count <- new.env(hash = TRUE, size = unique_idfas)
  
  leftBudget <- budget
  leftImpressions <- impression_cap
  
  bid_responses <- rep(0, bid_requests_count)
  wins <- rep(0, bid_requests_count)
  notifications <- rep(0L, bid_requests_count)
  
  win_notification <- function(auction_count, CPM, userID){
    wins[auction_count] <<- CPM
    notifications[auction_count] <<- 1L

    leftImpressions <<- leftImpressions - 1L
    leftBudget <<- leftBudget - CPM/1000
    idfa_count[[userID]] <<- c(idfa_count[[userID]], 1L)
  }
  
  bookkeeper_check <- function(idfa, ...){
    leftBudget>0 & length(idfa_count[[idfa]])<frequency_cap & leftImpressions>0
  }
  
  bidder <- function(idfa, bidfloor, auction_number){
    if (bookkeeper_check(idfa)){
      bid <- max(bidding_algo(userid=idfa, floor=bidfloor, auction_number=auction_number, notifications, wins), 0)
    }
    else {bid <- 0 }
    bid_responses[auction_number] <<- bid
    bid
  }
  return(bidder)
}

## Bidder base for random function
random_base <- function(bid_requests_count, bidding_algo, name){
  bid_responses <- rep(0, bid_requests_count)
  wins <- rep(0, bid_requests_count)
  
  win_notification <- function(auction_count, CPM, userID){
    wins[auction_count] <<- CPM  #1L
  }
  
  bidder <- function(idfa, bidfloor, auction_number){
    bid <- max(bidding_algo(userid=idfa, floor=bidfloor, auction_number=auction_number), 0)

    bid_responses[auction_number] <<- bid
    bid
  }
  return(bidder)
}

############# Bidding Functions #############
## Default function
default <- function(constant, margin, payout){
  function(userid, floor, ...){
    min(constant*floor, (1-margin)*payout)
  }
}


## Static function
static <- function(price, payout){
  price = price
  payout = payout
  function(...){
    price
  }
}

## Random Function
randBid <- function(bidsNo, b, e){
  rands <- runif(bidsNo, b, e)
  function(userid, floor, auction_number, ...){
    floor + rands[[auction_number]]
  }
}  

## PI controller based function
PI_base <- function(payout, margin, k_1, k_2, bid_requests, g, target){
  freq <- 10
  no_slots <- g/freq
  slot_impr_target <- target/no_slots
  optim_winRate <- slot_impr_target/freq
  price <- payout*(1-margin)
  new_alpha <- 1
  
  function(userid, floor, auction_number, notifications, ...){
    time_slot <<- auction_number%/%freq
    evaluation <<- auction_number%%freq
    # wins <- wins>0
    if (time_slot>0 & evaluation==0){
      # rate_gap_function(wins)
      # rate_gap_function <- function(){
      ex_n <- (time_slot-1)*freq + 1
      n <- time_slot*freq
      currentWinrate <- sum(notifications[ex_n:n])/freq
      rate_gap <- (optim_winRate - currentWinrate)/optim_winRate
      x <- slot_impr_target * time_slot
      gamma_par <- (x - sum(notifications))/(x)
      # }
      # rate_gap_function()

      new_alpha <<- new_alpha + k_1*rate_gap + k_2*gamma_par
    }
    price * new_alpha
  }
}

## PID controller based function
PID_base <- function(payout, margin, k_1, k_2, k_3, bid_requests, g, target){

  freq <- 10
  no_slots <- g/freq
  slot_impr_target <- target/no_slots
  optim_winRate <- slot_impr_target/freq
  currentWinrate <- 0
  price <- payout*(1-margin)
  new_alpha <- 1
  
  function(userid, floor, auction_number, notifications, ...){
    time_slot <<- auction_number%/%freq
    evaluation <<- auction_number%%freq
    # wins <- wins>0
    if (time_slot>0 & evaluation==0){
      # rate_gap_function(wins)
      # rate_gap_function <- function(){
      ex_n <- (time_slot-1)*freq + 1
      n <- time_slot*freq
      old_winRate <- currentWinrate
      currentWinrate <<- sum(notifications[ex_n:n])/freq
      d_component <- currentWinrate - old_winRate
      rate_gap <- (optim_winRate - currentWinrate)/optim_winRate
      x <- slot_impr_target * time_slot
      gamma_par <- (x - sum(notifications))/x
      # }
      # rate_gap_function()


      new_alpha <<- new_alpha + k_1*rate_gap + k_2*gamma_par + k_3*d_component
    }
    price * new_alpha
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
