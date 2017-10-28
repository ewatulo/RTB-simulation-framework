## MIPID controller based function
MIPID <- function(payout, margin, k_1, k_2, k_3, bid_requests, g, target){
  freq <- 10
  margin_weight <- 0.5
  winR_weight <- 1 - margin_weight
  margin <- margin 
  
  no_slots <- g/freq
  slot_impr_target <- target/no_slots
  optim_winRate <- slot_impr_target/freq
  currentWinrate <- 0
  currentMargin <- 0
  price <- payout*(1-margin)
  new_alpha <- 1
  
  function(userid, floor, auction_number, wins, CPMs){
    time_slot <<- auction_number%/%freq
    evaluation <<- auction_number%%freq
    if (time_slot>0 & evaluation==0){
      ex_n <- (time_slot-1)*freq + 1
      n <- time_slot*freq
      ## Win rate component
      old_winRate <- currentWinrate
      currentWinrate <<- sum(wins[ex_n:n])/freq
      
      ## Margin component
      old_margin <- currentMargin
      currentMargin <<- 1 - sum(CPMs[ex_n:n])/(sum(wins[ex_n:n])*payout)
      #print(currentMargin)
      currentMargin <<- `if`(is.nan(currentMargin), 0.01, currentMargin)
      
      d_component <- winR_weight*(currentWinrate - old_winRate) + margin_weight*(currentMargin - old_margin)
      
      rate_gap <- winR_weight * ((optim_winRate - currentWinrate)/optim_winRate) + margin_weight *((currentMargin - margin)/margin)
      
      wins <- sum(wins)
      #print(wins)
      x <- slot_impr_target * time_slot
      y <- 1 - sum(CPMs)/(wins*payout)
      y <- `if`(is.nan(y), 0.99, y)
      gamma_par <- winR_weight * ((x - wins)/x) + margin_weight * ((y - margin)/margin)
      
      new_alpha <<- new_alpha + k_1*rate_gap + k_2*gamma_par + k_3*d_component
    }
    price * new_alpha
  }
}
