# generate_bidRequests <- function(hours){
#   idfa <- as.data.table(read.fst("idfa.fst"))
#   idfas <- as.data.table(read.fst("idfas.fst"))
#   pois <- as.data.table(read.fst("pois.fst"))
#   
#   all_uniques <- sum(pois$idfas[seq_len(hours)])
#   nCol <<- as.list(seq_len(hours))
#   
#   repeated_idfas <- sum(sapply(seq_len(hours), function(x) {
#     nCol[[x]] <<-rpois(pois$Pois[x], 1)+2
#     sum(nCol[[x]])}
#   ))
#   
#   s <- sum(all_uniques, repeated_idfas)
#   bid_requests <<- list(idfa=rep(as.character(NA), s), timestamp=rep(0L, s), bidFloor=rep(NaN, s))
#   
#   lapply(seq_len(hours), function(x){
#     unique_idfas <- idfas$idfa[sample(1:nrow(idfas), pois$idfas[x], replace = TRUE)]
#     
#     dup_idfas <- rep(idfa$idfa[1:length(nCol[[x]])], as.vector(nCol[[x]]))
#     bids <- c(unique_idfas, dup_idfas)
#     floorPrices <- set_floor(length(bids))
#     hour <- rep(x-1L, length(bids))
#     b <- which(is.na(bid_requests$idfa))[1]
#     e <- b+length(bids)-1L
#     r_indices <- as.integer(c(b:e))
#     bid_requests$idfa[r_indices]<- bids
#     bid_requests$timestamp[r_indices]<<-hour
#     bid_requests$bidFloor[r_indices]<<-as.double(floorPrices)
#     NULL
#   })
#   NULL
# }

generate_bidRequests <- function(hours){
  idfa <- as.data.table(read.fst("idfa.fst"))
  idfas <- as.data.table(read.fst("idfas.fst"))
  pois <- as.data.table(read.fst("pois.fst"))

  all_uniques <- sum(pois$idfas[seq_len(hours)])
  nCol <<- as.list(seq_len(hours))

  repeated_idfas <- sum(sapply(seq_len(hours), function(x) {
    nCol[[x]] <<-rpois(pois$Pois[x]/10000, 1)+2
    sum(nCol[[x]])}
  ))

  s <- sum(all_uniques, repeated_idfas)
  data_set <- data.table(idfa=rep(as.character(NA), s), timestamp=rep(0L, s), bidFloor=rep(NaN, s))

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
    NULL
  })
  bid_requests <<- as.list(data_set[complete.cases(data_set)])
  NULL
}

set_floor <- function(x){
  floors <- as.data.table(read.fst("floors.fst"))
  sample(floors$bidfloor, x, replace = TRUE, prob = floors$count)
}
