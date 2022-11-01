## .................................................................................
## Purpose: Probability of infection given time versus draws of realized infxn times
## from FAVITES
##
## Author: Nick Brazeau
##
## Date: 01 November, 2022
##
## Notes: Selected RandomSingleSample transmission module in favites, which yields:
##              For each transmission, the source node is selected from the set of
##              all infected nodes in the Contact Network with equal probability,
##              and the destination node is selected from the set of all of the
##              source's uninfected neighbors with equal probability
## ---> basically did a branching process
## .................................................................................
library(tidyverse)
library(goodegg)
#............................................................
# functions
#...........................................................
#' @title Model for Determining Probability of Infection from Contact Matrix
#' @param transmission dataframe; realized transmission dataframe from FAVITES
#' @param contacts dataframe; realized contact dataframe from our internal R script
#TODO should be able to read in config file to determine N
find_pred_trans_model <- function(transmission = NULL, contacts = NULL, N = 150){

  #......................
  # assertions
  #......................
  goodegg:::assert_dataframe(transmission)
  if(!all(colnames(transmission) %in% c("i", "j", "tte"))){
    stop("Must have i, j, tte colnames in transmission df")
  }
  goodegg:::assert_dataframe(contacts)
  if(!all(colnames(contacts) %in% c("i", "j"))){
    stop("Must have i, j colnames in contact df")
  }

  #......................
  # fixed parameters
  #......................
  infxns <- transmission %>%
    dplyr::pull(j)
  prob_infxn <- length(infxns)/nrow(contacts)
  toe <- transmission$tte
  Nt <- N - cumsum(table(toe)) # NB, we are 0 indexed here

  #......................
  # contact counts
  #......................
  contact_counts <- contacts %>%
    dplyr::mutate(uconnect = purrr::map2_chr(i, j,
                                             function(x,y){paste(sort(c(x,y)), collapse = "|")})) %>%
    dplyr::group_by(uconnect) %>%
    dplyr::summarise(
      contact_cnt = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(i = stringr::str_split_fixed(uconnect, "\\|", n=2)[,1],
                  j = stringr::str_split_fixed(uconnect, "\\|", n=2)[,2]) %>%
    dplyr::select(c("i", "j", "contact_cnt"))

  # contact counts lack direction, so need to make expanded df
  contact_countsexpand <- contact_counts
  colnames(contact_countsexpand) <- c("j", "i", "contact_cnt")
  contact_counts <- dplyr::bind_rows(contact_counts, contact_countsexpand)

  #......................
  # ij varying params
  #......................
  #TODO this is bad scoping
  Qijmat <- contact_counts %>%
    dplyr::mutate(Cij = 1 - (1-prob_infxn)^contact_cnt) %>%
    dplyr::mutate(Bij = purrr::map(Cij, function(x){return(x/Nt)})) %>%
    dplyr::mutate(Qij = purrr::map(Bij, function(x){return(1-exp(-x))}))

  #......................
  # prob of infxn
  #......................
  # initialize
  p0 <- table(toe)[1]/N
  ret <- matrix(0, nrow = length(infxns), ncol = length(Nt))
  rownames(ret) <- infxns
  ret[, 1] <- p0 # prob of infxn at time 0
  # find seeds and replace w/ prob of infxn at time 0 is 1
  iseeds <- which(rownames(ret) %in% transmission$j[transmission$tte == 0])
  ret[iseeds, 1] <- 1

  for(t in 2:ncol(ret)){
    for (i in 1:nrow(ret)) {
      # find init
      previ <- ret[i, t-1]
      iterQijmat <- Qijmat[Qijmat$i == rownames(ret)[i], ]
      for(j in 1:nrow(iterQijmat)) {
        prevj <- ret[rownames(ret) == iterQijmat$j[j], t-1]
        Qijcalc <- Qijmat$Qij[[i]][t]
        # bring together
        ret[i,t] <- ret[i,t] + Qijcalc * prevj
      } # end j
      # now account for i before next time step
     # ret[i,t] <- ret[i,t] * (1-previ)
    } # end i
  } # end time step

}



#............................................................
# read in data
#...........................................................
transmission <- readr::read_tsv("results/mycontact_out_ret_1/error_free_files/transmission_network.txt.gz",
                                col_names = F) %>%
  magrittr::set_colnames(c("i", "j", "tte"))

contacts <- readr::read_tsv("gears/favites_realized_contact_matrix_1.tab.txt",
                            col_names = F) %>%
  magrittr::set_colnames(c("type", "i", "j", "details", "direction")) %>%
  dplyr::filter(type == "EDGE") %>%
  dplyr::select(c("i", "j"))

