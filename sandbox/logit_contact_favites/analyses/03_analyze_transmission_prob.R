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

  # pull infxns
  infxns <- transmission %>%
    dplyr::pull(j)

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


  # contact count denom
  C <- nrow(contact_counts)


  # contact counts lack direction, so need to make expanded df
  contact_countsexpand <- contact_counts
  colnames(contact_countsexpand) <- c("j", "i", "contact_cnt")
  contact_counts <- dplyr::bind_rows(contact_counts, contact_countsexpand)

  #......................
  # ij varying params
  #......................
  #TODO this is bad scoping
  Qijmat <- contact_counts %>%
    dplyr::mutate(Bij = contact_cnt/C) %>%
    dplyr::mutate(Qij = purrr::map_dbl(Bij, function(x){return(1-exp(-x))}))

  #......................
  # prob of infxn
  #......................
  # initialize
  ret <- matrix(0, nrow = length(infxns), ncol = sum(transmission$tte != 0) + 1)
  rownames(ret) <- infxns
  # find seeds and replace w/ prob of infxn at time 0 is 1
  iseeds <- which(rownames(ret) %in% transmission$j[transmission$tte == 0])
  ret[, 1] <- length(iseeds)/N

  for (t in 2:ncol(ret)) {
    for (i in 1:nrow(ret)) {
      # pull out connections
      ijmat <- Qijmat[rownames(ret)[i] == Qijmat$i, ]
      for (j in 1:nrow(ijmat)) {
        prevj <- ret[rownames(ret) == ijmat$j[j], t-1]
        ret[i,t] <- ret[i,t] + ijmat$Qij[j] * prevj
      } # end j connections
      ret[i,t] <- ret[i,t] + ret[i, t-1] # previ
    } # end i individual
  }



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

