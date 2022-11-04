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
#' @param contacts dataframe; realized contact dataframe from our internal R script
#' @param timemax numeric; expected max time to run out simulation
#TODO should be able to read in config file to determine N
find_pred_trans_model <- function(contacts = NULL, timemax = 150){

  #......................
  # assertions
  #......................
  goodegg:::assert_dataframe(contacts)
  if(!all(c("i", "j") %in% colnames(contacts))){
    stop("Must have i, j colnames in contact df")
  }

  # pull nodes
  nodes <- unique(c(contacts$i, contacts$j))

  #......................
  # ij varying params
  #......................
  # contact counts lack direction, so need to make expanded df
  contactsexpand <- contacts
  colnames(contactsexpand) <- c("j", "i", "tdist", "probsucc")
  contacts <- dplyr::bind_rows(contacts, contactsexpand)
  # Tij right now is the probability based on effective distance as part of a bernoulli trial
  # but need to lift over to Qij the probability of I-J coming into contact
  # as a proper probability matrix
  contactdenom <- contacts %>%
    dplyr::group_by(i) %>%
    dplyr::summarise(
      denom = sum(probsucc)
    )


  # liftover
  Qijmat <- contacts %>%
    dplyr::left_join(., contactdenom) %>%
    dplyr::mutate(Qij = probsucc/denom) %>%
    select(c("i", "j", "Qij")) %>%
    dplyr::ungroup()

  #............................................................
  # Initialize
  #...........................................................
  # initialize matrix
  ret <- matrix(0, nrow = length(nodes), ncol = timemax)
  rownames(ret) <- nodes
  #......................
  # initial condition based on degree connectivity
  #......................
  # summarize
  contact_init <- Qijmat %>%
    dplyr::group_by(i) %>%
    dplyr::summarise(
      contact_init = mean(Qij)
    )

  # initial condition
  ret[,1] <- contact_init$contact_init[match(contact_init$i, rownames(ret))]

  #............................................................
  # Run crux
  #...........................................................
  # run through
  for (t in 2:ncol(ret)) {
    for (i in 1:nrow(ret)) {
      # pull out connections
      ijmat <- Qijmat[rownames(ret)[i] == Qijmat$i, ]
      for (j in 1:nrow(ijmat)) {
        prevj <- ret[rownames(ret) == ijmat$j[j], t-1]
        ret[i,t] <- ret[i,t] + ijmat$Qij[j] * prevj
      } # end j connections
      ret[i,t] <- ret[i,t] * (1-ret[i, t-1]) + ret[i, t-1] # previ
    } # end i individual
  }



}



#............................................................
# read in data
#...........................................................
contacts <- readr::read_tsv("gears/full_contact_matrix.tab.txt")
