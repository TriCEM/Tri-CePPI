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
find_pred_trans_model <- function(contacts = NULL, timemax = 500){

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
  # Tij right now is the probability based on effective distance as part of a bernoulli trial
  # but need to lift over to Qij the probability of I-J coming into contact as a rate
  # or probability that we can convert back
  # saying that the rate is based on prob of success (prob * 1) divided number of success
  # based on overall bernoulli success rate prob
  denom <- nrow(contacts) * mean(contacts$probsucc)

  # liftover
  Qijmat <- contacts %>%
    dplyr::mutate(Qij = (probsucc * 1)/denom,
                  Qij = 1 - exp(-Qij)) %>%
    select(c("i", "j", "Qij"))

  # contact counts lack direction, so need to make expanded df
  Qijmatexpand <- Qijmat
  colnames(Qijmatexpand) <- c("j", "i", "Qij")
  Qijmat <- dplyr::bind_rows(Qijmat, Qijmatexpand)

  #............................................................
  # Initialize
  #...........................................................
  # initialize matrix
  ret <- matrix(0, nrow = length(nodes), ncol = timemax)
  rownames(ret) <- nodes
  #......................
  # initial condition based on degree connectivity
  # sum becuase one seed is randomly selected
  #......................
  # summarize
  contact_init <- Qijmat %>%
    dplyr::group_by(i) %>%
    dplyr::summarise(
      contact_init = sum(Qij)
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
  # out in a tidy format
  retdf <- as.data.frame(ret)
  colnames(retdf) <- paste0("t", 1:timemax)
  retdf <- retdf %>%
    dplyr::mutate(node = rownames(ret)) %>%
    tidyr::pivot_longer(., cols = dplyr::starts_with("t"),
                        names_to = "time", values_to = "probInfxn") %>%
    dplyr::select(c("node", dplyr::everything()))

  return(retdf)

}



#............................................................
# read in data
#...........................................................
contacts <- readr::read_tsv("gears/full_contact_matrix.tab.txt")
probInfxns <- find_pred_trans_model(contacts = contacts, timemax = 500)
saveRDS(file = "results/probInfxns.RDS", object = probInfxns)


#............................................................
# Viz output
#...........................................................
library(tidygraph)
library(ggraph)
library(gganimate)
library(gifski)

contactnet <- readRDS("gears/playnet.RDS")

# https://stackoverflow.com/questions/46408841/retrieving-node-coordinates-from-ggraph-network-chart
# thanks SO
# graph to extract features
contactgraph <- contactnet %>%
  ggraph::ggraph(layout = 'kk') +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point(size = 3)

nodenames <- contactnet %>%
  tidygraph::activate("nodes") %>%
  tibble::as_tibble() %>%
  dplyr::pull(node)

# get edges
edges <- ggplot_build(contactgraph)[[1]][[1]]
# get nodes
nodes <- ggplot_build(contactgraph)[[1]][[2]] %>%
  dplyr::select(c("x", "y", "group")) %>%
  dplyr::mutate(node = nodenames) # assume ggplot perserves order
#TODO check order assumption
nodes <- nodes %>%
  dplyr::left_join(., probInfxns, by = "node") %>%
  dplyr::mutate(time = stringr::str_replace_all(time, "t", ""),
                time = as.integer(time))

probinfxnplot <- ggplot() +
  geom_line(data = edges,
            aes(x = x,y = y, group = group), size = 0.15) +
  geom_point(data = nodes,
             aes(x = x,y = y, group = group, color = probInfxn),
             size = 3, alpha = 0.8) +
  scale_color_viridis("Prob. Infxn") +
  gganimate::transition_time(time) +
  ease_aes('linear') +
  labs(title = "Timepoint: {frame_time}") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.title = element_text(family = "Helvetica", face = "bold", vjust = 0.85, size = 12),
        legend.text = element_text(family = "Helvetica", hjust = 0.5, vjust = 0.5, angle = 0, size = 10),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.border = element_blank())

# save out
probinfxnplotgif <- animate(probinfxnplot,
                         duration = 10, fps = 10, width = 750, height = 500,
                         renderer = gifski_renderer())
anim_save(probinfxnplotgif, filename = "~/Desktop/probinfxn.gif")
