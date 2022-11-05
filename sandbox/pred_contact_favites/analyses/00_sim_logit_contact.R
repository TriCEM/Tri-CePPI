## .................................................................................
## Purpose: Generate contact matrix from simple logit model
## of weighted average distances
##
## Author: Nick Brazeau
##
## Date: 25 October
##
## Notes: Selected RandomSingleSample transmission module in favites, which yields:
##              For each transmission, the source node is selected from the set of
##              all infected nodes in the Contact Network with equal probability,
##              and the destination node is selected from the set of all of the
##              source's uninfected neighbors with equal probability
## ---> basically did a branching process
## .................................................................................
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
set.seed(48)

#............................................................
# start params
#...........................................................
nInds <- 150
#............................................................
# generate effective distance network
#...........................................................
playnet <- tidygraph::play_smallworld(n_dim  = 1, dim_size  = nInds,
                                      order = 3, p_rewire = 0.2)
# quick viz
playnet %>%
  ggraph::ggraph(layout = 'kk') +
  ggraph::geom_edge_link()

#......................
# extract contacts
#......................
ij <- playnet %>%
  tidygraph::activate("edges") %>%
  tibble::as_tibble() %>%
  dplyr::rename(i = from,
                j = to)

# add "weights" to connections
wi <- runif(n = nrow(ij))
# bring together
ijdist <- ij %>%
  dplyr::mutate(probsucc = wi,
                i = factor(i),
                j = factor(j))


#............................................................
# Code below ran iteratively to draw new realizations from
# same underlying probability matrix
#...........................................................
#                 outcome = purrr::map_int(probsucc, function(x){
#                  return(rbinom(1, 1, prob = x))

# # outcome/plots
# mean(ijdist$outcome)
# ijdist %>%
#   ggplot() +
#   geom_point(aes(x = gdistinv, y = tdistinv, color = probsucc)) +
#   scale_color_viridis_c()

# need this for human readability and names
# get names and liftover
rnnames <- apply(combn(letters, 6), 2, function(x) paste(x, collapse = ""))
rnnames <- sample(rnnames, size = nInds, replace = F)

# loop through names
ijdist$i <- as.character(ijdist$i)
ijdist$j <- as.character(ijdist$j)
for(i in 1:nInds)  {
  ijdist$i[ijdist$i == i] <- rnnames[i]
  ijdist$j[ijdist$j == i] <- rnnames[i]
}
# sanity check
#............................................................
# send out
#...........................................................
# full
readr::write_tsv(x = ijdist,
                 file = "gears/full_contact_matrix.tab.txt")
playnet <- playnet %>%
  tidygraph::activate("nodes") %>%
  dplyr::mutate(node = rnnames)
saveRDS(playnet, file = "gears/playnet.RDS")
