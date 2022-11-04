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
ij <- t(combn(nInds, 2))
#............................................................
# generate effective distance network
#...........................................................
playnet <- sample_smallworld(dim = 1, size = nInds,
                             nei = 2, p = 0.2)
# quick viz
playnet %>%
  ggraph::ggraph(layout = 'kk') +
  ggraph::geom_edge_link()

ijt <- purrr::map2_dbl(ij[,1], ij[,2],
                       function(x,y){igraph::distances(graph = playnet,
                                                       v = x, to = y)})
# distribtuion
summary(ijt)

# add "weights" to connections
wi <- runif(n = length(ijt))

# bring together
ijdist <- data.frame(i = ij[,1], j = ij[,2],
                     tdist = wi*ijt) %>%
  dplyr::mutate(i = factor(i),
                j = factor(j))


# make exponential decay ASSUMPTION
ijdist <- ijdist %>%
  dplyr::mutate(
    probsucc = 1 - pexp(tdist, rate = 1/mean(ijdist$tdist)))

summary(ijdist$probsucc)
hist(ijdist$probsucc)

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
