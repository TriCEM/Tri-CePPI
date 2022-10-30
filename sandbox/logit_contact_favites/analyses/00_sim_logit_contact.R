## .................................................................................
## Purpose: Generate contact matrix from simple logit model
## of weighted average distances
##
## Author: Nick Brazeau
##
## Date: 25 October
## .................................................................................
library(tidyverse)
library(igraph)
set.seed(48)

#............................................................
# start params
#...........................................................
nInds <- 150
ij <- t(combn(nInds, 2))
#............................................................
# generate transport network
#...........................................................
# ba_net <-  igraph::sample_gnm(n = nInds, # Erdos-Renyi
#                               m = 800)
ba_net <-  igraph::make_tree(n = nInds,
                             children = 2)
#plot(ba_net)

ijt <- purrr::map2_dbl(ij[,1], ij[,2], function(x,y){igraph::distances(graph = ba_net,
                                                                       v = x, to = y)})
ijtransdist <- data.frame(i = ij[,1], j = ij[,2],
                          tdist = ijt) %>%
  dplyr::mutate(i = factor(i),
                j = factor(j))

#............................................................
# generate euclidean geographic distances
#...........................................................
coords <- round(seq(1, nInds*nInds, by = nInds))
coords <- expand.grid(coords, coords)
coords <- coords[sample(1:nrow(coords), size = nInds), ]
distmat <- matrix(NA, nInds, nInds)
diag(distmat) <- 1 # although distance is truly 0 w/in a deme, make 1 for identity of magnetism
# get upper triangle (for loops are slower in R but for readability)
for(l in 1:nrow(ij)) {
  # get r index, remember r counts by rows first then columns
  e <- sum(ij[l,1] + (ij[l,2]-1)*nInds)
  distmat[e] <- dist(rbind(coords[ij[l,1], ], coords[ij[l,2], ]),
                     method = "euclidean")
}
# euclidean distances are symmetric
distmat[lower.tri(distmat)] <- t(distmat)[lower.tri(distmat)]

# long format
ijgeodist <- broom::tidy(as.dist(distmat)) %>%
  magrittr::set_colnames(c("i", "j", "gdist"))

#......................
# tidy up
#......................
ijdist <- dplyr::left_join(ijtransdist, ijgeodist)
plot(ijdist$tdist, ijdist$gdist)


# standardize and invert for expit
ijdist <- ijdist %>%
  dplyr::mutate(
    tdistinv = pexp(tdist, rate = 1/mean(ijdist$tdist)),
    gdistinv = pexp(gdist, rate = 1/mean(ijdist$gdist))
  )



# viz
ijdist %>%
  ggplot() +
  geom_point(aes(x = tdistinv, y = gdistinv))
cor(ijdist$tdistinv, ijdist$gdistinv)
summary(ijdist$gdistinv)
summary(ijdist$tdistinv)


#............................................................
# making a logistic model (kind of)
#...........................................................
# prob of event is the expit model
# 1 / (1+e^{-B %*% X})
# here just taking weighted average instead from prob standardize above
wa <- 2; wb <- 1
ijdist <- ijdist %>%
  dplyr::mutate(probsucc = (wa * tdistinv + wb * gdistinv)/(wa + wb) )

summary(ijdist$probsucc)
plot(ijdist$probsucc)

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
rnnames <- randomNames::randomNames(n = nInds,
                                    which.names = "first",
                                    sample.with.replacement = F)
rnnames <- stringr::str_replace_all(rnnames, "\\'|\\-| ", "")
# loop through names
ijdist$i <- as.character(ijdist$i)
ijdist$j <- as.character(ijdist$j)
for(i in 1:nInds) {
  ijdist$i[ijdist$i == i] <- rnnames[i]
  ijdist$j[ijdist$j == i] <- rnnames[i]
}


#............................................................
# send out
#...........................................................
# full
readr::write_tsv(x = ijdist,
                 file = "gears/full_contact_matrix.tab.txt")


