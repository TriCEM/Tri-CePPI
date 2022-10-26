## .................................................................................
## Purpose: Bring together iterations of transmission
##
## Author: Nick Brazeau
##
## Date: 25 October, 2022
##
## Notes:
## .................................................................................
library(tidyverse)
library(tidygraph)

#............................................................
# read in contact networks & calc "prob"
#...........................................................
fullnet <- readr::read_tsv("gears/full_contact_matrix.tab.txt")
correctmatrix <- fullnet %>%
  dplyr::select(c("i", "j", "probsucc")) %>%
  dplyr::mutate(type = "correct")

tdistmat <- fullnet %>%
  dplyr::mutate(probsucc = 1/(1+exp(-tdistinv))) %>%
  dplyr::mutate(type = "tdistance")

gdistmat <- fullnet %>%
  dplyr::mutate(probsucc = 1/(1+exp(-gdistinv))) %>%
  dplyr::mutate(type = "gdistance")





#............................................................
# read in transmission results
#...........................................................
