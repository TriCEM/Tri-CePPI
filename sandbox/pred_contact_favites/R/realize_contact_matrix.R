#!/usr/bin/env Rscript

#..............................................................
# Purpose of this script is to make realizations of the
# contact matrix we generated
#..............................................................
library(tidyverse)
library(optparse)
DIR <- "/Users/nbrazeau/Documents/Github/Tri-CePPI/sandbox/logit_contact_favites/"
source(paste0(DIR, "R/", "utils.R"))

option_list=list(
  make_option(c("-I", "--iter"),
              type = "character",
              default = NULL,
              help = paste("Iteration"),
              metavar = "character")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

#............................................................
# Realize from contact matrix
#...........................................................
ijdist <- readr::read_tsv(paste0(DIR, "gears/full_contact_matrix.tab.txt"))
ijdist <- ijdist %>%
  dplyr::mutate(outcome = purrr::map_int(probsucc, function(x){
                  return(rbinom(1, 1, prob = x))
                }))
#......................
# liftover for favites
#......................
realized_contacts <- ijdist %>%
  dplyr::filter(outcome == 1) %>%
  dplyr::select(c("i", "j"))

# out
liftover_conmat2favites(realized_contmat = realized_contacts,
                        outpath = paste0(DIR, "gears/favites_realized_contact_matrix",
                                         "_", opt$iter, ".tab.txt"))

