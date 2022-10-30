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

#............................................................
# read in contact networks & calc "prob"
#...........................................................
fullnet <- readr::read_tsv("gears/full_contact_matrix.tab.txt")

#......................
# parse out
#......................
correctmatrix <- fullnet %>%
  dplyr::select(c("i", "j", "probsucc")) %>%
  dplyr::rename(probsucc_correct = probsucc)

tdistmat <- fullnet %>%
  dplyr::rename(probsucc_tdist = tdistinv) %>%
  dplyr::select(c("i", "j", "probsucc_tdist"))

gdistmat <- fullnet %>%
  dplyr::mutate(probsucc_gdist = gdistinv) %>%
  dplyr::select(c("i", "j", "probsucc_gdist"))


# connected
fulldistmat <- correctmatrix %>%
  dplyr::left_join(., tdistmat) %>%
  dplyr::left_join(., gdistmat)


#............................................................
# read in transmission results
#...........................................................
retdirs <- list.dirs(path = "results/")
retdirs <- retdirs[grepl("error_free_files$", retdirs)]
retfiles <- paste0(retdirs, "/transmission_network.txt.gz")
transmission_records <- lapply(retfiles, readr::read_tsv, col_names = F) %>%
  dplyr::bind_rows()

#......................
# bring together
#......................
transmission_counts <- transmission_records %>%
  magrittr::set_colnames(c("i", "j", "tte")) %>%
  dplyr::mutate(uconnect = purrr::map2_chr(i, j,
                                           function(x,y){paste(sort(c(x,y)), collapse = "|")})) %>%
  dplyr::group_by(uconnect) %>%
  dplyr::summarise(
    trans_cnt = dplyr::n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(i = stringr::str_split_fixed(uconnect, "\\|", n=2)[,1],
                j = stringr::str_split_fixed(uconnect, "\\|", n=2)[,2]) %>%
  dplyr::select(c("i", "j", "trans_cnt"))

# combine
facetlbls <- ggplot2::as_labeller(c(`probsucc_correct` = "Correct Net",
                                    `probsucc_tdist` = "Transport Net",
                                    `probsucc_gdist` = "Geograph Net"))
plotObj <- dplyr::left_join(fulldistmat, transmission_counts) %>%
  dplyr::select(c("i", "j", "trans_cnt", dplyr::everything())) %>%
  tidyr::pivot_longer(., cols = starts_with("probsucc_"),
                      names_to = "probtype", values_to = "probsucc") %>%
  dplyr::mutate(trans_cnt = factor(trans_cnt, levels = 1:100)) %>%
  dplyr::filter(!is.na(trans_cnt)) %>%
  ggplot() +
  geom_boxplot(aes(x = trans_cnt, y = probsucc),
               outlier.colour = "red", outlier.shape = 8,
               outlier.size = 1) +
  facet_grid(~probtype, labeller = facetlbls) +
  xlab("Transmission Counts") + ylab("Predictive Prob.") +
  theme_linedraw() +
  theme(axis.title = element_text(family = "Arial", face = "bold", hjust = 0.5, size = 12),
        axis.text.x = element_text(family = "Arial", hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(family = "Arial", hjust = 0.5, size = 11))


# plot
plotObj
