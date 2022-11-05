## .................................................................................
## Purpose:
##
## Author: Nick Brazeau
##
## Date: 04 November, 2022
##
## Notes:
## .................................................................................
library(tidyverse)

#............................................................
# read in idealized infection prob
#...........................................................
probInfxns <- readRDS("results/probInfxns.RDS")

#............................................................
# read in transmission results
#...........................................................
retdirs <- list.dirs(path = "results/")
retdirs <- retdirs[grepl("error_free_files$", retdirs)]
retfiles <- paste0(retdirs, "/transmission_network.txt.gz")
transmission_records <- lapply(retfiles, readr::read_tsv, col_names = F) %>%
  dplyr::bind_rows(.id = "iter") %>%
  magrittr::set_colnames(c("iter", "from", "to", "tti"))


#............................................................
# bring together
#...........................................................
# downsample
rn <- sample(unique(probInfxns$node), 6)
# liftover transmission record
plotTransrecord <- transmission_records %>%
  dplyr::filter(tti != 0) %>%
  dplyr::rename(node = to,
                time = tti) %>%
  dplyr::select(c("iter", "node", "time")) %>%
  dplyr::filter(node %in% rn)


probInfxns %>%
  dplyr::mutate(time = stringr::str_replace_all(time, "t", ""),
                time = as.integer(time)) %>%
  dplyr::filter(node %in% rn) %>%
  ggplot() +
  geom_rect(data = plotTransrecord,
            aes(xmin = time-0.5, xmax = time+0.5, ymin = 0, ymax = Inf)) +
  geom_area(aes(y = probInfxn, x = time,
                group = node, fill = node), alpha = 0.25) +
  theme_linedraw() +
  xlab("Time") + ylab("Prob. of Infxn") +
  facet_wrap(~node) +
  theme(
    axis.text.x = element_text(angle = 45),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  )



