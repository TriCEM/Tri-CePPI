#' @title Liftover to FAVITES format
#' @param outpath charpath; writing out to your local drive
#' @details adding random names purely for human readability later, is
#' an unneccessary intermed step

liftover_conmat2favites <- function(realized_contmat, outpath){
  # drop tible class
  # TODO better check
  class(realized_contmat) <- "data.frame"

  # assert some class
  #TODO
  nodes <- unique(as.character(unlist(realized_contmat)))
  nodes <- nodes[order(nodes)]
  p1 <- data.frame(c1 = "NODE", c2 = nodes, c3 = ".", c4 = NA, c5 = NA)
  p2 <- data.frame(c1 = "EDGE", c2 = realized_contmat[,1], c3 = realized_contmat[,2],
                   c4 = ".", c5 = "u")
  # write out
  readr::write_tsv(x = rbind(p1,p2),
                   na = "",
                   col_names = F,
                   file = outpath)

  return(0)
}

