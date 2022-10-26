#' @title Liftover to FAVITES format
#' @param outpath charpath; writing out to your local drive
#' @details adding random names purely for human readability later, is
#' an unneccessary intermed step

liftover_conmat2favites <- function(realized_contmat, outpath){
  rnnames <- randomNames::randomNames(n = length(unique(unlist(realized_contmat))),
                                      which.names = "first",
                                      sample.with.replacement = F)
  rnnames <- stringr::str_replace_all(rnnames, "\\'|\\-| ", "")
  nodeval <- unique(unlist(realized_contmat))
  realized_contmat <- apply(realized_contmat, 2, as.character)
  for (i in 1:length(rnnames)) {
    realized_contmat[realized_contmat == nodeval[i]] <- rnnames[i]
  }

  # assert some class
  #TODO
  nodes <- sort(unique(as.vector(realized_contmat)))
  p1 <- data.frame(c1 = "NODE", c2 = nodes, c3 = ".", c4 = NA, c5 = NA)
  p2 <- data.frame(c1 = "EDGE", c2 = realized_contmat[,1], c3 = realized_contmat[,2],
                   c4 = ".", c5 = "u")
  # write out
  readr::write_tsv(x = rbind(p1,p2),
                   na = "",
                   col_names = F,
                   file = outpath)
  # return key for downstream
  out <- tibble::tibble(node = nodeval, names = rnnames)
  return(out)
}

