##' Print OMEGA and SIGMA matrices for NONMEM sections in block format.
##' Note: This function currently only works with fixed blocks as in 
##'       the NMsim_NWPRI functionality for printing $THETAPV.
##'
##' @param block_mat_string Output of NMsim::NMcreateMatLines. This is a string of
##' OMEGA/SIGMA estimates that will be wrapped onto multiple lines for ease of
##' reading in NONMEM control streams.
##' @return Character vector
##'
##' @keywords internal

prettyMatLines <- function(block_mat_string) {
  
  blocksize = as.numeric(gsub(pattern = "[[:alpha:]]|\\(", replacement = "", x = regmatches(block_mat_string, m = regexpr("BLOCK\\(\\d+", block_mat_string))))

  dollar_block_label = paste0(unlist(strsplit(block_mat_string, " FIX "))[1], " FIX")
  block_values = unlist(strsplit(unlist(strsplit(block_mat_string, " FIX "))[2], " "))
  row = list()
  i=1
  while(i<=blocksize) {
    idx.start = sum(sapply(X=1:i, FUN=function(x) length(1:x)))-i+1
    idx.end = idx.start+i-1
    row[[i]] = block_values[idx.start:idx.end]
    i=i+1
  }
  block.lines = c(dollar_block_label, unlist(lapply(row, function(.x) paste(.x, collapse = " "))))
  return(block.lines)
}
