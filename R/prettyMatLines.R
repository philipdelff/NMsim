##' @keywords internal

prettyMatLines <- function(block_mat_string) {
  # block_mat_string = "$THETAPV BLOCK(15) FIX 6.53838e-06 -1.3267e-05 0.0014287 -1.58875e-06 -5.45941e-05 6.66482e-05 4.1439e-06 -0.000237115 1.18724e-05 6.38698e-05 0.000111975 -2.48273e-05 -0.000479304 -0.000252843 0.0377089 -6.13071e-09 1.04019e-07 -1.44652e-08 -1.3657e-10 4.95032e-07 1.4308e-10 -2.57355e-08 -1.29522e-05 8.59179e-06 4.2652e-06 -0.000126909 -4.5838e-10 4.35979e-06 -1.94549e-07 -9.53104e-06 -1.65609e-07 2.79449e-06 -2.99101e-05 2.49789e-09 5.6143e-07 1.50904e-06 4.50035e-06 -0.000189684 5.90145e-06 4.517e-05 -0.000141681 1.07326e-08 3.27978e-06 2.2954e-06 4.48561e-05 1.42478e-08 -6.66919e-08 -3.40312e-09 5.58898e-08 1.00055e-08 1.29488e-11 3.5178e-09 2.07445e-09 4.68285e-08 2.29227e-10 -5.85635e-09 1.62079e-07 -1.81256e-08 -3.28916e-08 2.74424e-06 2.27903e-10 -5.39302e-08 1.31012e-08 -6.79149e-09 -1.81787e-11 2.28669e-09 -3.77133e-07 -7.50573e-07 4.1474e-07 8.50699e-08 -2.37329e-05 -5.51665e-11 4.11966e-08 1.63725e-08 -1.26127e-07 -1.2204e-09 -8.82729e-10 5.83974e-08 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 1e-30 5.20449e-08 -8.47737e-07 6.07393e-07 3.8424e-08 -4.1797e-05 -2.89234e-09 7.72446e-08 -1.20933e-07 -3.15856e-07 1.06161e-09 -6.73944e-09 4.90991e-08 1e-30 5.09426e-07 -6.65426e-07 4.98656e-05 -7.85838e-05 8.00667e-07 -0.000175329 2.67036e-08 -1.2028e-06 1.80628e-06 5.36498e-06 1.25578e-08 -1.60544e-07 1.43943e-07 1e-30 -1.95036e-07 0.000338403"
  
  blocksize = as.numeric(gsub(pattern = "[[:alpha:]]|\\(", replacement = "", x = regmatches(block_mat_string, m = regexpr("BLOCK\\(\\d+", block_mat_string))))

  dollar_block_label = paste0(unlist(strsplit(block_mat_string, " FIX "))[1], " FIXED")
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
