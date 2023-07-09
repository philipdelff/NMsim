writeTextFile <- function(lines,file){
    con <- file(file, "wb")
    writeLines(lines, con = con)
    close(con)
    NULL
}
