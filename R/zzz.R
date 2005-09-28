.onAttach <- function(lib, pkg) {
    desc <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- paste(desc[, "Title"], " (version ",
                 as.character(desc[, "Version"]), ")\n", sep = "")
    writeLines(strwrap(msg))
}
