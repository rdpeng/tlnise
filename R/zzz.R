.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    msg <- paste("Two level Normal independent sampling estimation (version ",
                 as.character(ver), ")", sep = "")
    writeLines(strwrap(msg))
}
