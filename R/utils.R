initTLNise <- function() {
    b <- rnorm(10)
    v <- runif(10)
    g <- tlnise(b, v, rep(1, 10), seed = -1, N = 1, prnt = FALSE)
    invisible()
}
