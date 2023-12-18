#' Pseudo-log transformation
#' 
#' Derived from the R package scales by Hadley Wickham
#' https://github.com/r-lib/scales
#' See scales::pseudo_log_trans()
#' 
#' @param base Base of logarithm
#' @param sigma Scaling factor
#' 
#' @return A scale
#' 
#' @importFrom scales trans_new
#' 
#' @export
pseudo.log.trans <- function(sigma=1, base=exp(1)) {
    scales::trans_new(
        "pseudo_log",
        function(x) asinh(x / (2 * sigma)) / log(base),
        function(x) 2 * sigma * sinh(x * log(base))
    )
}

#' Pseudo-log transformation in reverse
#' 
#' Derived from the R package scales by Hadley Wickham
#' https://github.com/r-lib/scales
#' See scales::pseudo_log_trans()
#' 
#' @param base Base of logarithm
#' @param sigma Scaling factor
#' 
#' @return A scale
#' 
#' @importFrom scales trans_new
#' 
#' @export
pseudo.log.trans.rev <- function(sigma=1, base=exp(1)) {
    scales::trans_new(
        "rev_pseudo_log",
        function(x) - asinh(x / (2 * sigma)) / log(base),
        function(x) - 2 * sigma * sinh(x * log(base))
    )
}

#' Normalize values between a given range
#' 
#' @param x Values to normalize
#' @param a Min of range
#' @param b max of range
#' 
#' @return Normalized values
#' 
#' @export
normalize.range <- function(x, a=0, b=1) {
    (b-a)*( (x-min(x)) / (max(x)-min(x)) )+a
}

#' Normalize values between zero and one
#' 
#' @param x Values to normalize
#' 
#' @return Normalized values
#' 
#' @export
normalize.zo <- function(x) {
    normalize.range(x, a=0, b=1)
}

#' Normalize significance values
#' 
#' @param x Values to normalize
#' @param base Base of logarithm
#' 
#' @return Normalized values
#' 
#' @return Normalized values
#' 
#' @export
normalize.nlog <- function(x, base=10) {
    -log(x, base=base)
}

#' Normalize significance values between a given range
#' 
#' @param x Values to normalize
#' @param base Base of logarithm
#' @param a Min of range
#' @param b max of range
#' 
#' @return Normalized values
#' 
#' @export
normalize.nlog.range <- function(x, base=10, a=0, b=1) {
    normalize.range(normalize.nlog(x, base=base), a=a, b=b)
}

#' Colorize numerical values
#' 
#' @param x Values to normalize
#' @param resolution Limit resolution for small values
#' @param pal Color palette (?viridis)
#' 
#' @return Colorized values
#' 
#' @import viridis
#' 
#' @export
colorize <- function(x, resolution=4, pal=viridis::plasma) {
    multiplier <- 100*resolution
    colors <- pal(multiplier+1)
    colors[round(normalize.zo(x)*multiplier, 0)+1]
}

#' Colorize numerical values for the heat palette
#' 
#' @param x Values to normalize
#' @param resolution Limit resolution for small values
#' 
#' @return Colorized values
#' 
#' @export
colorize.heat <- function(values, resolution=4) {
    multiplier <- 100*resolution
    colors <- rev(heat.colors(multiplier+1))
    colors[round(normalize.zo(values)*multiplier, 0)+1]
}

#' Color ramp functions
#' 
#' @param x Vector of colors
#' 
#' @return A color ramp function
#' 
#' @importFrom grDevices colorRampPalette
#' 
#' @export
cfx <- function(x) {
  function(y) {colorRampPalette(c(x))(y)}
}

#' Repeatable vector of distinct colors
#' 
#' @param reps Number of copies
#' 
#' @return A vector of distinct values
#' 
#' @export
rcolors <- function(reps=1) {
    set.seed(1)
    rep(sample(colors())(), reps)
}
