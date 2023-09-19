
plotting_to_file <- function() {
  i <- get0(
    ".Devices",
    envir = baseenv(),
    ifnotfound = {
      return(FALSE)
    },
    inherits = FALSE
  )

  if(length(i) < grDevices::dev.cur()){
    return (FALSE)
  }

  i = i[[grDevices::dev.cur()]]

  return (
    isTRUE('dipsaus_dev_name' %in% names(attributes(i)) || i %in% c('pdf', 'png', 'jpeg'))
  )

}

clip_x <- function (x, lim) {
  if (length(lim) == 1) {
    lim = c(lim, -lim)
  }
  x[x < min(lim)] <- min(lim)
  x[x > max(lim)] <- max(lim)
  x
}

round_to_nearest <- function(x, val = 10) {
  val * round(x / val)
}

..get_nearest_i <- function(from,to, lower_only=FALSE) {

  if(lower_only) {
    res <- sapply(from, function(.x) {
      to2 = to[to<= .x]
      which.min(abs(.x-to2))
    })
  } else {
    res <- sapply(from, function(.x) {
      which.min(abs(.x-to))
    })
  }

  return(res)
}

..get_nearest <- function(from, to) {
  stats::approxfun(to, seq_along(to))(from)
}

..get_nearest_val <- function(from,to) {
  to[..get_nearest_i(from,to)]
}

`%near%` <- function(x, y, eps=1e-4) {
  abs(x-y) < eps
}


`%within%` <- function (a, b) {
  (a >= min(b)) & (a <= max(b))
}

