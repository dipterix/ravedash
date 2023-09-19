key_missing <- function () {
  x <- list()
  class(x) <- "key_missing"
  x
}

is.key_missing <- function (x) {
  inherits(x, "key_missing")
}


get_pretty_digits <- function(x, allow_negative_round=FALSE) {
  max_x <- max(abs(x))

  dig = 0

  if(allow_negative_round && max_x > 100 ) {
    dig = -1
  } else if(max_x < 1) {
    dig = abs(floor(log10(max_x)))
  }

  dig
}

pretty_round <- function(x, allow_negative_round=FALSE) {
  round(x, get_pretty_digits(x, allow_negative_round = allow_negative_round))
}
