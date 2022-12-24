
cache_mem2 <- function(max_size = 20*1024^2, ...) {
  cache <- cachem::cache_mem(max_size = max_size, ...)

  cache$compare <- function(..., .list = NULL) {
    items <- c(list(...), .list)
    if(!length(items)) { return(TRUE) }
    nms <- names(items)
    if(!length(nms) || "" %in% nms) {
      warning("cache_mem2: arguments for `compare` method must be all named.")
      return(FALSE)
    }
    if(length(nms[!nms %in% cache$keys()])) {
      return(FALSE)
    }
    re <- dipsaus::forelse(nms, function(nm) {
      if(identical(x = cache$get(nm), y = items[[nm]])) {
        return(NULL)
      } else {
        return(FALSE)
      }
    }, TRUE)
    return(re)
  }

  class(cache) <- c("cache_mem2", class(cache))
  cache
}

