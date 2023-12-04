uenc <- function(x) URLencode(x, reserved=TRUE)

chomp_url <- function(url) sub("/*$", "", url)
