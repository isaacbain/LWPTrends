unpaste <- function (str, sep = "/", fixed = T) {
  w <- strsplit(str, sep, fixed = fixed)
  w <- matrix(unlist(w), ncol = length(str))
  nr <- nrow(w)
  ans <- vector("list", nr)
  for (j in 1:nr) ans[[j]] <- w[j, ]
  ans
}
