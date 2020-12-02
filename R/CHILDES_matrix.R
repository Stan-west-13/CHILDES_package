
CHILDES_matrix <- function(k, x, transcript_id = NULL, utterance_id = NULL, token_order = NULL, by_utterance = FALSE){
  ix <- order(transcript_id, utterance_id, token_order)
  x <- x[ix]
  tid <- transcript_id[ix]
  uid <- utterance_id[ix]
  xfct <- as.factor(x)
  n <- nlevels(xfct)
  M <- matrix(0, nrow = n, ncol = n,
              dimnames = list(levels(xfct), levels(xfct)))
  ix <- as.numeric(xfct)

  cid <- if (by_utterance) uid else tid
  i <- 1
  N <-length(x)
  while (i < N) {
  ## Storing chunk label into ci and current reference token into wi
    ci <- cid[i]
    wi <- ix[i]
  ## Defining window for co-occurring words.  cid[i] = cue, cid[a:b] = associates
    a <- i + 1
    b <- i + (k - 1)
    if (b > N) b <- N
    i <- a
    wj <- ix[a:b]
    zj <- cid[a:b] == ci
    if (any(zj)) {
      M[wi, wj[zj]] <- M[wi, wj[zj]] + 1
    }
  }
  return(M)
}



CHILDES_matrix_test <- function() {
  xstring <- "the ball is on the dog why is the ball on the dog"
  x <- strsplit(xstring, split = ' ')[[1]]
  n <- length(x)
  tid <- rep(1, 6)
  uid <- rep(1, 6)
  tok <- 1:6
  k <- 3
  tmp <- CHILDES_matrix(k = 3, x, transcript_id = tid, utterance_id = uid, token_order = tok)
  y <- c("the", "ball", "is", "on", "dog")
  return(tmp[y, y])
}
CHILDES_matrix_test()







