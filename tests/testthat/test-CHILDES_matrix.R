testthat::test_that("a matrix of co-occurring words is formed", {
  x <- c("the", "ball", "is", "on", "the", "dog")
  n <- length(x)
  tid <- c(1,1,1,1,1,1)
  uid <- c(1,1,1,1,1,1)
  tok <- 1:n
  k <- 3
  y <- c("the", "ball", "is", "on", "dog")

  tmp <- matrix(c(0,1,1,0,1,
                  0,0,1,1,0,
                  1,0,0,1,0,
                  1,0,0,0,1,
                  0,0,0,0,0), nrow = 5, ncol = 5,
                dimnames = list(y, y), byrow = T)


  y <- c("ball", "dog", "is", "on", "the")
   testthat::expect_equivalent(CHILDES_matrix(k, x, transcript_id = tid, utterance_id = uid, token_order = tok), tmp[y,y])
   })


testthat::test_that("matrix doesn't span chunks, one chunk, by_utterance = T, k = 2", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 2
  tid <- c(1,1,1,1,1,1)
  uid <- c(1,1,1,1,1,1)
  tok <- 1:length(x)
  check <- matrix(c(0,1,0,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,1,0,0,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = T), check)
})

testthat::test_that("matrix doesn't span chunks, two chunks, by_utterance =  T, k = 2", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 2
  tid <- c(1,1,1,1,1,1)
  uid <- c(1,1,1,2,2,2)
  tok <- 1:length(x)
  check <- matrix(c(0,1,0,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,0,0,0,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = T), check)
})


testthat::test_that("matrix doesn't span chunks, one chunk, by_utterance =  F, k = 2", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 2
  tid <- c(1,1,1,1,1,1)
  uid <- c(1,1,1,1,1,1)
  tok <- 1:length(x)
  check <- matrix(c(0,1,0,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,1,0,0,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = F), check)
})

testthat::test_that("matrix doesn't span chunks, two chunks,  by_utterance =  F, k = 2", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 2
  tid <- c(1,1,1,2,2,2)
  uid <- c(1,1,1,1,1,1)
  tok <- 1:length(x)
  check <- matrix(c(0,1,0,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,0,0,0,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = F), check)
})

testthat::test_that("matrix doesn't span chunks, two chunks,  by_utterance =  F, k = 3", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 3
  tid <- c(1,1,1,2,2,2)
  uid <- c(1,1,1,1,1,1)
  tok <- 1:length(x)
  check <- matrix(c(0,1,1,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,0,0,0,
                    0,0,0,0,1,1,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = F), check)
})

testthat::test_that("matrix doesn't span chunks, two chunks, by_utterance =  T, k = 3", {
  x <- c("A", "B", "C", "D", "E", "F")
  k <- 3
  tid <- c(1,1,1,1,1,1)
  uid <- c(1,1,1,2,2,2)
  tok <- 1:length(x)
  check <- matrix(c(0,1,1,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,0,0,0,
                    0,0,0,0,1,1,
                    0,0,0,0,0,1,
                    0,0,0,0,0,0), nrow = 6, ncol = 6, byrow = T)
  testthat::expect_equivalent(CHILDES_matrix(k,x, transcript_id = tid, utterance_id = uid, token_order = tok, by_utterance = T), check)
})


