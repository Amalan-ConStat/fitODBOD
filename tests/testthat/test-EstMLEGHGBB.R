estimate<-EstMLEGHGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.1,0.5,0.4)

context("Checking outputs")
test_that("estimate method",{
          expect_identical(estimate@method,
                           "BFGS")
                           })

test_that("estimate method",{
          expect_identical(estimate@optimizer,
                           "optim")
                          })

#test_that("minimized negative ll value",{
#          expect_identical(round(estimate@min,4),
#                          436.8137)
#                          })

test_that("Checking class of output",{
          expect_that(estimate,
          is_a("mle2"))
          })
