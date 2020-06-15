context("Utility functions")
library(NetworkMetaAnalysis)

test_that("computeAngle gives correct angles", {
	expect_equivalent(computeAngle(c(0, 0.5, 1, 0.5, 0), c(1, 0.5, 0, -0.5, -1)), c(90, 45, 0, -45, -90))
}) 

test_that("Comparison id's are handled correctly", {
	expect_equivalent(sortComparisonIds(c("4-4", "6-3", "1-999")), c("4-4", "3-6", "1-999"))
	expect_equivalent(sortComparisonIds(c("4,4", "6,3", "1,999"), sep = ","), c("4,4", "3,6", "1,999"))
})