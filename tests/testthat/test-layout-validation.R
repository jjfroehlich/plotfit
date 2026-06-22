test_that("rectangular design validation accepts and rejects expected matrices", {
  valid_matrix <- rbind(
    c("1", "1", "2"),
    c("1", "1", "2"),
    c("3", "#", "#")
  )

  invalid_matrix <- rbind(
    c("1", "#", "1"),
    c("1", "2", "2")
  )

  expect_true(patchworkLayoutOptimizer:::validate_rectangular_design(valid_matrix))
  expect_false(patchworkLayoutOptimizer:::validate_rectangular_design(invalid_matrix))
})
