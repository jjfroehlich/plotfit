test_that("text grobs are collected recursively", {
  device_state <- patchworkLayoutOptimizer:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(patchworkLayoutOptimizer:::close_measurement_device(device_state), add = TRUE)

  nested_grob <- grid::gTree(
    children = grid::gList(
      grid::textGrob("alpha"),
      grid::gTree(children = grid::gList(grid::textGrob("beta", rot = 45)))
    )
  )

  text_grobs <- patchworkLayoutOptimizer:::collect_text_grobs(
    nested_grob,
    component_type = "axis_b",
    component_name = "axis-b"
  )

  expect_equal(sort(text_grobs$text), c("alpha", "beta"))
  expect_true(all(text_grobs$width_mm > 0))
  expect_true(any(text_grobs$rotation == 45))
})
