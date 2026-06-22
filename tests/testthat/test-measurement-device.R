test_that("measurement device opens and closes", {
  old_devices <- grDevices::dev.list()

  device_state <- patchworkLayoutOptimizer:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(patchworkLayoutOptimizer:::close_measurement_device(device_state), add = TRUE)

  expect_true(device_state$device %in% grDevices::dev.list())
  patchworkLayoutOptimizer:::close_measurement_device(device_state)

  current_devices <- grDevices::dev.list()
  expect_identical(is.null(old_devices), is.null(current_devices))
})
