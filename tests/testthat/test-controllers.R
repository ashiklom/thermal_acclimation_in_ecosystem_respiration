# The address the Slurm workers dial: never one only the controller's own node
# can reach.

test_that("a link-local interface listed first is passed over", {
  # c1104u05n02, 2026-10-01
  ips <- c(enp0s20f0u14u3 = "169.254.1.2", cluster = "10.18.23.7", ib0 = "10.184.1.76")
  expect_identical(controller_host(ips), "10.18.23.7")
})

test_that("without a cluster interface, the first routable address is used", {
  expect_identical(controller_host(c(lo = "127.0.0.1", eth0 = "10.0.0.5", ib0 = "10.1.0.5")), "10.0.0.5")
})

test_that("no routable address is an error, not a silent unreachable controller", {
  expect_error(controller_host(c(lo = "127.0.0.1", usb = "169.254.1.2")), "No routable")
})
