test_that("percentage formatter works", {
  expect_equal(format_percent(5, decimal_places = 2), "5.00%")
  expect_equal(format_percent(2.5, decimal_places = 1), "2.5%")
  expect_equal(format_percent(2.5, decimal_places = 3), "2.500%")
  expect_equal(format_percent(2.28, decimal_places = 1), "2.3%")
  expect_equal(format_percent(2.4, decimal_places = 0), "2%")
  expect_equal(format_percent(2.6, decimal_places = 0), "3%")
  expect_equal(format_percent(2.5, decimal_places = 0), "2%")
})
