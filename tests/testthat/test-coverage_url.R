test_that("Test if coverage_url returns a correct URL", {
  coverage_percentage(testthat::test_path("test_xml", "badge_tst.xml"), delete_xml = FALSE) -> covr_p

  testthat::expect_equal(
    .coverage_url(coverage_perc = covr_p),
    "https://img.shields.io/badge/Code%20Coverage-19.35%25-red"
  )
})
