test_that("multiplication works", {
  coverage_percentage("./tests/test_xml/badge_tst.xml")

  .coverage_url(coverage_percentage("./tests/test_xml/badge_tst.xml"))
})
