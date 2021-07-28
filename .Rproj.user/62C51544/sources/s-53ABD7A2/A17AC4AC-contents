create_coverage_badge <- function(coverage_perc, path = "coverage_badge.svg") {

  dplyr::case_when(
    coverage_perc < 65 ~ "Red",
    coverage_perc >= 65 & coverage_perc < 80 ~ "Yellow",
    coverage_perc >= 80 ~ "DarkGreen"
  ) -> color

  glue::glue("https://img.shields.io/badge/Code%20Coverage-{coverage_perc}%25-{color}") %>%
    xml2::read_html() %>%
    xml2::write_html(path)

}


coverage_percentage <- function(cobertura_path = "cobertura.xml") {

  readr::read_lines(cobertura_path) %>%
    tibble::enframe() %>%
    dplyr::slice(2) %>%
    dplyr::select(value) %>%
    tibble::deframe() %>%
    stringi::stri_extract(regex = '(?<=line-rate=\\")\\d{1}\\.\\d{1,20}') %>%
    {as.numeric(.) * 100} %>%
    round(2)

}
