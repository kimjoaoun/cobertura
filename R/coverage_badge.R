

generate_cobertura <- function() {
  covr::to_cobertura(
    covr::package_coverage()
    )
}

coverage_percentage <- function(cobertura_path = "cobertura.xml", delete_xml = FALSE) {

  readr::read_lines(cobertura_path) %>%
    tibble::enframe() %>%
    dplyr::slice(2) %>%
    dplyr::select(value) %>%
    tibble::deframe() %>%
    stringi::stri_extract(regex = '(?<=line-rate=\\")\\d{1}\\.\\d{1,20}') %>%
    {as.numeric(.) * 100} %>%
    round(2) -> percentage

  if(delete_xml) {
    fs::file_delete(cobertura_path)
  }

  return(percentage)
}

.coverage_url <- function(coverage_perc) {
  dplyr::case_when(
    coverage_perc < 65 ~ "red",
    coverage_perc >= 65 & coverage_perc < 80 ~ "yellow",
    coverage_perc >= 80 ~ "brightgreen"
  ) -> color

  glue::glue("https://img.shields.io/badge/Code%20Coverage-{coverage_perc}%25-{color}") %>%
    as.character()
}

create_coverage_badge <- function(coverage_perc, path = "coverage_badge.svg") {

  url <- .coverage_url(coverage_perc)

  url %>%
    xml2::read_html() %>%
    xml2::write_html(path)

  invisible(url)
}

