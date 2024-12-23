#' Get the agricultural community boundary data
#'
#' @description
#' `get_boundary()` downloads and reads one or more agricultural community
#' boundary data provided by the MAFF.
#' @param data
#'   List of [sf::sf()] objects or one or more strings representing prefecture
#'   codes.
#' @param year
#'   The year when the agricultural community boundary data was created.
#' @param census_year
#'   The year of the Agricultural and Forestry Census.
#' @param path
#'   Path to the ZIP file containing the agricultural community boundary data;
#'   use a local ZIP file instead of going looking for a ZIP file. Specify a
#'   directory containing one or more ZIP files, not the ZIP file itself.
#' @param to_wgs84
#'   logical. Convert JGD2000 to WGS 84.
#' @param quiet
#'   logical. Suppress information about the data to be read.
#' @returns A list of [sf::sf()] objects.
#'
#' @examplesIf interactive()
#' path <- system.file("extdata", "castle.zip", package = "fude")
#' d <- read_fude(path)
#' b <- get_boundary(d)
#'
#' @export
get_boundary <- function(data,
                         year = 2020,
                         census_year = 2020,
                         path = NULL,
                         to_wgs84 = TRUE,
                         quiet = FALSE) {

  pref_codes <- fude_to_pref_code(data)

  x <- lapply(pref_codes, function(i) {
    pref_code <- get_pref_code(i)
    read_boundary(pref_code, year, census_year, quiet, path, to_wgs84)
  })

  names(x) <- sprintf("MA0001_%s_%s_%s", year, census_year, sapply(pref_codes, get_pref_code))

  return(x)
}

read_boundary <- function(pref_code, year, census_year, quiet, path, to_wgs84) {
  url <- sprintf("https://www.machimura.maff.go.jp/shurakudata/%s/ma/MA0001_%s_%s_%s.zip",
                 census_year, year, census_year, pref_code)

  zipfile <- tempfile(fileext = ".zip")
  homepath <- file.path(getwd(), basename(url))

  if (is.null(path)) {
    if (!file.exists(homepath)) {
      utils::download.file(url, homepath)
    }
    file.copy(homepath, zipfile)
  } else {
    file.copy(file.path(path, basename(url)), zipfile)
  }

  exdir <- tempdir()
  utils::unzip(zipfile, exdir = exdir)

  shp_files <- list.files(exdir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

  on.exit({
    unlink(zipfile)
    unlink(shp_files)
  })

  if (length(shp_files) != 1) {
    stop(ifelse(length(shp_files) > 1, "Multiple shapefiles found.", "No shapefile found in the ZIP archive."))
  }

  x <- sf::st_read(shp_files, quiet = quiet, options = "ENCODING=CP932") |>
    dplyr::left_join(fude::community_code_table |>
                       dplyr::select(.data$KEY, .data$PREF_KANA, .data$PREF_ROMAJI, .data$CITY_KANA, .data$CITY_ROMAJI, .data$RCOM_ROMAJI),
                     by = c("KEY" = "KEY")) |>
    dplyr::mutate(
      boundary_edit_year = year,
      boundary_census_year = census_year
    )

  # Convert JGD2000 to WGS 84
  if (sf::st_crs(x)$epsg != 4326 && isTRUE(to_wgs84)) {
    x <- sf::st_transform(x, crs = 4326)
  }

  return(x)
}

fude_to_lg_code <- function(data) {

  if (is.character(data)) {

    x <- data

  } else if (is.data.frame(data)) {

    x <- unique(data$local_government_cd)

  } else if (is.list(data)) {

    x <- unique(unlist(sapply(data, `[[`, "local_government_cd")))

  }

  return(x)

}

fude_to_pref_code <- function(data) {
  local_government_cd <- fude_to_lg_code(data)

  x <- unique(substr(local_government_cd, start = 1, stop = 2))

  return(x)
}

get_pref_code <- function(input) {
  if (input %in% fude::pref_code_table$pref_code) {
    return(input)
  } else if (any(fude::pref_code_table$pref_kanji == input)) {
    return(fude::pref_code_table$pref_code[fude::pref_code_table$pref_kanji == input])
  } else if (any(grepl(input, fude::pref_code_table$pref_kanji))) {
    return(fude::pref_code_table$pref_code[grepl(input, fude::pref_code_table$pref_kanji)][1])
  } else {
    stop("Invalid input. Please enter a valid prefecture name or code.")
  }
}
