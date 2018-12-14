#' @title Get Pages
#' @description This function retrieves the total number of pages from the
#' WineEnthusiast webpage (www.winemag.com) for the specified country
#' @param country \code{character}, the name of the country
#' @return A \code{vector} containing a sequence ranging from 1 to the total
#' number of pages
#' @export
#' @example
#' get_pages("Canada")

get_pages <- function(country){

  stopifnot(exprs = {
    is.character(country)
    length(country) == 1
    all(!is.na(country))
  })

  country <- gsub(country, pattern = " ", replacement = "%20")


  url_path <- paste0(
    "https://www.winemag.com/?s=&drink_type=wine&country=",
    country,"&price=1-3000&page=1"
  )

  xml2::read_html(url_path) %>%
    rvest::html_nodes(".pagination a") %>%
    rvest::html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE) %>%
    magrittr::subtract(e2 = 1) %>%
    seq(from = 1)
}

#' @title Get Prices
#' @description This function retrieves a list of prices for a specified
#' country for a single webpage,
#' corresponding to the wines from the WineEnthusiast website
#' @param country \code{character}, the name of the country
#' @param page \code{integer}, the webpage from which to retrieve the prices
#' @return A \code{numeric vector} containing the prices from the wines on the
#' specified webpage
#' @export
#' @example
#' get_prices("Canada",1)

get_prices <- function(country, page, validate = TRUE){

  stopifnot(exprs = {
    is.logical(validate)
    length(validate) == 1
    all(!is.na(validate))
  })
  if (validate) {
    stopifnot(exprs = {
      is.character(country)
      length(country) == 1
      all(!is.na(country))
      is.numeric(page)
      length(page) == 1
      all(!is.na(page))
      page %in% get_pages(country)
    })
  }

  country <- gsub(country, pattern = " ", replacement = "%20")

  url_path_page <- paste0(
    "https://www.winemag.com/?s=&drink_type=wine&country=",
    country,"&price=1-3000&page=",
    page
  )

  xml2::read_html(url_path_page) %>%
    rvest::html_nodes(".price") %>%
    rvest::html_text() %>%
    substring(first = 2) %>%
    as.numeric()

}

#' @title Get All Prices
#' @description This function retrieves a list of prices for a specified
#' country, corresponding to the wines from the WineEnthusiast webpage
#' @param country \code{character}, the name of the country
#' @return A \code{numeric vector} containing the prices from all wines from
#' the specified country
#' @export
#' @example
#' get_all_prices("Canada")

get_all_prices <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  country <- gsub(country, pattern = " ", replacement = "%20")

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_prices(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.numeric()
}

#' @title Get Quality
#' @description This function retrieves a list of qualities for a specified
#' country for a single webpage,
#' corresponding to the wines from the WineEnthusiast website
#' @param country \code{character}, the name of the country
#' @param page \code{integer}, the webpage from which to retrieve the qualities
#' @return A \code{numeric vector} containing the qualities from the wines on the
#' specified webpage
#' @export
#' @example
#' get_quality("Canada",1)

get_quality <- function(country, page, validate = TRUE){

  stopifnot(exprs = {
    is.logical(validate)
    length(validate) == 1
    all(!is.na(validate))
  })
  if (validate) {
    stopifnot(exprs = {
      is.character(country)
      length(country) == 1
      all(!is.na(country))
      is.numeric(page)
      length(page) == 1
      all(!is.na(page))
      page %in% get_pages(country)
    })
  }

  country <- gsub(country, pattern = " ", replacement = "%20")

  url_path_page <- paste0(
    "https://www.winemag.com/?s=&drink_type=wine&country=",
    country,"&price=1-3000&page=",
    page
  )

  quality <- xml2::read_html(url_path_page) %>%
    rvest::html_nodes(".rating") %>%
    rvest::html_text() %>%
    substring(first = 1, last = 2) %>%
    as.numeric()

  quality[quality == 10] <- 100

  return(quality)

}

#' @title Get All Qualities
#' @description This function retrieves a list of qualities for a specified
#' country, corresponding to the wines from the WineEnthusiast webpage
#' @param country \code{character}, the name of the country
#' @return A \code{numeric vector} containing the qualities from all wines from
#' the specified country
#' @export
#' @example
#' get_all_qualities("Canada")

get_all_qualities <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  country <- gsub(country, pattern = " ", replacement = "%20")

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_quality(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.numeric()
}

#' @title Get Title
#' @description This function retrieves a list of titles for a specified
#' country for a single webpage,
#' corresponding to the wines from the WineEnthusiast website
#' @param country \code{character}, the name of the country
#' @param page \code{integer}, the webpage from which to retrieve the titles
#' @return A \code{character} containing the titles from the wines on the
#' specified webpage
#' @export
#' @example
#' get_title("Canada",1)

get_title <- function(country, page, validate = TRUE){

  stopifnot(exprs = {
    is.logical(validate)
    length(validate) == 1
    all(!is.na(validate))
  })
  if (validate) {
    stopifnot(exprs = {
      is.character(country)
      length(country) == 1
      all(!is.na(country))
      is.numeric(page)
      length(page) == 1
      all(!is.na(page))
      page %in% get_pages(country)
    })
  }

  country <- gsub(country, pattern = " ", replacement = "%20")

  url_path_page <- paste0(
    "https://www.winemag.com/?s=&drink_type=wine&country=",
    country,"&price=1-3000&page=",
    page
  )

  xml2::read_html(url_path_page) %>%
    rvest::html_nodes(".review-listing .title") %>%
    rvest::html_text()

}

#' @title Get All Titles
#' @description This function retrieves a list of titles for a specified
#' country, corresponding to the wines from the WineEnthusiast webpage
#' @param country \code{character}, the name of the country
#' @return A \code{character} containing the qualities from all wines from
#' the specified country
#' @export
#' @example
#' get_all_titles("Canada")

get_all_titles <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  country <- gsub(country, pattern = " ", replacement = "%20")

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_title(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.character()
}


#' @title Compile Wine Data
#' @description This function creates a dataframe, containing the qualities,
#' prices, titles and countries of the wines retrieved from Winemag.com,
#' for a given country
#' @param country \code{character}, the name of the country
#' @return A \code{dataframe} containing the following variables:
#' \describe{
#'      \item{title}{Title of the wine}
#'      \item{quality}{Quality of the wine}
#'      \item{price}{Price of the wine}
#'      \item{country}{Country of origin}
#' }
#' @export
#' @example
#' compile_data("Canada")


compile_data <- function(country){

  country <- gsub(country, pattern = " ", replacement = "%20")

  data.frame(get_all_titles(country)) %>%
    dplyr::mutate(quality = get_all_qualities(country),
                  price = get_all_prices(country),
                  country = rep(country, length(price)))

}

#' @title Compile Complete Wine Data
#' @description This function creates a dataframe, containing the qualities,
#' prices, titles and countries of the wines retrieved from Winemag.com,
#' for multiple countries
#' @param country \code{character}, the name of the country
#' @return A \code{dataframe} containing the following variables:
#' \describe{
#'      \item{title}{Title of the wine}
#'      \item{quality}{Quality of the wine}
#'      \item{price}{Price of the wine}
#'      \item{country}{Country of origin}
#' }
#' @export
#' @example
#' compile_data(c("Canada", "Israel"))

compile_full_data <- function(all.countries){

  all.countries <- gsub(all.countries, pattern = " ", replacement = "%20")

  dplyr::bind_rows(lapply(X = all.countries, FUN = compile_data))

}

#' @title Compile First Page of Wine Data
#' @description This function creates a dataframe, containing the qualities,
#' prices, titles and countries of the wines retrieved from the first page of the
#' website - retrieved from Winemag.com - for a given country
#' @param country \code{character}, the name of the country
#' @return A \code{dataframe} containing the following variables:
#' \describe{
#'      \item{title}{Title of the wine}
#'      \item{quality}{Quality of the wine}
#'      \item{price}{Price of the wine}
#'      \item{country}{Country of origin}
#' }
#' @export
#' @example
#' compile_first_page("Canada")

compile_first_page <- function(country){

  country <- gsub(country, pattern = " ", replacement = "%20")

  data.frame(title =get_title(country, page = 1)) %>%
    dplyr::mutate(quality = get_quality(country, page = 1),
                  price = get_prices(country, page = 1),
                  country = rep(country, length(price)))

}


#' @title Compile Partial Wine Data
#' @description This function creates a dataframe, containing the qualities,
#' prices, titles and countries of the wines retrieved from the first page of the
#' website - retrieved from Winemag.com - for multiple countries
#' @param country \code{character}, the name of the country
#' @return A \code{dataframe} containing the following variables:
#' \describe{
#'      \item{title}{Title of the wine}
#'      \item{quality}{Quality of the wine}
#'      \item{price}{Price of the wine}
#'      \item{country}{Country of origin}
#' }
#' @export
#' @example
#' compile_partial_data(c("Canada", "Israel"))

compile_partial_data <- function(all.countries){

  all.countries <- gsub(all.countries, pattern = " ", replacement = "%20")

  dplyr::bind_rows(lapply(X = all.countries, FUN = compile_first_page))

}



