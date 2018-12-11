#Setup#####
# library("robotstxt")
#
# paths_allowed(
#   path = "/?s=&search_type=reviews&drink_type=wine&price=1-3000",
#   domain = "https://www.winemag.com/"
# )



# get_pages
get_pages <- function(country){

  stopifnot(exprs = {
    is.character(country)
    length(country) == 1
    all(!is.na(country))
  })

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

# get_prices

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

# get_all_prices

get_all_prices <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_prices(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.numeric()
}

# get_quality

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

# get_all_qualities

get_all_qualities <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_quality(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.numeric()
}

# get_titles

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

  url_path_page <- paste0(
    "https://www.winemag.com/?s=&drink_type=wine&country=",
    country,"&price=1-3000&page=",
    page
  )

  xml2::read_html(url_path_page) %>%
    rvest::html_nodes(".review-listing .title") %>%
    rvest::html_text()

}

# get_all_qualities

get_all_titles <- function (country, delay = 1){
  stopifnot(exprs = {
    is.numeric(delay)
    length(delay) == 1
    all(!is.na(delay))
    delay >= 0
  })

  sapply(X = get_pages(country), FUN = function(page) {
    Sys.sleep(time = delay)
    get_title(country = country, page = page, validate = FALSE)
  }) %>% unlist() %>% as.character()
}


# compile_data

# compile_country_data

compile_data <- function(country){

  data.frame(get_all_titles(country)) %>%
    dplyr::mutate(quality = get_all_qualities(country),
                  price = get_all_prices(country),
                  country = rep(country, length(price)))

}

# compile_full_data

compile_full_data <- function(all.countries){

  dplyr::bind_rows(lapply(X = all.countries, FUN = compile_data))

}



