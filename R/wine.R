#' @title Retrieving the best wine in the World
#'
#' @description This function will allow you to retrieve from the wine review
#' data, the best wine
#' @param Data A \code{dataframe} This is the dataframe that is used to retrieve
#' the wines. It should be the data: winemag_data_130k_v2.
#' @param Country A \code{character} This is the name of the country/countries
#' of interest.
#' @param Price A \code{numeric vector} This is a vector containing the minimum
#' and the maximum values of the price.
#' @param Variety A \code{character} This is the name of the variety/varieties
#' of interest.
#' @param Province A \code{character} This is the name of the province/provinces
#' of interest.
#' @param N A \code{integer} This is the number of wines returned by the
#' function. This number should be a positive integer.
#' @param Criteria A \code{character} The variable used to rank the wines. It
#' can take the following values: "price", "ascending price", "quality",
#' "price & quality", "ascending price & quality" or "quality/price".
#' @return A \code{tibble} displaying the top N wines according to the specified
#' criterion.
#' @author Vanessa Armistead
#' @author Bart Roes
#' @author Luisa Pricken
#' @author Ameni Rouatbi
#' @importFrom dplyr select filter arrange rename mutate
#' @export
#' @examples
#' data = data("winemag")
#' get_wine(data, Country = "Italy")

get_wine<- function(Data, Country = NULL,Price= NULL, Variety= NULL,
                    Province = NULL, N=10, Criteria = NULL) {
  cleantable <- Data[,c("title","points", "price" ,"country",
                        "province", "variety", "winery")] %>%
    filter(is.na(points) == FALSE,
           is.na(price) == FALSE,
           is.na(title) == FALSE)  %>%
    rename('quality' = `points`)

  if(is.null(Price) == TRUE){
    cleantable <- cleantable
  } else {
    cleantable <- cleantable %>%
      filter(price >= min(Price), price <= max(Price))
  }

  if(is.null(Country) == TRUE) {
    cleantable <- cleantable
  } else if(sum(country %in% Country) == 0) {
    warning("There are no wine from this country")
  } else {
    cleantable <- cleantable %>%
      filter(country %in% Country)
  }

  if(is.null(Province) == TRUE) {
    cleantable <- cleantable
  } else if(sum(province %in% Province) == 0) {
    warning("There are no wine from this province")
  } else {
    cleantable <- cleantable %>%
      filter(province %in% Province)
  }

  if(is.null(Variety) == TRUE) {
    cleantable <- cleantable
  } else if(sum(variety %in% Variety) == 0) {
    warning("There are no wine from this variety")
  } else {
    cleantable <- cleantable %>%
      filter(variety %in% Variety)
  }

  if(is.null(Criteria) == TRUE) {
    cleantable <- cleantable
  } else if (Criteria== "price") {
    cleantable <- cleantable %>%
      arrange(-price)
  } else if (Criteria== "ascending price") {
    cleantable <- cleantable %>%
      arrange(price)
  } else if (Criteria == "quality") {
    cleantable <- cleantable %>%
      arrange(-quality)
  } else if (Criteria == "price & quality") {
    cleantable <- cleantable %>%
      arrange(-price, -quality)
  } else if (Criteria == "ascending price & quality") {
    cleantable <- cleantable %>%
      arrange(price, -quality)
  } else if (Criteria == "quality/price") {
    cleantable <- cleantable %>%
      mutate(ratio= quality/price) %>%
      arrange(-ratio)
  } else {
    stop("Criteria is incorrectly specified")
  }

  if (N<0){
    stop("N should be a positive number")
  } else if (N %% 1 != 0){
    stop("N should be an integer")
  } else if(N>=nrow(cleantable)){
    return(cleantable)
  } else {
    warning("There are more wines that the number specified")
    return(cleantable[1:N,])
    }

}

