library(geojsonio)
countries <- geojson_read("/Users/Vanessa/Desktop/untitled folder/countries.geojson", what = "sp")
usethis::use_data(countries, overwrite = TRUE)
