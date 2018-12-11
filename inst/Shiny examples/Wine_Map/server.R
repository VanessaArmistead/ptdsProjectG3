library("shiny")
library("leaflet")
library("data.table")
library("purrr")
library("geonames")
library("tidyverse")
library("ptdsProjectG3")
library("kableExtra")


# Define server logic
server <- function(input, output, session) {

  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    countries@data$quality$country, signif(countries@data$quality$avg.quality, digits = 3)) %>%
    lapply(htmltools::HTML)

  wine.label <- sprintf(
    "<strong>%s</strong><br/>%s",
    countries@data$quality$country, countries@data$quality$V2) %>%
    lapply(htmltools::HTML)

  number.label <- sprintf(
    "<strong>%s</strong><br/>%g",
    countries@data$quality$country, countries@data$quality$n) %>%
    lapply(htmltools::HTML)

  grouped.labels <- list(labels, wine.label, number.label)

  unique.countries <- unique(ptdsProjectG3::winemag$country)
  unique.varieties <- unique(ptdsProjectG3::winemag$variety)

  # Creating colours for the graph
  # Based on avg quality
  bins <- c()
  for (i in 1:5) {
    bins[i] <- quantile(countries@data$quality$avg.quality, na.rm = T, probs = 0.2*i)
  }
  bins.avg <- c(80,round(bins,1),93)
  pal.avg <- colorBin("YlOrRd", domain = countries$df,
                      bins = bins.avg)
  # Based on number of wines
  bins <- c()
  for (i in 1:5) {
    bins[i] <- quantile(countries@data$quality$n, na.rm = T, probs = 0.2*i)
  }
  bins.n <- c(0, bins, 60000)
  pal.n <- colorBin("YlOrRd", domain = countries$df,
                    bins = bins.n)

  pal.fun <- list(pal.avg,
                  pal.avg,
                  pal.n)
  pal <- list(pal.avg(countries@data$quality$avg.quality),
              pal.avg(countries@data$quality$avg.quality),
              pal.n(countries@data$quality$n))
  pal.name <- c(rep("Average wine quality",2),"Amount of wines")

  output$PriceQuality <- renderPlot({
    if(input$click.country %in% unique.countries) {
      data %>%
        filter(is.null(input$variety) | variety %in% input$variety,
               country == input$click.country) %>%
        ggplot(aes(points, log(price))) +
        geom_point(col = "navyblue", alpha = 0.5) +
        geom_smooth(method = "gam") +
        theme_bw() +
        labs(y = "Price (log)", x = "Quality")
    } else {
      data %>%
        filter(is.null(input$variety) | variety %in% input$variety) %>%
        ggplot(aes(points, log(price))) +
        geom_point(col = "navyblue", alpha = 0.5) +
        geom_smooth(method = "gam") +
        theme_bw() +
        labs(y = "Price (log)", x = "Quality")
    }
  })


  output$mymap <- renderLeaflet({
    leaflet(countries) %>%
      setView(0, 0, 2) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('pk.eyJ1IjoiYmFydGozaCIsImEiOiJjam81amF6ODcwODBqM3FvYTlrN2E3azlvIn0.PUtXU40gLYiECsGAMzeYiw'))) %>%
      addPolygons(
        fillColor = pal[[match(input$label, c("Average quality",
                                              "Most common wine",
                                              "Amount of wines"))]],
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = grouped.labels[[match(input$label, c("Average quality",
                                                     "Most common wine",
                                                     "Amount of wines"))]],
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId = ~countries$ADMIN) %>%
      addLegend(pal = pal.fun[[match(input$label, c("Average quality",
                                                    "Most common wine",
                                                    "Amount of wines"))]],
                values = ~density, opacity = 0.7,
                title = pal.name[match(input$label, c("Average quality",
                                                      "Most common wine",
                                                      "Amount of wines"))],
                position = "bottomleft") %>%
      addMiniMap(position = "bottomleft")
  })

  observeEvent(input$mymap_shape_click, {
    x <- input$mymap_shape_click
    updateSelectInput(session, "click.country", selected=x$id)
  })

  output$winetable <- DT::renderDataTable({

    cleantable <- data[,c("title","points", "price" ,"country",
                          "province", "variety", "winery")] %>%
      filter(is.na(points) == FALSE,
             is.na(price) == FALSE,
             is.na(title) == FALSE)  %>%
      rename('quality' = `points`) %>%
      arrange(desc(quality)) %>%
      filter(is.null(input$country) | country %in% input$country,
             is.null(input$province) | province %in% input$province,
             is.null(input$table.variety) | variety %in% input$table.variety,
             price >= min(input$pricerange),
             price <= max(input$pricerange))

    DT::datatable(cleantable, escape = FALSE)
  })

  output$best_wine <- function(){
       get_wine(Country=input$click.country,
               Variety = input$variety,
                Data=winemag,
                N=5) %>% select(title) %>% kable(col.names=c(" ")) %>%
                    kable_styling(bootstrap_options = "striped", font_size=12)
  }
}
