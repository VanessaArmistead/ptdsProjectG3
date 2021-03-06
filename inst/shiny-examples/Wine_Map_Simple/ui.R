library("shiny")
library("leaflet")
library("ptdsProjectG3")
library("data.table")


# Define UI for application
ui <- fluidPage(
  navbarPage("Wine Quality", id="nav",

             tabPanel("Interactive map",
                      div(class="outer",

                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                          ),

                          leafletOutput("mymap", width = '100%', height = '100%'),

                          absolutePanel(id = "controls",
                                        class = "panel panel-default",
                                        fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto",
                                        right = 20, bottom = "auto",
                                        width = 330, height = "auto",

                                        h2("Wine data explorer"),

                                        h4("Quality/Price Analysis"),

                                        plotOutput("PriceQuality", height = 250),

                                        selectInput("click.country",
                                                    "Graph displayed for: ",
                                                    c("All countries" = "",
                                                      sort(unique(winemag$country))),
                                                    selected = ""),

                                        h4("Table of Top 5 Wines"),

                                        tableOutput("best_wine")
                          ),

                          absolutePanel(id = "controls",
                                           class = "panel panel-default",
                                           fixed = TRUE,
                                           draggable = TRUE, top = 60, left = 20,
                                           right = "auto", bottom = "auto",
                                           width = 330, height = "auto",

                                           h2("Map Information"),

                                           selectInput("label", "Label",
                                                       c("Average quality",
                                                         "Most common wine",
                                                         "Amount of wines"),
                                                       multiple=FALSE)


                          ),


                          tags$div(id="cite",
                                   "Data compiled from ", tags$em('WineEnthusiast'),
                                   "."
                          )
                      )
             ),


             tabPanel("Data explorer",

                      fluidRow(
                        column(width = 10,
                               sliderInput("pricerange", "Price range",
                                           min=0, max=1000, value=c(0,1000))
                        )
                      ),
                      hr(),
                      DT::dataTableOutput("winetable")
             )
  )
)
