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

                                        selectInput("label", "Label",
                                                    c("Average quality",
                                                      "Most common wine",
                                                      "Amount of wines"),
                                                    multiple=FALSE),

                                        selectInput("variety",
                                                    "Variety",
                                                    c("All variaties"="",
                                                      sort(unique(winemag$variety))),
                                                    multiple=TRUE),

                                        plotOutput("PriceQuality", height = 250),
                                        selectInput("click.country",
                                                    "Graph displayed for: ",
                                                    c("All countries" = "",
                                                      sort(unique(winemag$country))),
                                                    selected = "")
                          ),

                          tags$div(id="cite",
                                   'Data compiled from ', tags$em('WineEnthusiast'),
                                   ' on November 22nd, 2017.'
                          )
                      )
             ),


             tabPanel("Data explorer",

                      fluidRow(
                        column(3,
                               selectInput("table.variety", "Varieties",
                                           c("All variaties"="",
                                             sort(unique.varieties)),
                                           multiple=TRUE)
                        ),
                        column(3,
                               selectInput("country", "Countries",
                                           c("All countries"="",
                                             sort(unique.countries)),
                                           multiple=TRUE)
                        ),
                        column(3,
                               conditionalPanel("input.country",
                                                selectInput("province",
                                                            "Provinces",
                                                            c("All provinces"=""),
                                                            multiple=TRUE)
                               )

                        )
                      ),
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
