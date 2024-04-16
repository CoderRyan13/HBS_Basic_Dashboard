library(bs4Dash)
library(reactable)
library(shinydashboard)
library(leaflet)
library(webr)
library(dplyr)
# importing the library
library("devtools")
devtools::install_github("arthur-shaw/susoapi")
library("susoapi")
library("tidyverse")
library("haven")
#library("plyr")

library(DBI)
library(RMySQL)
library(ggplot2)
# creating a database connection
connection <- dbConnect(RMySQL::MySQL(), 
                        dbname = Sys.getenv("MYSQL_SURVEY_DB"), 
                        host = Sys.getenv("MYSQL_HOST"), 
                        port = 3306, 
                        user = Sys.getenv("MYSQL_USER"), 
                        password = Sys.getenv("MYSQL_PWD"))


set_credentials(
  server = "http://hqsurveys.sib.org.bz",
  workspace = "hbs",
  user = "gapi",
  password = "API4statistics!"
)

# Test if connection was made
# query <- "show tables";
# result <- dbGetQuery(connection,query);
# print(result)

tablequery1 <- "SELECT * FROM hbs_consumption_pattern_roster";
result1 <- dbGetQuery(connection,tablequery1);

tablequery2 <- "SELECT * FROM hbs_ff_roster";
result2 <- dbGetQuery(connection,tablequery2);

tablequery3 <- "SELECT * FROM hbs_listing_roster";
result3 <- dbGetQuery(connection,tablequery3);

tablequery4 <- "SELECT * FROM hbs_transportation_1";
result4 <- dbGetQuery(connection,tablequery4);

tablequery5 <- "SELECT * FROM hbs_transportation_2";
result5 <- dbGetQuery(connection,tablequery5);

plotquery1 <- "SELECT name, age FROM hbs_listing_roster";
result6 <- dbGetQuery(connection, plotquery1);

result7 <- c(2, 4, 1, 5, 10);

# loading the dataset
data <- as.data.frame(Titanic);
# Building a table with the data for the plot
PD <- data |> group_by(Class, Survived) |> summarise(n = sum(Freq));

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Basic HBS Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("HBS Tables", tabName = "hbstable", icon = icon("table"),
               menuSubItem("Consumption Pattern", tabName = "consumption"), 
               menuSubItem("FF Roster", tabName = "ffroster"),
               menuSubItem("Listing Roster", tabName = "listingroster"),
               menuSubItem("Transportation 1", tabName = "trans1"),
               menuSubItem("Transportation 2", tabName = "trans2")
      ),
      menuItem("Household Metrics", tabName = "hhmetrics", icon = icon("people-roof")),
      menuItem("Response Rates", tabName = "responserates", icon = icon("percent"),
               menuSubItem("Household Response", tabName = "hhresponse"), 
               menuSubItem("EA Response", tabName = "earesponse"),
               menuSubItem("FI Response", tabName = "firesponse"),
               menuSubItem("CS Response", tabName = "csresponse")
      ),
      menuItem("Payment", tabName = "payment", icon = icon("hand-holding-dollar")),
      menuItem("Maps", tabName = "maps", icon = icon("map"),
               menuSubItem("Response", tabName = "responsemap"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home", fluidPage(
        # Boxes need to be put in a row (or column)
        fluidRow(
          infoBox(
            width = 3,
            color = 'maroon',
            icon = icon("sitemap"),
            "Started Clusters:",
            "69"
            # n_started_clus
          ),
          infoBox(
            width = 3,
            color = 'maroon',
            icon = icon("sitemap"),
            "Completed Clusters:"
            # n_complete_clusters
          ),
          infoBox(
            width = 3,
            color = 'maroon',
            icon = icon("house-circle-check"),
            "HH Response:"
            # paste0(pct_response, "%")
          ),
          infoBox(
            width = 3,
            color = 'maroon',
            icon = icon("square-check"),
            "HH Completion:"
            # paste0(pct_complete, "%")
          )
        ),
        fluidRow(
          infoBox(
            width = 3,
            color = 'olive',
            icon = icon("users"),
            "Participation:"
            # paste0(pct_participation, "%")
          ),
          infoBox(
            width = 3,
            color = 'olive',
            icon = icon("bed"),
            "Unemployed:"
            # paste0(pct_unemployed, "%")
          ),
          infoBox(
            width = 3,
            color = 'olive',
            icon = icon("user-clock"),
            "Underemployed:"
            # paste0(pct_underemployed, "%")
          ),
          infoBox(
            width = 3,
            color = 'olive',
            icon = icon("user-secret"),
            "Potential:"
            # paste0(pct_potential, "%")
          )
        ),
        h4("Different Graphs"),
        fluidRow(
          column(
            width = 7,
            plotOutput("plotted")
          ),
          column(
            width = 5,
            plotOutput("chart")
          )
        )
      )
      ),  
      tabItem("consumption", fluidPage(
        h4("Consumption Pattern Table"),
        reactable(
          result1,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#A5D6A7")
          ),
          # bordered = TRUE,
          highlight = TRUE,
          striped = TRUE,
          outlined = TRUE,
          borderless = TRUE,
          filterable = TRUE,
          # columns = list(
          # row_names = colDef(filterable = FALSE)
          # ),
          # searchable = TRUE,
          # minRows = 10,
          showPageSizeOptions = TRUE,
          # pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 10,
          columns = list(
            interview__id = colDef(cell = function(value, index) {
              # Render as a link
              url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            })
          )
        )
      )
      ),
      tabItem("ffroster", fluidPage(
        h4("FF Roster Table"),
        reactable(
          result2,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#A5D6A7")
          ),
          # bordered = TRUE,
          highlight = TRUE,
          striped = TRUE,
          outlined = TRUE,
          borderless = TRUE,
          filterable = TRUE,
          # columns = list(
          # row_names = colDef(filterable = FALSE)
          # ),
          # searchable = TRUE,
          # minRows = 10,
          showPageSizeOptions = TRUE,
          # pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 10,
          columns = list(
            interview__id = colDef(cell = function(value, index) {
              # Render as a link
              url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            })
          )
        )
      )
      ),
      tabItem("listingroster", fluidPage(
        h4("Listing Roster Table"),
        reactable(
          result3,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#A5D6A7")
          ),
          # bordered = TRUE,
          highlight = TRUE,
          striped = TRUE,
          outlined = TRUE,
          borderless = TRUE,
          filterable = TRUE,
          # columns = list(
          # row_names = colDef(filterable = FALSE)
          # ),
          # searchable = TRUE,
          # minRows = 10,
          showPageSizeOptions = TRUE,
          # pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 10,
          columns = list(
            interview__id = colDef(cell = function(value, index) {
              # Render as a link
              url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            })
          )
        )
      )
      ),
      tabItem("trans1", fluidPage(
        h4("Transportation 1 Table"),
        reactable(
          result4,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#A5D6A7")
          ),
          # bordered = TRUE,
          highlight = TRUE,
          striped = TRUE,
          outlined = TRUE,
          borderless = TRUE,
          filterable = TRUE,
          # columns = list(
          # row_names = colDef(filterable = FALSE)
          # ),
          # searchable = TRUE,
          # minRows = 10,
          showPageSizeOptions = TRUE,
          # pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 10,
          columns = list(
            interview__id = colDef(cell = function(value, index) {
              # Render as a link
              url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            })
          )
        )
      )
      ),
      tabItem("trans2", fluidPage(
        h4("Transportation 2 Table"),
        reactable(
          result5,
          defaultColDef = colDef(
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 70,
            headerStyle = list(background = "#A5D6A7")
          ),
          # bordered = TRUE,
          highlight = TRUE,
          striped = TRUE,
          outlined = TRUE,
          borderless = TRUE,
          filterable = TRUE,
          # columns = list(
          # row_names = colDef(filterable = FALSE)
          # ),
          # searchable = TRUE,
          # minRows = 10,
          showPageSizeOptions = TRUE,
          # pageSizeOptions = c(10, 15, 20),
          defaultPageSize = 10,
          columns = list(
            interview__id = colDef(cell = function(value, index) {
              # Render as a link
              url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            })
          )
        )
      )
      ),
      tabItem("ffroster", fluidPage(
                  h4("FF Roster Table"),
                  reactable(
                    result2,
                    defaultColDef = colDef(
                      cell = function(value) format(value, nsmall = 1),
                      align = "center",
                      minWidth = 70,
                      headerStyle = list(background = "#A5D6A7")
                    ),
                    # bordered = TRUE,
                    highlight = TRUE,
                    striped = TRUE,
                    outlined = TRUE,
                    borderless = TRUE,
                    filterable = TRUE,
                    # columns = list(
                    # row_names = colDef(filterable = FALSE)
                    # ),
                    # searchable = TRUE,
                    # minRows = 10,
                    showPageSizeOptions = TRUE,
                    # pageSizeOptions = c(10, 15, 20),
                    defaultPageSize = 10,
                    columns = list(
                      interview__id = colDef(cell = function(value, index) {
                        # Render as a link
                        url <- sprintf("https://hqsurveys.sib.org.bz/hbs/Interview/Review/%s", value)
                        htmltools::tags$a(href = url, target = "_blank", as.character(value))
                      })
                    )
                  )
                )),
      tabItem("hhmetrics",
              tabsetPanel(
                id = "hhmetricstabset",
                tabPanel("National", fluidPage(
                  h4("National")
                )),
                tabPanel("Urban/Rural", fluidPage(
                  h4("Urban/Rural")
                )),
                tabPanel("District", fluidPage(
                  h4("District")
                )),
                tabPanel("Stratum", fluidPage(
                  h4("Stratum")
                )),
                tabPanel("Cluster", fluidPage(
                  h4("Cluster")
                )),
                tabPanel("Interviewer", fluidPage(
                  h4("Interviewer")
                ))
              ) 
      ),
      tabItem("hhresponse",
              tabsetPanel(
                id = "hhresponsetabset",
                tabPanel("National", fluidPage(
                  h4("National")
                )),
                tabPanel("Urban/Rural", fluidPage(
                  h4("Urban/Rural")
                )),
                tabPanel("District", fluidPage(
                  h4("District")
                )),
                tabPanel("Stratum", fluidPage(
                  h4("Stratum")
                )),
                tabPanel("Cluster", fluidPage(
                  h4("Cluster")
                )),
                tabPanel("Interviewer", fluidPage(
                  h4("Interviewer")
                ))
              ) 
      ),
      tabItem("earesponse",
              tabsetPanel(
                id = "earesponsetabset",
                tabPanel("National", fluidPage(
                  h4("National")
                )),
                tabPanel("Urban/Rural", fluidPage(
                  h4("Urban/Rural")
                )),
                tabPanel("District", fluidPage(
                  h4("District")
                )),
                tabPanel("Stratum", fluidPage(
                  h4("Stratum")
                )),
                tabPanel("Cluster", fluidPage(
                  h4("Cluster")
                )),
                tabPanel("Interviewer", fluidPage(
                  h4("Interviewer")
                ))
              ) 
      ),
      tabItem("firesponse",
              tabsetPanel(
                id = "firesponsetabset",
                tabPanel("National", fluidPage(
                  h4("National")
                )),
                tabPanel("Urban/Rural", fluidPage(
                  h4("Urban/Rural")
                )),
                tabPanel("District", fluidPage(
                  h4("District")
                )),
                tabPanel("Stratum", fluidPage(
                  h4("Stratum")
                )),
                tabPanel("Cluster", fluidPage(
                  h4("Cluster")
                )),
                tabPanel("Interviewer", fluidPage(
                  h4("Interviewer")
                ))
              ) 
      ),
      tabItem("csresponse",
              tabsetPanel(
                id = "csresponsetabset",
                tabPanel("National", fluidPage(
                  h4("National")
                )),
                tabPanel("Urban/Rural", fluidPage(
                  h4("Urban/Rural")
                )),
                tabPanel("District", fluidPage(
                  h4("District")
                )),
                tabPanel("Stratum", fluidPage(
                  h4("Stratum")
                )),
                tabPanel("Cluster", fluidPage(
                  h4("Cluster")
                )),
                tabPanel("Interviewer", fluidPage(
                  h4("Interviewer")
                ))
              ) 
      ),
      tabItem("payment",
              tabsetPanel(
                id = "paymenttabset",
                tabPanel("Interviewers", fluidPage(
                  selectInput("district", label = "District:", choices = c("Corozal" = "corozal",
                                                                           "Orange Walk" = "orangewalk",
                                                                           "Belize" = "belize",
                                                                           "Cayo" = "cayo",
                                                                           "Stann Creek" = "stanncreek",
                                                                           "Toledo" = "toledo"
                                                                           ))
                )),
                tabPanel("FS", fluidPage(
                  fluidRow(
                    column( width = 4,
                      selectInput("district", label = "District:", choices = c("Corozal" = "corozal",
                                                                               "Orange Walk" = "orangewalk",
                                                                               "Belize" = "belize",
                                                                               "Cayo" = "cayo",
                                                                               "Stann Creek" = "stanncreek",
                                                                               "Toledo" = "toledo"
                      ))
                    ),
                    column( width = 4,
                      selectInput("days", label = "Days to pay:", choices = c("1" = 1,
                                                                              "2" = 2,
                                                                              "3" = 3,
                                                                              "4" = 4,
                                                                              "5" = 5
                      ))
                    )
                  )
                  
                )),
                tabPanel("Driver", fluidPage(
                  fluidRow(
                    column( width = 4,
                            selectInput("district", label = "District:", choices = c("Corozal" = "corozal",
                                                                                     "Orange Walk" = "orangewalk",
                                                                                     "Belize" = "belize",
                                                                                     "Cayo" = "cayo",
                                                                                     "Stann Creek" = "stanncreek",
                                                                                     "Toledo" = "toledo"
                            ))
                    ),
                    column( width = 4,
                            selectInput("days", label = "Days to pay:", choices = c("1" = 1,
                                                                                    "2" = 2,
                                                                                    "3" = 3,
                                                                                    "4" = 4,
                                                                                    "5" = 5
                            ))
                    )
                  )
                ))
              )
      ),
      tabItem("responsemap", fluidPage(
        h4("Punta Gorda"),
        m <- leaflet() |>
          addTiles() |>  # Add default OpenStreetMap map tiles
          addMarkers(lng=-88.803627, lat=16.101311, popup="The birthplace of R"),
        m  # Print the map
      )
      )
    )
  )
)

server <- function(input, output) {
  output$plotted <- renderPlot(
    # barplot(result6)
    ggplot(result6, aes(x = reorder(name, -(as.integer(as.character(age)))), y = as.integer(as.character(age)))) + 
      geom_col(fill = 'royalblue4') +
      geom_text(aes(label = age), vjust = 1.5, color = "white", size = 4.2) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "People and their ages",
           subtitle = "A simple bar chart") +
      theme(
        plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        plot.subtitle = element_text(size = 12, margin = margin(10, 0, 30, 0), color = "gray"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(5, 0, 0, 0), angle = 90, hjust = 1),
        axis.text.y = element_blank()
      )
  )
  
  output$chart <- renderPlot(
    # Pie-Donut chart
    PieDonut(PD, aes(Class, Survived, count=n), title = "Titanic: Survival by Class")
  )
  

}

shinyApp(ui, server)