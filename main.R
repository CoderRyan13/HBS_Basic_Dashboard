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


connectDB <- function(){
  connection <- dbConnect(RMySQL::MySQL(), 
                          dbname = Sys.getenv("MYSQL_SURVEY_DB"), 
                          host = Sys.getenv("MYSQL_HOST"), 
                          port = 3306, 
                          user = Sys.getenv("MYSQL_USER"), 
                          password = Sys.getenv("MYSQL_PWD"))
  return(connection)
}

fetchWholeTable <- function(table){
  d2 <- connectDB()
  
  data <- dbReadTable(d2, table)
  
  dbDisconnect(d2)
  
  data
}

fetchQuery <- function(q){
  d2 <- connectDB()
  
  data <- dbGetQuery(d2, q)
  
  dbDisconnect(d2)
  
  data
}

level_choices <-
  c(
    'national',
    'urban_rural',
    'district',
    'ED_number'
  )

names(level_choices) <- 
  c(
    'National',
    'Urban Rural',
    'Distirct',
    'ED'
  )

calculateHHMetric <- function(val) {
  if (val == 'national'){
    
    hh_vars <- 
      households |>
      summarise(
        Questionnaires = n(),
        Respondents = sum(FINALRESULT_hh %in% c('1', '2'), na.rm = T)
      )
    
    
    listing_vars <-
      listing |>
      summarise(
        Males = sum(sex == 1, na.rm = T),
        Females = sum(sex == 2, na.rm = T)
      ) |> 
      summarise(
        'Avg Males' = mean(Males, na.rm = T),
        `Avg Females` = mean(Females, na.rm = T)
      )
    
    
    df <- cbind(hh_vars, listing_vars)
    
  } else {
    
    hh_vars <- 
      households |>
      group_by(!!as.name(val)) |>
      summarise(
        Questionnaires = n(),
        Respondents = sum(FINALRESULT_hh %in% c('1', '2'), na.rm = T)
      )
    
    
    listing_vars <-
      listing |>
      group_by(!!as.name(val), interview__key) |>
      summarise(
        Males = sum(sex == 1, na.rm = T),
        Females = sum(sex == 2, na.rm = T)
      ) |>
      group_by(!!as.name(val)) |>
      summarise(
        'Avg Males' = mean(Males, na.rm = T),
        `Avg Females` = mean(Females, na.rm = T)
      )
    
    
    df <- hh_vars |>
      left_join(
        listing_vars,
        by = val
      )
  }
  reactable(df)
}


calculateHHResponse <- function(val) {
  if (val == 'national'){
    hh_val <- 
      households |>
      summarise(
        `Response %` = paste0(sprintf(((sum(FINALRESULT_hh %in% 1, na.rm = T)+sum(FINALRESULT_hh %in% 2, na.rm = T))/
                                 (sum(FINALRESULT_hh %in% 1, na.rm = T)+sum(FINALRESULT_hh %in% 2, na.rm = T)+
                                    sum(FINALRESULT_hh %in% 4, na.rm = T)+sum(FINALRESULT_hh %in% 6, na.rm = T)+
                                    sum(FINALRESULT_hh %in% 7, na.rm = T))), fmt = '%.1f'), '%'), 
        `Complete %` = paste0(sprintf(sum(FINALRESULT_hh %in% 1, na.rm = T)/(sum(FINALRESULT_hh %in% 2, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 3, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 4, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 5, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 6, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 7, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 8, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 9, na.rm = T)+
                                                                       sum(FINALRESULT_hh %in% 0, na.rm = T)), fmt = '%.1f'), '%'),
        Completed = sum(FINALRESULT_hh %in% 1, na.rm = T),
        Partial = sum(FINALRESULT_hh %in% 2, na.rm = T),
        `Vacant Dwelling` = sum(FINALRESULT_hh %in% 3, na.rm = T),
        Refusal = sum(FINALRESULT_hh %in% 4, na.rm = T),
        `A.N.F` = sum(FINALRESULT_hh %in% 5, na.rm = T),
        `N.S.R` = sum(FINALRESULT_hh %in% 6, na.rm = T),
        `No Contact` = sum(FINALRESULT_hh %in% 7, na.rm = T),
        `Vacant Lot` = sum(FINALRESULT_hh %in% 8, na.rm = T),
        `U.C/N.L` = sum(FINALRESULT_hh %in% 9, na.rm = T),
        Other = sum(FINALRESULT_hh %in% 0, na.rm = T)
      )
    
  } else {
    
    hh_val <- 
      households |>
      group_by(!!as.name(val)) |>
      summarise(
        `Response %` = paste0(sprintf(((sum(FINALRESULT_hh %in% 1, na.rm = T)+
                                          sum(FINALRESULT_hh %in% 2, na.rm = T))/
                                         (sum(FINALRESULT_hh %in% 1, na.rm = T)+
                                            sum(FINALRESULT_hh %in% 2, na.rm = T)+
                                            sum(FINALRESULT_hh %in% 4, na.rm = T)+
                                            sum(FINALRESULT_hh %in% 6, na.rm = T)+
                                            sum(FINALRESULT_hh %in% 7, na.rm = T))), fmt = '%.1f'), '%'), 
        `Complete %` = paste0(sprintf(sum(FINALRESULT_hh %in% 1, na.rm = T)/(sum(FINALRESULT_hh %in% 2, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 3, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 4, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 5, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 6, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 7, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 8, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 9, na.rm = T)+
                                                                               sum(FINALRESULT_hh %in% 0, na.rm = T)), fmt = '%.1f'), '%'),
        Completed = sum(FINALRESULT_hh %in% 1, na.rm = T),
        Partial = sum(FINALRESULT_hh %in% 2, na.rm = T),
        `Vacant Dwelling` = sum(FINALRESULT_hh %in% 3, na.rm = T),
        Refusal = sum(FINALRESULT_hh %in% 4, na.rm = T),
        `A.N.F` = sum(FINALRESULT_hh %in% 5, na.rm = T),
        `N.S.R` = sum(FINALRESULT_hh %in% 6, na.rm = T),
        `No Contact` = sum(FINALRESULT_hh %in% 7, na.rm = T),
        `Vacant Lot` = sum(FINALRESULT_hh %in% 8, na.rm = T),
        `U.C/N.L` = sum(FINALRESULT_hh %in% 9, na.rm = T),
        Other = sum(FINALRESULT_hh %in% 10, na.rm = T)
      )
  }
  reactable(hh_val)
}



defTable <- function(table) {
  reactable(
    table,
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
}



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

consumption <- fetchWholeTable('hbs_consumption_pattern_roster')


households <- fetchWholeTable('hbs_household')

ff <- fetchWholeTable('hbs_ff_roster')

listing <- fetchWholeTable('hbs_listing_roster')

trans1 <- fetchWholeTable('hbs_transportation_1')

trans2 <- fetchWholeTable('hbs_transportation_2')

plotquery1 <- fetchQuery('SELECT name, age FROM hbs_listing_roster')

# cbind(hh_vars, listing_vars)

# loading the dataset
data <- as.data.frame(Titanic);
# Building a table with the data for the plot
PD <- data |> group_by(Class, Survived) |> summarise(n = sum(Freq));


setwd("C:\\Users\\sib_temp\\Music\\HBS_2024")

usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

mapdata <- read.csv("airports.csv")
#head(mapdata)

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
               menuSubItem("Basic Map", tabName = "responsemap")),
      menuItem("Visuals", tabName = "visuals", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("visuals", fluidPage(
        "Visuals"
      )),
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
        defTable(consumption)
      )
      ),
      tabItem("ffroster", fluidPage(
        h4("FF Roster Table"),
        defTable(ff)
      )
      ),
      tabItem("listingroster", fluidPage(
        h4("Listing Roster Table"),
        defTable(listing)
      )
      ),
      tabItem("trans1", fluidPage(
        h4("Transportation 1 Table"),
        defTable(trans1)
      )
      ),
      tabItem("trans2", fluidPage(
        h4("Transportation 2 Table"),
        defTable(trans2)
      )
      ),
      tabItem("hhmetrics",
              fluidPage(
                sidebarLayout(
                  sidebarPanel( width = 2,
                    selectInput('metric_select', 'Select Level:', choices = level_choices)
                  ),
                  mainPanel( width = 10,
                    reactableOutput("metric_table")
                  )
                )
              ) 
      ),
      tabItem("hhresponse",
              fluidPage(
                sidebarLayout( 
                  sidebarPanel( width = 2,
                    selectInput('hhresponse_select', 'Select Level:', choices = level_choices)
                  ),
                  mainPanel( width = 10,
                    reactableOutput("hhresponse_table")
                  )
                )
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
        h4("US Airports"),
        leaflet(data = mapdata) %>%
          setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
          addTiles() %>%
          addMarkers(~LONGITUDE, ~LATITUDE, popup = ~AIRPORT, label = ~AIRPORT) %>%
          addProviderTiles(providers$Esri.WorldStreetMap)
      )
      )
    )
  )
)

server <- function(input, output) {
  output$plotted <- renderPlot(
    # barplot(result6)
    ggplot(plotquery1[1:10, ], aes(x = reorder(name, -(as.integer(as.character(age)))), y = as.integer(as.character(age)))) + 
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
  
  output$metric_table <- renderReactable({
    val <- input$metric_select
    calculateHHMetric(val)
  }
  )
  
  output$hhresponse_table <- renderReactable({
    val <- input$hhresponse_select
    calculateHHResponse(val)
  })

}

shinyApp(ui, server)