

library(shiny)
library(dplyr)
library(reactable)

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test Metrics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('metric_select', 'Select Level:', choices = level_choices)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           reactableOutput("metric_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$metric_table <- renderReactable({
    
    val <- input$metric_select
    
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
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
