library(shiny)
library(bslib)
library(tidyverse)
library(networkD3)
library(igraph)

random_names <- babynames::babynames %>%
  filter(year == 2010) %>%
  pull(name) |> sample(size = 15)

secret_santa_randomizer <- \(participants) {
  
  giver <- participants
  
  recipients <- giver
  
  data <- tibble(giver = giver,
                 recipients = recipients)
  
  i <- 0
  while(any(data$giver == data$recipients)) {
    i <- i + 1
    data <- tibble(giver = sample(giver),
                   recipients = sample(recipients))
    
  }
  
  data <- data |> 
    mutate(
      results = paste0(giver, " -> ", recipients)
    )
  
  return(data)
  
}

data <- secret_santa_randomizer(
  babynames::babynames %>%
    filter(year == 2010) %>%
    pull(name) |> sample(size = 10)
)


cards <- list(card(
  card_header("Participants",
              status = "primary"),
  card_body(tableOutput("res"))
),
card(
  card_header('Network'),
  card_body(networkD3::simpleNetworkOutput("network"))
))

ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  title = "Secret Santa Randomizer",
  sidebar = sidebar(
    selectizeInput(
      inputId = "participants",
      label = "Participants",
      choices = random_names,
      selected = random_names,
      options = list(create = TRUE),
      multiple = TRUE
    ),
    actionButton(
      inputId = "randomize",
      label = "Randomize"
    )
  ),
  layout_column_wrap(cards[[1]], cards[[2]], width = .5)

)

server <- function(input, output, session) {

  results <- eventReactive(input$randomize, {

    secret_santa_randomizer(input$participants)

  })

  output$res <- renderTable({
    
    results()
    
  })
  
  output$network <- networkD3::renderSimpleNetwork({
    
    simplenet <- results() |> 
      select(giver, recipients)
    
    simpleNetwork(
      simplenet,
      height = "500px",
      width = "500px",
      fontSize = 30,
      linkDistance = 300,
      charge = -100
    )
    
    
  })
  
}

shinyApp(ui, server)