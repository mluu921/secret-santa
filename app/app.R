library(shiny)
library(bslib)
library(dplyr)
library(gt)

random_names <- babynames::babynames |>
  filter(year == 2010) |>
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
  
  tibble::lst(
    data,
    i
  )
  
}

data <- secret_santa_randomizer(
  babynames::babynames |>
    filter(year == 2010) |>
    pull(name) |> sample(size = 10)
)


ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
  title = "Secret Santa Randomizer",
  fillable = FALSE,
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
  value_box(
    title = 'Iterations',
    value = textOutput('iterations'),
    showcase = icon("users"),
    height = 150
  ),
  card(
    card_header("Participants",
                status = "primary"),
    card_body(
      gt::gt_output("res")),
    fill = TRUE
  )
)

server <- function(input, output, session) {

  results <- eventReactive(input$randomize, {

    secret_santa_randomizer(input$participants)
    
  })

  output$res <- gt::render_gt({
    
    results()$data |> 
      gt() |> 
      cols_label(giver = "Giver",
                 recipients = "Recipients",
                 results = "Results")
    
  })
  
  output$iterations <- renderText({
    
    results()$i
      
  })
  
}

shinyApp(ui, server)