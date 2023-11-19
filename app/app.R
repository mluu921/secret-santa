library(shiny)
library(bslib)
library(dplyr)
library(gt)
library(rlang)
library(bsicons)
library(lubridate)

random_names <- babynames::babynames |>
  filter(year == 2010) |>
  pull(name) |> sample(size = 15)

secret_santa_randomizer <-
  \(participants, additional_criteria = NULL) {
    giver <- participants
    
    recipients <- giver
    
    data <- tibble(giver = giver,
                   recipients = recipients)
    
    i <- 0
    
    condition <- TRUE
    
    expr({
      while (condition) {
        i <- i + 1
        data <- tibble(giver = sample(giver),
                       recipients = sample(recipients)) |>
          mutate(results = paste0(giver, " -> ", recipients))
        
        condition <- any(data$giver == data$recipients) | any(data$results %in% additional_criteria)
        
      }
      
    }) |> eval()
    
    data <- data |> 
      mutate(
        text = glue::glue("{giver} will be giving a gift to {recipients}."))
    
    tibble::lst(data,
                i)
    
  }


# t <- secret_santa_randomizer(random_names)

if(Sys.Date() > mdy(paste0('12-25', year(Sys.Date())))) {
  
  days_until_next_christmas <- \(year = year(Sys.Date())) {
    
    mdy(paste0('12/25/', year + 1)) - Sys.Date()
    
  }
  
} else {
  
  days_until_next_christmas <- \(year = year(Sys.Date())) {
    
    mdy(paste0('12/25/', year)) - Sys.Date()
    
  }
}

countdown <- days_until_next_christmas(year = year(Sys.Date()))

vbs <- list(
  value_box(
    title = 'Number of Iterations',
    value = textOutput('iterations'),
    showcase = bs_icon("sort-up"),
    height = 150,
    theme_color = 'primary'
  ),
  value_box(
    title = 'Total Participants',
    value = textOutput('participants'),
    showcase = bs_icon("people-fill"),
    height = 150,
    theme_color = 'danger'
  ),
  value_box(
    title = 'Days Until Christmas',
    value = h3(countdown),
    showcase = bs_icon("calendar-date"),
    height = 150,
    theme_color = 'success'
  )
  
)

ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(version = 5, bootswatch = "simplex"),
  title = "Secret Santa Randomizer",
  fillable = FALSE,
  sidebar = sidebar(
    open = 'open',
    width = '300px',
    selectizeInput(
      inputId = "participants",
      label = "Participants",
      choices = random_names,
      selected = random_names,
      options = list(create = TRUE),
      multiple = TRUE
    ),
    selectizeInput(
      inputId = "additional_criteria",
      label = "Additional Exclusion Criteria",
      choices = NULL,
      options = list(create = TRUE),
      multiple = TRUE
    ),
    actionButton(
      inputId = "randomize",
      label = "Randomize"
    )
  ),
  layout_column_wrap(!!!vbs, width = '250px'),
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

    secret_santa_randomizer(input$participants, input$additional_criteria)
    
  })

  output$res <- gt::render_gt({
    
    results()$data |> 
      gt() |> 
      cols_hide(columns = c(giver, recipients)) |>
      cols_align(align = "left", columns = c(text, results)) |>
      opt_interactive(use_compact_mode = TRUE,
                      page_size_default = 15) |> 
      cols_label(giver = "Giver",
                 recipients = "Recipients",
                 results = "Results",
                 text = 'Summary')
    
  })
  
  output$iterations <- renderText({
    
    results()$i
      
  })
  
  output$participants <- renderText({
    
    length(input$participants)
    
  })
  
}

shinyApp(ui, server)