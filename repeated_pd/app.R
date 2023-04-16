library(shiny)
library(tidyverse)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Repeated Prisoners' Dilemma"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("prob_continue", "Probability of Another Round", min = 0, max = 0.999, value = 0.95, step = 0.01),
      sliderInput("temptation_payoff", "Temptation Payoff", min = 0, max = 10, value = 4, step = 1),
      sliderInput("suckers_payoff", "Sucker's Payoff", min = 0, max = 10, value = 1, step = 1),
      sliderInput("coop_payoff", "Cooperative Payoff", min = 0, max = 10, value = 3, step = 1),
      sliderInput("defect_payoff", "Defection Payoff", min = 0, max = 10, value = 2, step = 1),
      selectInput("player1_strategy", "Player 1 Strategy", choices = c("Grim Trigger", "Tit for Tat", "Random", "Always Cooperate", "Always Defect"), selected = "Random"),
      selectInput("player2_strategy", "Player 2 Strategy", choices = c("Grim Trigger", "Tit for Tat", "Random", "Always Cooperate", "Always Defect"), selected = "Random")
    ),
    mainPanel(
      verbatimTextOutput("payoff_matrix"),
      plotOutput("totals_graph"),
      dataTableOutput("game_history"),
      dataTableOutput("cum_payoff"),
      p("This model is coded with", a("R and Shiny",href="https://shiny.rstudio.com/"), "by", a("Ryan Safner",href="http://ryansafner.com"))
    )
  )
)

# Define the server
server <- function(input, output) {
  # Collect inputs for payoffs
  
  payoff_matrix <- reactive({
    coop_payoff <- input$coop_payoff
    defect_payoff <- input$defect_payoff
    suckers_payoff <- input$suckers_payoff
    temptation_payoff <- input$temptation_payoff
    
    # Define the payoff matrix
    payoff_matrix <- matrix(c(coop_payoff, suckers_payoff, temptation_payoff, defect_payoff), nrow = 2, ncol = 2, byrow = TRUE,
                            dimnames = list(c("Cooperate", "Defect"), c("Cooperate", "Defect")))
  })
  
  output$payoff_matrix <- renderPrint({
    payoff_matrix()
  })
  
  # Define a function to play one round of the game
  play_round <- function(player1_move, player2_move, payoff_matrix) {
    player1_payoff <- payoff_matrix()[player1_move, player2_move][1]
    player2_payoff <- payoff_matrix()[player2_move, player1_move][1]
    return(c(player1_payoff, player2_payoff))
  }
  
  # Define a function to choose a move based on a given strategy
  choose_move <- function(strategy, history) {
    if (strategy == "Grim Trigger") {
      if ("Defect" %in% history$player2_move) {
        return("Defect")
      } else {
        return("Cooperate")
      }
    } else if (strategy == "Tit for Tat") {
      if (nrow(history) == 0) {
        return("Cooperate")
      } else {
        return(history$player2_move[nrow(history)])
      }
    } else if (strategy == "Random") {
      p <- runif(1,0,1)
      if (p > 0.5) {
        return("Cooperate")
      } else {
        return("Defect")
      }
    } else if (strategy == "Always Cooperate") {
      return("Cooperate")
    } else if (strategy == "Always Defect") {
      return("Defect")
    } else {
      stop("Invalid strategy")
    }
  }
  
  # Define the game history as a reactive value
  game_history <- reactive({
    # Set the initial state of the game
    history <- data.frame(round = integer(),
                          player1_move = character(),
                          player2_move = character(),
                          player1_payoff = numeric(),
                          player2_payoff = numeric(),
                          stringsAsFactors = FALSE)
    num_rounds <- 1/(1-input$prob_continue)
    
    # Play the game for a given number of rounds
    for (i in 1:num_rounds) {
      # Choose moves for both players
      player1_move <- choose_move(input$player1_strategy, history)
      player2_move <- choose_move(input$player2_strategy, history)
      
      # Calculate payoffs
      round_payoffs <- play_round(player1_move, player2_move, payoff_matrix)
      player1_payoff <- round_payoffs[1]
      player2_payoff <- round_payoffs[2]
      
      # Add the round to the history
      history <- history %>%
        add_row(round = i, player1_move = player1_move, player2_move = player2_move,
                player1_payoff = player1_payoff, player2_payoff = player2_payoff)
      
      # Determine whether to continue to the next round
      if (runif(1) > input$prob_continue) {
        break
      }
    }
    
    
    # Return the game history
    return(history)
    
  })
  
  output$game_history <- renderDataTable({
    game_history() %>%
      tibble() %>%
      rename("Round" = round,
             "Player 1 Choice" = player1_move,
             "Player 2 Choice" = player2_move,
             "Player 1 Payoff" = player1_payoff,
             "Player 2 Payoff" = player2_payoff) %>%
      datatable(.,
                rownames = F,
      )
  })
  
  output$cum_payoff <- renderDataTable({
    
    game_history() %>% 
      tibble() %>%
      summarize_each(c(player1_payoff, player2_payoff), fun = sum) %>%
      rename("Player 1 Total Payoff" = player1_payoff,
             "Player 2 Total Payoff" = player2_payoff) %>%
      datatable(.,
                rownames = F,
                options = list(dom = 't') # only show table, no other options
      )
  })
  
  output$totals_graph <- renderPlot({
    
    game_history() %>%
      select(round, player1_payoff, player2_payoff) %>%
      pivot_longer(., cols = c(player1_payoff, player2_payoff), names_to = "player") %>%
      rename("Payoff" = value,
             "Player" = player) %>%
      mutate(Player = case_when(Player == "player1_payoff" ~ "Player 1",
                                Player == "player2_payoff" ~ "Player 2")
      ) %>%
      group_by(Player) %>%
      mutate(cs = cumsum(Payoff)) %>%
      ggplot(data = .)+
      aes(x = round,
          y = cs,
          color = Player)+
      geom_path(size = 2)+
      labs(x = "Round",
           y = "Cumulative Payoff",
           color = NULL)+
      theme_light(base_family = "Fira Sans Condensed", base_size = 14)+
      scale_color_viridis_d()+
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)