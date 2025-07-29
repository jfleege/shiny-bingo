
library(shiny)
library(bslib)

#---------------------------------------------------------------#

# helper function to create a bingo board
create_board <- function() {
  number_pool <- c(1:50)
  rough_player_card <- sample(number_pool, 25)
  
  temp_player_card <- matrix(
    data = rough_player_card, 
    nrow = 5, ncol = 5, 
    byrow = FALSE
  )
  
  colnames(temp_player_card) <- c("B", "I", "N", "G", "O")
  
  temp_player_card[3,3] <- "FREE"
  
  temp_player_card <- as.data.frame(temp_player_card, row.names = NULL)
  
  return(temp_player_card)
}

# marks a card if number matches
mark_card <- function(card, number) {
  card[card == number] <- "X"
  return(card)
}

# checks if a card has a bingo
check_for_bingo <- function(card) {
  is_bingo <- function(line) all(line == "X" | line == "FREE")
  rows_bingo <- any(apply(card, 1, is_bingo))
  cols_bingo <- any(apply(card, 2, is_bingo))
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  diag_bingo <- is_bingo(diag1) || is_bingo(diag2)
  return(rows_bingo || cols_bingo || diag_bingo)
}

# tracks how many moves a player has until they win the game
moves_till_bingo <- function(card) {
  counts <- c()
  
  for (r in 1:nrow(card)) {
    line <- card[r, ]
    counts <- c(counts, sum(line != "X" & line != "FREE"))
  }
  
  for (c in 1:ncol(card)) {
    line <- card[, c]
    counts <- c(counts, sum(line != "X" & line != "FREE"))
  }
  
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  counts <- c(counts,
              sum(diag1 != "X" & diag1 != "FREE"),
              sum(diag2 != "X" & diag2 != "FREE"))
  
  return(min(counts))
}

#---------------------------------------------------------------#

# defines UI for application that creates/plays bingo
ui <- fluidPage(
  
  # change theme
  theme = bs_theme(version = 5, bootswatch = "united"),
  
  # dark mode option
  input_dark_mode(id = "mode"),
  
  # changes player text characteristics
  tags$style(HTML("
  .control-label[for='players'] {
    font-size: 20px;
    font-weight: bold;
  }
  ")),
  
  # changes label characteristics
  tags$style(HTML("
    .radio label {
      font-size: 18px;
      padding: 5px 0;
    }

    .radio input[type='radio'] {
      transform: scale(1.4);
      margin-right: 8px;
    }
  ")),
  
  # changes title characteristics
  tags$style(HTML("
  h2 {
    font-size: 32px;
    font-weight: bold;
  }
  ")),
  
  # application title
  titlePanel("Bingo in R (Shiny Edition)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("players", "Number of players:", choices = c(1, 2), selected = NULL),
      
      actionButton("new_game", "Start New Game", style = 
        "font-size: 18px;
        padding: 6px 10px;
        background-color: orange;
        color: white;
        border-radius: 8px;
        border: none;
        box-shadow: 2px 2px 5px grey;
      "),
      
      br(), br(),
      
      actionButton("next_num", "Call Next Number", style = 
        "font-size: 18px;
        padding: 6px 10px;
        background-color: orange;
        color: white;
        border-radius: 8px;
        border: none;
        box-shadow: 2px 2px 5px grey;
      "),
      
      br(), br(),
      
      uiOutput("called_number"),
      
      uiOutput("winner_text")
    ),
    
    mainPanel(
      uiOutput("bingo_boards")  # will be used to render the board tables
    )
  )
)

#---------------------------------------------------------------#

# defines the server logic to help the ui run
server <- function(input, output) {
  rv <- reactiveValues(
    players = 1,
    boards = list(),
    numbers_left = 1:50,
    move_count = 0,
    winner = NULL
  )
  
  observeEvent(input$new_game, {
    rv$players <- input$players
    rv$boards <- lapply(1:rv$players, function(i) create_board())
    rv$numbers_left <- 1:50
    rv$move_count <- 0
    rv$winner <- NULL
  })
  
  observeEvent(input$next_num, {
    if (length(rv$numbers_left) == 0 || !is.null(rv$winner)) return()
    
    number_drawn <- sample(rv$numbers_left, 1)
    rv$numbers_left <- setdiff(rv$numbers_left, number_drawn)
    rv$move_count <- rv$move_count + 1
    
    rv$boards <- lapply(rv$boards, mark_card, number = number_drawn)
    
    for (i in 1:rv$players) {
      if (check_for_bingo(rv$boards[[i]])) {
        rv$winner <- paste("Player", i, "wins in", rv$move_count, "moves!")
        break
      }
    }
    
    output$called_number <- renderUI({
      HTML(paste0("<div style='font-weight: bold; font-size: 18px;'>Last number called: ", number_drawn, "</div>"))
    })
    
    # bingo moves notification
    lapply(1:rv$players, function(i) {
      moves_left <- moves_till_bingo(rv$boards[[i]])
      showNotification(
        paste("Player", i, "is", moves_left, "move(s) away from Bingo!"),
        type = "warning",  # or "warning"/"error" if needed
        duration = 1
      )
    })
  })
  
  output$winner_text <- renderUI({
    req(rv$winner)
    HTML(paste0("<div style=' font-weight: bold; font-style: italic; font-size: 18px; color: green;'>", rv$winner, "</div>"))
  })
  
  output$bingo_boards <- renderUI({
  fluidRow(
    lapply(1:rv$players, function(i) {
      column(
        width = 6,
        tags$h4(style = "font-weight: bold; font-size: 24px;", paste("Player", i)),
        tableOutput(paste0("board", i))
      )
    })
  )
})
  
  observe({
    for (i in 1:rv$players) {
      local({
        idx <- i
        output[[paste0("board", idx)]] <- renderUI({
          board <- rv$boards[[idx]]
          
          # build each row of the table
          table_rows <- lapply(1:5, function(r) {
            tags$tr(
              lapply(1:5, function(c) {
                val <- board[r, c]
                
                # cell styling
                style <- if (val == "X") {
                  "background-color: lightcoral; font-weight: bold; text-align: center;"
                } else if (val == "FREE") {
                  "background-color: lightgreen; font-weight: bold; font-style: italic; text-align: center;"
                } else {
                  "text-align: center;"
                }
                
                tags$td(style = paste("border: 3px solid gray; padding: 10px;", style), val)
              })
            )
          })
          
          # assemble full table
          tags$table(
            style = "border-collapse: collapse; margin-bottom: 30px;",
            tags$thead(
              tags$tr(lapply(c("B", "I", "N", "G", "O"), function(h) {
                tags$th(style = "border: 3px solid gray; padding: 10px; font-size: 18px;", h)
              }))
            ),
            tags$tbody(table_rows)
          )
        })
      })
    }
  })
}

#---------------------------------------------------------------#

# run the application 
shinyApp(ui = ui, server = server)

