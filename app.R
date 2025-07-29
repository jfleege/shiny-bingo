
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

    # application title
    titlePanel("Bingo in R (Shiny Edition)"),

    sidebarLayout(
      sidebarPanel(
        radioButtons("players", "Number of players:", choices = c(1, 2), selected = 1),
        actionButton("new_game", "Start New Game"),
        actionButton("next_num", "Call Next Number"),
        textOutput("called_number"),
        textOutput("winner_text")
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
    
    output$called_number <- renderText({
      paste("Last number called:", number_drawn)
    })
  })
  
  output$winner_text <- renderText({
    req(rv$winner)
    rv$winner
  })
  
  output$bingo_boards <- renderUI({
    tagList(
      lapply(1:rv$players, function(i) {
        tagList(
          strong(paste("Player", i)),
          tableOutput(paste0("board", i))
        )
      })
    )
  })
  
  observe({
    for (i in 1:rv$players) {
      local({
        idx <- i
        output[[paste0("board", idx)]] <- renderTable({
          rv$boards[[idx]]
        }, bordered = TRUE, align = "c")
      })
    }
  })
}

#---------------------------------------------------------------#

# run the application 
shinyApp(ui = ui, server = server)
