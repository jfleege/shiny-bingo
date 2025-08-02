library(shiny)
library(bslib)

#---------------------------------------------------------------#

# helper functions that help the bingo game run
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
  for (r in 1:nrow(card)) counts <- c(counts, sum(card[r, ] != "X" & card[r, ] != "FREE"))
  for (c in 1:ncol(card)) counts <- c(counts, sum(card[, c] != "X" & card[, c] != "FREE"))
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  counts <- c(counts, sum(diag1 != "X" & diag1 != "FREE"), sum(diag2 != "X" & diag2 != "FREE"))
  return(min(counts))
}

# default name pool for player name entering
default_names <- c("Mordecai", "Rigby", "Skips", "Benson", "Muscle Man", "Pops", "Eileen", "CJ")

#---------------------------------------------------------------#
 # stylizes panels so that they can be used in totality 
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "united"),
  input_dark_mode(id = "mode"),
  
  tags$style(HTML("
    .control-label[for='players'] {
      font-size: 22px;
      font-weight: bold;
    }
    .radio label {
      font-size: 18px;
      padding: 5px 0;
    }
    .radio input[type='radio'] {
      transform: scale(1.4);
      margin-right: 8px;
    }
    h2 {
      font-size: 32px;
      font-weight: bold;
    }
    h4 {
      font-size: 22px;
      font-weight: bold;
    }
    h5 {
      font-size: 18px;
      font-weight: bold;
      margin-top: 8;
    }
  ")),
  
  tags$style(HTML("
  .well {
    background-color: #33333 !important;
    border: none !important;
    border-radius: 10px !important;
    box-shadow: none !important;
  }
")),
  
  tags$style(HTML("
  table {
    margin-left: auto;
    margin-right: auto;
  }
")),
  
  tags$style(HTML("
  called_number {
    font-weight: bold;
  }
")),
  
  # changes header panel
  div(
    style = "
    background: linear-gradient(135deg, #CE5937 0%, #1C6EA4 63%, #C5A600 100%);
    padding: 20px;
    border-radius: 10px;
    color: white;",
    h2("Bingo in R (Shiny Edition)", 
       style = "font-weight: bold; font-size: 28px; margin-bottom: 0;")
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("players", "Number of players:", choices = c(1, 2, 3, 4), selected = NULL),
      
      uiOutput("player_names_ui"),
      
      actionButton("new_game", "Start New Game", style = 
                     "font-size: 18px; padding: 6px 10px; background-color: orange; color: white;
         border-radius: 8px; border: none; box-shadow: 2px 2px 5px grey; margin-top: 8px;"),
      
      br(),
      
      actionButton("next_num", "Call Next Number", style = 
                     "font-size: 18px; padding: 6px 10px; background-color: orange; color: white;
         border-radius: 8px; border: none; box-shadow: 2px 2px 5px grey; margin-top: 10px;"),
      
      br(),
      
      uiOutput("called_number"),
      uiOutput("winner_text"),
      uiOutput("high_score_text"),
      
      br(),
      
      tags$hr(),
      div(class = "faded-panel",
          h4("Game Stats"),
          div(style = "font-size: 18px; font-weight: bold; font-style: italic; margin-top: 10px;",
              textOutput("move_counter")
          ),
          div(style = "font-size: 18px; font-weight: bold; font-style: italic;",
              textOutput("number_pool_left")
          ),
      ),
      
      tags$hr(),
      div(
        style = "margin-top: 12px;",
        h5("GitHub: ", style = "margin-top: 12px;", tags$a(href = "https://github.com/jfleege", "https://github.com/jfleege", target = "_blank"))
      )
    ),
    
    mainPanel(
      uiOutput("bingo_boards")
    )
  )
)

#---------------------------------------------------------------#

server <- function(input, output) {
  rv <- reactiveValues(
    players = 1,
    boards = list(),
    numbers_left = 1:50,
    move_count = 0,
    winner = NULL,
    high_score = NULL
  )
  
  observeEvent(input$new_game, {
    rv$players <- input$players
    rv$player_names <- sapply(1:input$players, function(i) {
      user_input <- input[[paste0("player_name_", i)]]
      if (nzchar(trimws(user_input))) {
        user_input
      } else {
        available <- setdiff(default_names, rv$player_names)
        if (length(available) > 0) {
          sample(available, 1)
        } else {
          paste("Player", i)
        }
      }
    })
    
    rv$boards <- replicate(rv$players, create_board(), simplify = FALSE)
    rv$numbers_left <- 1:50
    rv$move_count <- 0
    rv$winner <- NULL
  })
  
  # renders player name box
  output$player_names_ui <- renderUI({
    req(input$players)  # only show if players are selected
    
    lapply(1:input$players, function(i) {
      textInput(
        inputId = paste0("player_name_", i),
        label = paste("Enter name for Player", i, "(or leave blank for random):"),
        value = ""
      )
    })
  })
  
  # next number logic
  observeEvent(input$next_num, {
    if (length(rv$numbers_left) == 0 || !is.null(rv$winner)) return()
    
    if (length(rv$boards) < rv$players) {
      showNotification("You need to start a new game first!", type = "error")
      return()
    }
    
    number_drawn <- sample(rv$numbers_left, 1)
    rv$numbers_left <- setdiff(rv$numbers_left, number_drawn)
    rv$move_count <- rv$move_count + 1
    rv$boards <- lapply(rv$boards, mark_card, number = number_drawn)
    
    # check for winner & update high score
    for (i in 1:rv$players) {
      if (check_for_bingo(rv$boards[[i]])) {
        rv$winner <- paste(rv$player_names[i], "wins in", rv$move_count, "moves!")
        
        # update high score after win
        if (is.null(rv$high_score) || rv$move_count < rv$high_score) {
          rv$high_score <- rv$move_count
        }
        break
      }
    }
    
    # renders number called
    output$called_number <- renderUI({
      HTML(paste0("<div style='font-weight: bold; font-size: 18px; margin-top: 14px;'>Last number called: ", number_drawn, "</div>"))
    })
    
    # warning message
    lapply(1:rv$players, function(i) {
      moves_left <- moves_till_bingo(rv$boards[[i]])
      showNotification(
        paste(rv$player_names[i], "is", moves_left, "move(s) away from Bingo!"),
        type = "warning",
        duration = 2
      )
    })
  })
  
  # renders winner text
  output$winner_text <- renderUI({
    req(rv$winner)
    HTML(paste0("<div style='margin-top: 8px; font-weight: bold; font-style: italic; font-size: 18px; color: #66B3FF;'>", rv$winner, "</div>"))
  })
  
  # renders high score
  output$high_score_text <- renderUI({
    if (!is.null(rv$high_score)) {
      HTML(paste0("<div style='margin-top: 8px; font-weight: bold; font-size: 18px; font-style: bold;'><strong>Session Best: </strong>" , rv$high_score, " moves</div>"))
    }
  })
  
  # move counter
  output$move_counter <- renderText({
    paste("Moves so far:", rv$move_count)
  })
  
  # numbers not drawn
  output$number_pool_left <- renderText({
    paste("Numbers remaining:", length(rv$numbers_left))
  })
  
  
  output$link <- renderText({ 
    format("https://github.com/jfleege") 
  }) |>
    bindEvent(input$update) 
  
  # renders bingo boards
  output$bingo_boards <- renderUI({
    req(rv$boards, rv$player_names)
    
    rows <- list()
    
    i <- 1
    while (i <= rv$players) {
      name1 <- if (!is.null(rv$player_names) && length(rv$player_names) >= i) {
        rv$player_names[i]
      } else {
        paste("Player", i)
      }
      
      panel1 <- column(5, wellPanel(
        h4(name1, style = "font-weight: bold; font-size: 20px;"),
        tableOutput(paste0("board", i))
      ))
      
      if (i + 1 <= rv$players) {
        name2 <- if (length(rv$player_names) >= i + 1) {
          rv$player_names[i + 1]
        } else {
          paste("Player", i + 1)
        }
        
        panel2 <- column(5, wellPanel(
          h4(name2, style = "font-weight: bold; font-size: 20px;"),
          tableOutput(paste0("board", i + 1))
        ))
        
        rows[[length(rows) + 1]] <- list(
          fluidRow(panel1, panel2),
          br()
        )
        i <- i + 2
      } else {
        rows[[length(rows) + 1]] <- list(
          fluidRow(panel1),
          br()
        )
        i <- i + 1
      }
    }
    
    tagList(rows)
  })
  
  
  
  
  observe({
    for (i in 1:rv$players) {
      local({
        idx <- i
        output[[paste0("board", idx)]] <- renderUI({
          req(length(rv$boards) >= idx)
          board <- rv$boards[[idx]]
          table_rows <- lapply(1:5, function(r) {
            tags$tr(
              lapply(1:5, function(c) {
                val <- board[r, c]
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
          tags$table(
            style = "border-collapse: collapse; margin-bottom: 30px;",
            tags$thead(
              tags$tr(lapply(c("B", "I", "N", "G", "O"), function(h) {
                tags$th(style = "border: 3px solid gray; padding: 10px; font-size: 18px; background-color: orange", h)
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

shinyApp(ui = ui, server = server)

