library(shiny)
library(shinyjs)
library(later)
library(data.table)

# Load the GameEnv class
source("../R/game_env.R")

# Load your two Q‐tables
Q1 <- readRDS("../data/Q1_selfplay_double.rds")
Q2 <- readRDS("../data/Q2_selfplay_double.rds")

# Reconstruct the greedy policy = argmax_a [Q1 + Q2]
combined <- merge(
  Q1[, .(State, Action, V1 = Value)],
  Q2[, .(State, Action, V2 = Value)],
  by = c("State","Action"), all = TRUE
)
combined[is.na(V1), V1 := 1.0]
combined[is.na(V2), V2 := 1.0]
combined[, Combined := V1 + V2]

policy_dt <- combined[
  , .SD[which.max(Combined)], by = State,
  .SDcols = c("State","Action")
]
setnames(policy_dt, "Action", "BestAction")
policy_vec <- setNames(as.character(policy_dt$BestAction), policy_dt$State)


ui <- fluidPage(
  useShinyjs(),
  titlePanel("5×5 Marble Duel"),
  sidebarLayout(
    sidebarPanel(
      selectInput("first_player", "Who starts?", 
                  choices = c("You"="Human", "Agent"="Agent")),
      actionButton("newgame", "New Game"),
      br(), br(),
      actionButton("up",    "↑", width="60px"),
      actionButton("down",  "↓", width="60px"),
      actionButton("left",  "←", width="60px"),
      actionButton("right", "→", width="60px")
    ),
    mainPanel(
      uiOutput("grid")
    )
  )
)

server <- function(input, output, session) {
  # Print any unhandled errors to the R console
  options(shiny.error = function() {
    err <- geterrmessage()
    message("Shiny error: ", err)
  })
  
  # 1) Reactive container for the game state
  env <- reactiveVal(NULL)
  
  # 2) Top‐level renderUI:
  output$grid <- renderUI({
    e <- env()
    if (is.null(e)) {
      # show an empty placeholder before “New Game”
      return(tags$div("Click New Game to start"))
    }
    # build the 5×5 grid
    tagList(
      lapply(1:5, function(r) {
        tags$div(style="display:flex;",
                 lapply(1:5, function(c) {
                   idx <- (r-1)*5 + c
                   base <- "width:50px;height:50px;border:1px solid #000;text-align:center;line-height:50px;"
                   if (idx == e$pos) {
                     tags$div(style=paste0(base, "background:blue;color:white;"), "M")
                   } else if (e$mask[idx]) {
                     tags$div(style=paste0(base, "background:gray;"), "")
                   } else {
                     tags$div(style=base, "")
                   }
                 })
        )
      })
    )
  }) 

    
  # 3) Disable/enable buttons based on valid moves
  valid_moves <- reactive({
    e <- env()
    if (is.null(e)) return(rep(FALSE,4))
    e$valid_moves()
  })
  observe({
    vm <- valid_moves()
    shinyjs::toggleState("up",    vm[1])
    shinyjs::toggleState("down",  vm[2])
    shinyjs::toggleState("left",  vm[3])
    shinyjs::toggleState("right", vm[4])
  })
  
  # 4) Game initializer
  observeEvent(input$newgame, {
    e <- GameEnv$new()
    e$reset()
    env(e)
    # If agent starts:
    if (input$first_player == "Agent") {
      later(agent_move, 0.3)
    }
  })
  
  # 5) Core move logic
  do_move <- function(action) {
    tryCatch({
      e   <- isolate(env())        # ← keep isolate on the read
      res <- e$step(action)
      
      env(e)                       # ← WRITE **without** isolate  ✅
      
      if (res$done) {
        winner <- if (e$player == -1) "You" else "Agent"
        showModal(modalDialog(title = "Game Over",
                              paste(winner, "win!")))
      } else {
        turn_is_agent <- isolate(
          (input$first_player == "Human" && e$player == -1) ||
            (input$first_player == "Agent" && e$player ==  1)
        )
        if (turn_is_agent) later(agent_move, 0.3)
      }
    }, error = function(err) {
      message("💥 Error in do_move(): ", err$message)
      showModal(modalDialog(
        title = "Internal Error",
        paste("Something went wrong:", err$message),
        easyClose = TRUE
      ))
    })
  }
  
  
  
  # 6) Human button handlers
  observeEvent(input$up,    do_move(1))
  observeEvent(input$down,  do_move(2))
  observeEvent(input$left,  do_move(3))
  observeEvent(input$right, do_move(4))
  
  
  
  # 7) Agent’s turn (greedy policy)
  agent_move <- function() {
    tryCatch({
      e  <- isolate(env())             # <- isolate reactive read
      st <- e$get_state()
      act <- policy_vec[st]
      if (is.na(act)) {
        act <- sample(which(e$valid_moves()), 1)
      } else {
        act <- as.integer(act)
      }
      do_move(act)
    }, error = function(err) {
      message("💥 Error in agent_move(): ", err$message)
    })
  }
}  

shinyApp(ui, server)
