library(shiny)
library(readxl)
library(dplyr)
library(writexl)


# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  titlePanel("Text Segmentation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ---------------------------------------------------
      # Person's name
      # ---------------------------------------------------
      
      textInput(
        "person_name",
        "Your name:",
        value = ""
      ),
      
      # ---------------------------------------------------
      # Upload original Excel file
      # ---------------------------------------------------
      
      fileInput(
        "file",
        "Upload Excel file",
        accept = c(".xlsx", ".xls")
      ),
      
      hr(),
      
      # ---------------------------------------------------
      # Navigation
      # ---------------------------------------------------
      
      actionButton(
        "prev_response",
        "← Previous"
      ),
      
      actionButton(
        "next_response",
        "Next →"
      ),
      
      br(),
      br(),
      
      # ---------------------------------------------------
      # Reset
      # ---------------------------------------------------
      
      actionButton(
        "reset_response",
        "Reset this response"
      ),
      
      hr(),
      
      # ---------------------------------------------------
      # Download
      # ---------------------------------------------------
      
      downloadButton(
        "download",
        "Download segmented file"
      )
    ),
    
    mainPanel(
      
      h3("Current response"),
      
      uiOutput("response_number"),
      
      uiOutput("response_id"),
      
      uiOutput("text_to_segment"),
      
      hr(),
      
      h3("Current segmentation"),
      
      tableOutput("segments")
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  
  # -------------------------------------------------------
  # Clean the person's name
  # -------------------------------------------------------
  
  clean_name <- reactive({
    
    req(input$person_name)
    
    name <- trimws(input$person_name)
    
    # Replace spaces and other non-alphanumeric
    # characters with underscores
    
    name <- gsub(
      "[^A-Za-z0-9]+",
      "_",
      name
    )
    
    # Remove underscores at the beginning/end
    
    name <- gsub(
      "^_+|_+$",
      "",
      name
    )
    
    name
  })
  
  
  # -------------------------------------------------------
  # Read uploaded Excel file
  # -------------------------------------------------------
  
  data <- reactive({
    
    req(input$file)
    
    # Require a name before processing data
    
    validate(
      need(
        nzchar(trimws(input$person_name)),
        "Please enter your name first."
      )
    )
    
    read_excel(input$file$datapath) %>%
      select(Response_ID, Text) %>%
      mutate(
        Response_ID = as.character(Response_ID),
        Text = as.character(Text)
      )
  })
  
  
  # -------------------------------------------------------
  # Which response are we currently looking at?
  # -------------------------------------------------------
  
  current_response <- reactiveVal(1)
  
  
  # -------------------------------------------------------
  # Move to next response
  # -------------------------------------------------------
  
  observeEvent(input$next_response, {
    
    req(data())
    
    current <- current_response()
    
    if (current < nrow(data())) {
      current_response(current + 1)
    }
  })
  
  
  # -------------------------------------------------------
  # Move to previous response
  # -------------------------------------------------------
  
  observeEvent(input$prev_response, {
    
    req(data())
    
    current <- current_response()
    
    if (current > 1) {
      current_response(current - 1)
    }
  })
  
  
  # -------------------------------------------------------
  # Store segmentation boundaries
  #
  # For each response we store the word numbers after
  # which a segment should end.
  # -------------------------------------------------------
  
  boundaries <- reactiveVal(list())
  
  
  # -------------------------------------------------------
  # Reset current response
  # -------------------------------------------------------
  
  observeEvent(input$reset_response, {
    
    req(data())
    
    all_boundaries <- boundaries()
    
    response_key <- as.character(current_response())
    
    all_boundaries[[response_key]] <- integer(0)
    
    boundaries(all_boundaries)
  })
  
  
  # -------------------------------------------------------
  # Response number
  # -------------------------------------------------------
  
  output$response_number <- renderUI({
    
    req(data())
    
    current <- current_response()
    
    total <- nrow(data())
    
    h4(
      paste0(
        "Response ",
        current,
        " of ",
        total
      )
    )
  })
  
  
  # -------------------------------------------------------
  # Response ID
  # -------------------------------------------------------
  
  output$response_id <- renderUI({
    
    req(data())
    
    row <- data()[current_response(), ]
    
    h4(
      paste0(
        "Response_ID: ",
        row$Response_ID
      )
    )
  })
  
  
  # -------------------------------------------------------
  # Display clickable words
  # -------------------------------------------------------
  
  output$text_to_segment <- renderUI({
    
    req(data())
    
    text <- data()$Text[current_response()]
    
    words <- strsplit(
      text,
      "\\s+"
    )[[1]]
    
    response_key <- as.character(
      current_response()
    )
    
    current_boundaries <- boundaries()[[response_key]]
    
    if (is.null(current_boundaries)) {
      current_boundaries <- integer(0)
    }
    
    
    # -----------------------------------------------------
    # Create one clickable button for each word
    # -----------------------------------------------------
    
    word_buttons <- lapply(
      seq_along(words),
      function(i) {
        
        # Blue when this word is the end of a segment
        
        if (i %in% current_boundaries) {
          
          button_style <- paste0(
            "background-color:#0d6efd;",
            "color:white;"
          )
          
        } else {
          
          button_style <- paste0(
            "background-color:#eeeeee;",
            "color:black;"
          )
        }
        
        
        tags$button(
          
          type = "button",
          
          class = "word-button",
          
          style = paste0(
            "margin-right:3px;",
            "margin-bottom:5px;",
            "border:none;",
            "border-radius:4px;",
            "padding:6px 9px;",
            "cursor:pointer;",
            button_style
          ),
          
          onclick = sprintf(
            "Shiny.setInputValue(
               'word_clicked',
               {index:%d, nonce:Date.now()}
             );",
            i
          ),
          
          words[i]
        )
      }
    )
    
    
    tagList(
      
      tags$p(
        "Click a word to make it the end of a segment.",
        style = "color:#666666;"
      ),
      
      do.call(
        tagList,
        word_buttons
      )
    )
  })
  
  
  # -------------------------------------------------------
  # Respond to word clicks
  # -------------------------------------------------------
  
  observeEvent(input$word_clicked, {
    
    req(data())
    
    # Which word was clicked?
    
    i <- input$word_clicked$index
    
    all_boundaries <- boundaries()
    
    response_key <- as.character(
      current_response()
    )
    
    current <- all_boundaries[[response_key]]
    
    if (is.null(current)) {
      current <- integer(0)
    }
    
    
    # Toggle the boundary
    
    if (i %in% current) {
      
      current <- current[
        current != i
      ]
      
    } else {
      
      current <- sort(
        c(current, i)
      )
    }
    
    
    all_boundaries[[response_key]] <- current
    
    boundaries(all_boundaries)
  })
  
  
  # -------------------------------------------------------
  # Create segmented data
  # -------------------------------------------------------
  
  segmented_data <- reactive({
    
    req(data())
    
    
    result <- list()
    
    
    for (r in seq_len(nrow(data()))) {
      
      text <- data()$Text[r]
      
      response_id <- data()$Response_ID[r]
      
      words <- strsplit(
        text,
        "\\s+"
      )[[1]]
      
      cuts <- boundaries()[[as.character(r)]]
      
      if (is.null(cuts)) {
        cuts <- integer(0)
      }
      
      
      # The final word always ends the final segment
      
      cuts <- sort(
        unique(
          c(
            cuts,
            length(words)
          )
        )
      )
      
      
      start <- 1
      
      split_id <- 1
      
      
      for (end in cuts) {
        
        # Safety check
        
        if (start <= end) {
          
          segment <- paste(
            words[start:end],
            collapse = " "
          )
          
          result[[length(result) + 1]] <- data.frame(
            
            Response_ID = response_id,
            
            Split_ID = split_id,
            
            Text = segment,
            
            stringsAsFactors = FALSE
          )
          
          start <- end + 1
          
          split_id <- split_id + 1
        }
      }
    }
    
    
    bind_rows(result)
  })
  
  
  # -------------------------------------------------------
  # Show current segmentation
  # -------------------------------------------------------
  
  output$segments <- renderTable({
    
    req(data())
    
    segmented_data() %>%
      filter(
        Response_ID ==
          data()$Response_ID[
            current_response()
          ]
      )
  })
  
  
  # -------------------------------------------------------
  # Download segmented data
  # -------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      
      name <- clean_name()
      
      if (name == "") {
        name <- "unnamed"
      }
      
      paste0(
        "segmented_data_",
        name,
        ".xlsx"
      )
    },
    
    
    content = function(file) {
      
      write_xlsx(
        segmented_data(),
        file
      )
    }
  )
}


# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)