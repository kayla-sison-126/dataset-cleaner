# --------------------------
# STAT440: Final Project SP26
# Kayla Sison, netid: ksiso2
#
# DESCRIPTION:
# This shiny app allows users to input their own dataset
#  url and then displays the first few rows of the dataset 
#  as well as a summary and dimensions.
# It has various buttons to help modify the dataset: 
# - format date columns (if any)
# - remove duplicates
# - remove outliers (using IQR method)
# - rename columns
# - perform string operations (trim whitespace, change case, remove digits)
# It will also have a text input for users to run custom 
#  SQL queries on their dataset.
#
# The app uses a preview system: every operation stores its
#  result in pending_result rather than overwriting dataset immediately.
#  The user must explicitly apply or discard the preview before another
#  operation can run. This prevents accidental changes and lets users
#  compare the before and after.
#
# Users may download their modified dataset with the download button.
#
# --------------------------
# TEST LINK 1: CTA - https://uofi.box.com/shared/static/2nk4zg0jbanfr6vz5jmjli9hsdp61agj.csv
# TEST LINK 2: Food Inspections - https://uofi.box.com/shared/static/7w6748g7ks7yjz6udkac2xh4eay689ci.csv
# --------------------------

# LIBRARIES
library(tidyverse)
library(shiny)
library(shinyjs)
library(sqldf)
library(jsonlite)
library(openxlsx)
library(lubridate)
library(shinycssloaders) # for loading indicators
library(httr)            # for checking URL reachability before loading


# SHINY APP
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # import google fonts
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lora:ital,wght@0,400..700;1,400..700&family=Playfair+Display:ital,wght@0,400..900;1,400..900&display=swap"),
    tags$style(HTML("
      body { 
        background-color: #faf6f0; /* cream color */
        color: #3a2f28; /* brown color*/
        font-family: 'Lora', Georgia, serif; 
        font-size: 1.05rem;
      }
      
      /* headers */
      h2, h3, h4 { 
        font-family: 'Playfair Display', serif; 
        color: #2b201a; 
        font-weight: 700;
      }
      h2 { 
        font-size: 2.4rem; 
        color: #a04c36; /*rust color */
        border-bottom: 2px solid #e6dec9; 
        padding-bottom: 12px; 
        margin-top: 30px;
      }
      h4 { 
        color: #4a6042; /* sage green color */
        border-bottom: 1px solid #e6dec9; 
        padding-bottom: 6px; 
        margin-top: 28px; 
        font-size: 1.15rem;
      }
      
      /* sidebar/well */
      .well { 
        background: #f1ebd9!important; /* darker cream */
        border: 1px solid #dcd3be!important; 
        border-radius: 8px!important; 
        box-shadow: none!important; 
      }
      
      /*form stuff */
      .control-label { 
        font-size: 1.1rem!important;
        font-weight: 600!important; 
        color: #2b201a!important;
        margin-top: 10px;
      }
      .form-control, select, .selectize-input { 
        background-color: #faf6f0!important; 
        color: #3a2f28!important; 
        border: 1px solid #dcd3be!important; 
        border-radius: 4px!important; 
        font-family: 'Lora', serif;
      }
      .selectize-dropdown, .selectize-dropdown-content {
        background-color: #faf6f0!important;
        color: #3a2f28!important;
      }
      .help-block { 
        font-size: 1rem!important;
        color: #615246!important; 
        font-style: italic; 
        margin-top: 8px;
      }
      
      /* buttons */
      .btn { 
        font-family: 'Lora', serif;
        border-radius: 4px; 
        font-weight: 500; 
        border: 1px solid #dcd3be; 
        background: #faf6f0; 
        color: #3a2f28; 
        transition: all 0.2s ease;
        margin-bottom: 6px;
      }
      .btn:hover { 
        background: #e6dec9; 
        color: #2b201a; 
        border-color: #cbbfa3;
      }
      
      /* more buttons (apply changes) */
      #load_data, #apply_changes, #download { 
        background: #526f4e; 
        color: #faf6f0; 
        border: 1px solid #41593e; 
      }
      #load_data:hover, #apply_changes:hover, #download:hover { 
        background: #41593e; 
      }
      
      /* more buttons (discard changes) */
      #discard_changes { 
        background: #b55138; 
        color: #faf6f0; 
        border: 1px solid #913f2a; 
      }
      #discard_changes:hover { 
        background: #913f2a; 
      }
      
      /* previews, output */
      .preview-box { 
        background: #fdf6f0; 
        border: 2px dashed #b55138; 
        border-radius: 8px; 
        padding: 20px; 
        margin-bottom: 25px; 
      }
      .table { 
        color: #3a2f28; 
        background: #faf6f0; 
        font-family: monospace;
        font-size: 0.9rem;
      }
      .table th { 
        border-bottom: 2px solid #e6dec9!important; 
        color: #526f4e; 
        font-family: 'Playfair Display', serif;
        font-size: 1rem;
      }
      .table td { 
        border-top: 1px solid #f1ebd9!important; 
      }
      pre { 
        background: #f1ebd9; 
        color: #3a2f28; 
        border: 1px solid #dcd3be; 
        border-radius: 6px; 
        font-family: monospace; 
        padding: 15px; 
      }
    "))
  ),
  
  
  
  titlePanel("Dataset Cleaner"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter dataset URL"),
      actionButton("load_data", "Load Data"),
      
      h4("Basic Tools"),
      actionButton("clean_dates", "Format Date Columns"),
      actionButton("remove_duplicates", "Remove Duplicates"),
      actionButton("remove_outliers", "Remove Outliers"),
      uiOutput("rename_ui"),
      actionButton("rename_col", "Rename Column"),
      
      h4("String Tools"),
      uiOutput("string_ui"),
      actionButton("clean_strings", "Apply String Operation"),
      
      h4("Developer Tools"),
      textInput("sql_query", "SQL Query"),
      actionButton("run_query","Run SQL Query"),
      helpText("Please reference your dataset as 'df' in your query. e.g. SELECT * FROM df WHERE col > 5"),
      
      downloadButton("download","Download Cleaned Data", style="margin-top: 20px; width: 100%;")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.has_pending",
        div(class = "preview-box",
            h3("Preview of Changes", style = "margin-top: 0; color: #b55138;"),
            actionButton("apply_changes", "Apply Changes"),
            actionButton("discard_changes", "Discard Changes"),
            div(textOutput("prev_dims"), style = "color: #615246; margin-bottom: 15px; font-style: italic;"),
            withSpinner(tableOutput("pending_preview"), type=6)
        )
      ),
      
      h3("Dataset (downloadable)"),
      div(textOutput("data_dims"), style = "color: #615246; margin-bottom: 15px; font-style: italic;"),
      h4("Dataset Head:"),
      withSpinner(tableOutput("data_head"), type=6),
      h4("Summary:"),
      withSpinner(verbatimTextOutput("data_summary"), type=6)
    )
  )
)


server <- function(input, output) {
  # dataset holds the committed/applied version of the data
  # pending_result holds a preview that the user must approve before it replaces dataset
  dataset <- reactiveVal(NULL)
  pending_result <- reactiveVal(NULL)
  
  # store dataset using proper function for file type
  observeEvent(input$load_data, { 
    req(input$url) #req requires that a value is available
    pending_result(NULL) # clear any old preview
    
    withProgress(message="Loading data...", value=0.5, {
      url <- input$url
      ext <- tools::file_ext(url) #get file extension
      
      tryCatch({
        # check URL is reachable before attempting to parse,
        # so we can give the user a clean error message instead of a cryptic R error
        httr::HEAD(url)
        
        df <- if (ext=="csv") {
          read_csv(url)
        }
        else if (ext=="tsv") {
          read_delim(url, delim="\t")
        }
        else if (ext=="json") {
          fromJSON(url)
        }
        else if (ext=="xlsx") {
          # openxlsx cannot read xlsx directly from a URL, so we
          # download to a temp file first
          tmp <- tempfile(fileext = ".xlsx")
          download.file(url, tmp, mode="wb", quiet=TRUE)
          read.xlsx(tmp)
        }
        else {
          stop("Unsupported file type :(") #stop basically raises an error
        }
        
        # validate df before assigning
        if (!is.data.frame(df)) {
          stop("Loaded data is not tabular. Check that your URL points to a structured dataset.")
        }
        if (nrow(df) == 0) {
          stop("Loaded dataset is empty.")
        }
        
        dataset(df)
        
      }, error=function(e) {
        showNotification(paste("Error loading data:", e$message), type="error")
      })
      
    })
  })
  
  
  # show/hide preview panel
  # outputOptions with suspendWhenHidden=FALSE is required so that the
  #  conditionalPanel in the UI can react to has_pending even before it
  #  has been rendered for the first time
  output$has_pending <- reactive({ !is.null(pending_result()) })
  outputOptions(output, "has_pending", suspendWhenHidden = FALSE)
  
  # render pending preview
  output$pending_preview <- renderTable({
    req(pending_result())
    head(pending_result())
  })
  
  # apply changes: promote pending_result to dataset and clear preview
  observeEvent(input$apply_changes, {
    req(pending_result())
    dataset(pending_result()) #make pending result the new dataset
    pending_result(NULL)      #clear pending_result
    showNotification("Changes applied.", type = "message")
  })
  
  # discard changes: simply clear pending_result, dataset is unchanged
  observeEvent(input$discard_changes, {
    pending_result(NULL) #clear pending_result
    showNotification("Changes discarded.", type = "message")
  })
  
  
  # display the dimensions of the data
  output$data_dims <- renderText({
    req(dataset())
    paste("Dimensions:", nrow(dataset()), "rows x", ncol(dataset()), "columns")
  })
  
  # display the dimensions of the PREVIEW data
  output$prev_dims <- renderText({
    req(pending_result())
    paste("Preview Dimensions:", nrow(pending_result()), "rows x", ncol(pending_result()), "columns")
  })
  
  # display the first few rows of the dataset
  output$data_head <- renderTable({
    req(dataset())
    head(dataset())
  })
  
  # display a summary of the dataset
  output$data_summary <- renderPrint({
    req(dataset())
    summary(dataset())
  })
  
  
  # handle downloading functionality
  output$download <- downloadHandler(
    filename = function() {
      paste("cleaned_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(dataset())
      write_csv(dataset(), file)
    }
  )
  # make sure download doesn't work when dataset not yet loaded
  observe({
    toggleState("download", condition=!is.null(dataset()))
  })
  
  
  # Clean date columns when the button is clicked
  #  only convert columns that are character type: avoids re-parsing
  #  columns that are already numeric or date objects.
  #  any(!is.na(parsed_date)) ensures we only overwrite if at least
  #  one value successfully parsed; otherwise we leave the column alone
  observeEvent(input$clean_dates, {
    req(dataset())
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    
    ord <- c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS") #some possible formats to try
    df <- dataset()
    
    for (col in colnames(df)) {
      if (is.character(df[[col]])) {
        tryCatch(
          {
            parsed_date <- parse_date_time(df[[col]], 
                                           orders = ord, #attempt all orders in ord
                                           quiet=TRUE #silence warnings
                                           )
            
            if (any(!is.na(parsed_date))) {
              df[[col]] <- parsed_date
            }
          }, 
          error=function(e) NULL
        )
      }
    }
    pending_result(df)
    showNotification("Preview ready: Date columns formatted.", type="message")
  })
  
  
  # Remove duplicates when the button is clicked
  # using sqldf here to demonstrate SQL within the app, but could also use distinct()
  observeEvent(input$remove_duplicates, {
    req(dataset())
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    
    df <- dataset()
    old <- nrow(dataset())
    df <- sqldf("SELECT DISTINCT * FROM df") # could use distinct() here
    new <- nrow(df)
    pending_result(df)
    showNotification(paste("Preview ready: Removed",old-new,"duplicate rows."), type="message")
  })
  
  
  # Remove outliers when the button is clicked:
  # uses the iqr method for all numeric columns
  #  note: filtering is applied sequentially, so row count can compound
  #  across columns; wide datasets with many numeric columns may lose more rows
  observeEvent(input$remove_outliers, {
    req(dataset())
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    
    df <- dataset()
    for (col in colnames(df)) {
      if (is.numeric(df[[col]])) {
        # algebra to get outlier bounds from the iqr:
        Q1 <- quantile(df[[col]], 0.25, na.rm=TRUE)
        Q3 <- quantile(df[[col]], 0.75, na.rm=TRUE)
        my_iqr <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * my_iqr
        upper_bound <- Q3 + 1.5 * my_iqr
        
        # filter out values past the outliers bounds
        df <- df %>% filter((.data[[col]] >= lower_bound) & (.data[[col]] <= upper_bound))
      }
    }
    removed <- nrow(dataset()) - nrow(df) # count outliers
    if (nrow(df) == 0) {
      showNotification("Warning: result is empty. Discarding.", type="warning")
      return()
    }
    pending_result(df)
    showNotification(paste("Preview ready:", removed, "rows with outliers removed."), type="message")
  })
  
  # UI for renaming columns
  output$rename_ui <- renderUI({
    req(dataset())
    tagList(
      selectInput("col_to_rename", "Select column to rename:", choices=colnames(dataset())),
      textInput("new_col_name", "Enter new column name:")
    )
  })
  
  # Rename column when the button is clicked
  observeEvent(input$rename_col, {
    req(dataset(), input$col_to_rename, input$new_col_name)
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    if (trimws(input$new_col_name) == "") {
      showNotification("New column name cannot be empty.", type="warning")
      return()
    }
    if (input$new_col_name %in% colnames(dataset())) {
      showNotification("Column name already exists. Please choose a different name.", type="warning")
      return()
    }
    df <- dataset()
    
    # rename the selected column, stripping surrounding whitespace from the new name
    colnames(df)[colnames(df) == input$col_to_rename] <- trimws(input$new_col_name) # rename col, removing surrounding whitespace
    pending_result(df)
    showNotification(paste("Preview ready: Renamed column to", input$new_col_name), type="message")
  })
  
  
  # UI for string cleaning: shows a character column picker and an operation dropdown.
  # Only character columns are offered since string operations don't apply to numeric/date types.
  output$string_ui <- renderUI({
    req(dataset())
    char_cols <- colnames(dataset())[sapply(dataset(), is.character)]
    if (length(char_cols) == 0) {
      return(helpText("No character columns found in dataset."))
    }
    tagList(
      selectInput("string_col", "Select column:", choices=char_cols),
      selectInput("string_op", "Operation:", choices=c(
        "Trim whitespace"  = "trim",
        "To uppercase"     = "upper",
        "To lowercase"     = "lower",
        "Remove digits"    = "remove_digits"
      ))
    )
  })
  
  # Apply the selected string operation to the selected column.
  # Uses stringr functions from tidyverse (str_squish, str_remove_all)
  # and base R (toupper, tolower).
  observeEvent(input$clean_strings, {
    req(dataset(), input$string_col, input$string_op)
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    
    df <- dataset()
    col <- input$string_col
    
    df[[col]] <- switch(input$string_op,
                        trim          = str_squish(df[[col]]),       # remove leading/trailing/extra whitespace
                        upper         = toupper(df[[col]]),           # convert to uppercase
                        lower         = tolower(df[[col]]),           # convert to lowercase
                        remove_digits = str_remove_all(df[[col]], "\\d") # remove all digit characters
    )
    
    pending_result(df)
    showNotification(paste("Preview ready: String operation applied to", col), type="message")
  })
  
  
  # Run SQL query when the button is clicked
  observeEvent(input$run_query, {
    req(dataset(), input$sql_query)
    if (!is.null(pending_result())) {
      showNotification("Please apply or discard the current preview first.", type = "warning")
      return()
    }
    if (trimws(input$sql_query)=="") {
      showNotification("SQL query cannot be empty.", type="warning")
      return()
    }
    tryCatch(
      {
        df <- dataset()
        result <- sqldf(input$sql_query) # run the query
        if (nrow(result) == 0) {
          showNotification("Warning: result is empty. Discarding.", type="warning")
          return()
        }
        pending_result(result) # store result in pending
        showNotification("Preview ready: SQL query applied.", type="message")
      },
      error=function(e) {
        showNotification(paste("Error in SQL query:", e$message), type="error")
      }
    )
  })
}

shinyApp(ui = ui, server = server)


