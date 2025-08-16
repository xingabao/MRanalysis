# Load R packages
suppressMessages(suppressWarnings(library(gdata)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinyjs)))

# Define file path
temp_dir <- MRanalysisBase::MRtempdir()
file_path <- file.path(temp_dir, 'OpenGWAS-JWT.txt')
MRanalysisBase::create_file_dir(file_path)

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

# UI section
ui <- fluidPage(
  tags$head(
    useShinyjs(),
    tags$link(rel = 'shortcut icon', href = '/XINGABAO/img/favicon.ico'),
    MRanalysisBase::web.statistic.baidu
  ),
  titlePanel("OpenGWAS Token"),
  br(),
  sidebarLayout(
    tags$div(
      id = 'new_api_key_div',
      sidebarPanel(
        textAreaInput("new_api_key", "Enter New OpenGWAS JSON Web Token (JWT)", "", width = '100%', rows = 3),
        p("Please visit ", a("https://api.opengwas.io/", href = "https://api.opengwas.io/", target = "_blank"),
          " to register and generate a new token, then copy the generated token here and submit."),
        actionButton("update_button", "Update Token")
      )
    ),
    mainPanel(
      h4("Current OpenGWAS Token:"),
      verbatimTextOutput("current_api_key"),
      br(),
      h4("JWT Valid Until:"),
      verbatimTextOutput("jwt_valid_until_output")
    )
  ),
  # Add CSS style for text wrapping
  tags$head(
    tags$style(HTML("
      pre {
        white-space: pre-wrap;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }
    "))
  )
)

# Server section
server <- function(input, output, session) {
  # Initially read the API key from file
  initial_key <- if (file.exists(file_path)) {
    readLines(file_path, warn = FALSE)[1]
  } else {
    if (MRanalysisBase::OpenGWAS.token() != '') {
      MRanalysisBase::OpenGWAS.token()
    } else {
      "No API Key found. Please update."
    }
  }

  # Set initial environment variable
  Sys.setenv(OPENGWAS_JWT = initial_key)

  # Use reactiveVal to store the current API key
  current_key <- reactiveVal(initial_key)

  # Detect root URL using session$clientData
  root_url <- reactive({
    client_data <- session$clientData
    # Construct the root URL from protocol, hostname, and port
    root <- paste0(client_data$url_protocol, "//", client_data$url_hostname, ifelse(client_data$url_port == "", "", paste0(":", client_data$url_port)))
    return(root)
  })

  # Conditionally disable textAreaInput based on root URL containing "mranalysis.cn"
  observe({
    url <- root_url()
    if (grepl(MRanalysisBase::MR.DOMIAN, url, ignore.case = TRUE)) {
      runjs("$('#new_api_key').prop('disabled', true);")
      runjs("$('#update_button').prop('disabled', true);")
      shiny::removeUI(selector = '#new_api_key_div', immediate = TRUE)
    } else {
      runjs("$('#new_api_key').prop('disabled', false);")
      runjs("$('#update_button').prop('disabled', false);")
    }
    
    if (Sys.getenv('XINGABAO') == 'ONLINE') {
      runjs("$('#new_api_key').prop('disabled', true);")
      runjs("$('#update_button').prop('disabled', true);")
      shiny::removeUI(selector = '#new_api_key_div', immediate = TRUE)
    }
  })

  # Dynamically retrieve jwt_valid_until value
  jwt_valid_until_val <- reactive({
    current_key()  # Depend on API Key changes
    result <- tryCatch({
      user_info <- suppressMessages(suppressWarnings(ieugwasr::user()))
      # user_info <- list()
      # user_info$user <- list()
      # user_info$user$jwt_valid_until <- '2000-00-00 00:00:00 UTC'
      if (!is.null(user_info$user$jwt_valid_until)) {
        user_info$user$jwt_valid_until
      } else {
        "JWT validity date not available."
      }
    }, error = function(e) {
      paste("Error retrieving JWT validity date:", e$message)
    })
    result
  })

  # Display current API key with partial masking
  output$current_api_key <- renderText({
    key <- current_key()
    if (nchar(key) > 300) {
      paste0(substr(key, 1, 150), "   ...   ", substr(key, nchar(key) - 149, nchar(key)))
    } else {
      key
    }
  })

  # Display jwt_valid_until
  output$jwt_valid_until_output <- renderText({
    jwt_valid_until_val()
  })

  # Listen for update button
  observeEvent(input$update_button, {
    url <- root_url()
    new_key <- input$new_api_key
    if (nchar(trim(new_key)) > 0 && !grepl(MRanalysisBase::MR.DOMIAN, url, ignore.case = TRUE)) {
      writeLines(new_key, file_path)
      current_key(new_key)
      Sys.setenv(OPENGWAS_JWT = new_key)  # Update environment variable
      MRanalysisBase::set_env_in_renviron(var = 'OPENGWAS_JWT', value = new_key, force = TRUE)
      updateTextInput(session, "new_api_key", value = "")
      showNotification("API Key updated successfully!", type = "message")
    } else {
      showNotification("Please enter a valid API Key.", type = "error")
    }
  })

  # Display root URL
  output$root_url_output <- renderText({
    root_url()
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server, options = list(port = 80))