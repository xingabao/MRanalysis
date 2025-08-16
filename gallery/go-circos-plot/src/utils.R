#' Title Connect the id with element
#'
#' @param id 
#' @param elem 
#'
#' @return
#' @export
#'
#' @examples
connect.id <- function(id, elem) {
  if (id == '') {
    return(elem)
  } else {
    return(paste0(id, '_', elem))
  }
}


#' Title
#'
#' @param id 
#' @param globalenv 
#' @param packages 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
import_globalenv_ui_demo <- function (id, globalenv = TRUE, packages = get_data_packages(), title = TRUE) {
  ns <- NS(id)
  choices <- list()
  if (isTRUE(globalenv)) {
    choices <- append(choices, "Use Demo Data")
  }
  if (!is.null(packages)) {
    choices <- append(choices, list(Packages = as.character(packages)))
  }
  if (isTRUE(globalenv)) {
    selected <- "Use Demo Data"
  } else {
    selected <- packages[1]
  }
  if (isTRUE(title)) {
    title <- tags$h4(i18n("Import a dataset from an environment"), class = "datamods-title")
  }
  tags$div(class = "datamods-import", datamods:::html_dependency_datamods(), title, 
           pickerInput(inputId = ns("data"), label = i18n("Select a data.frame:"), choices = NULL, options = list(title = i18n("List of data.frame...")), width = "100%"),
           pickerInput(inputId = ns("env"), label = i18n("Select an environment in which to search:"), choices = choices, selected = selected, width = "100%", options = list(title = i18n("Select environment"), `live-search` = TRUE, size = 10)),
           tags$div(id = ns("import-placeholder"), shinyWidgets::alert(id = ns("import-result"), status = "info", tags$b(i18n("No data selected!")), i18n("Use a data.frame from your environment or from the environment of a package."), dismissible = TRUE)),
           uiOutput(outputId = ns("container_valid_btn"),  style = "margin-top: 20px;"))
}


#' Title
#'
#' @param id 
#' @param from 
#' @param file_extensions 
#' @param packages 
#'
#' @return
#' @export
#'
#' @examples
import_ui_demo <- function (id, from = c("env", "file", "copypaste", "googlesheets", "url"), file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"), packages = NULL) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)
  env <- if ("env" %in% from) { tabPanelBody(value = "env", tags$br(), import_globalenv_ui_demo(id = ns("env"), title = NULL, packages = packages)) }
  file <- if ("file" %in% from) { tabPanelBody(value = "file", tags$br(), import_file_ui(id = ns("file"), title = NULL, file_extensions = file_extensions)) }
  copypaste <- if ("copypaste" %in% from) { tabPanelBody(value = "copypaste", tags$br(), import_copypaste_ui(id = ns("copypaste"), title = NULL)) }
  googlesheets <- if ("googlesheets" %in% from) { tabPanelBody(value = "googlesheets", tags$br(), import_googlesheets_ui(id = ns("googlesheets"), title = NULL)) }
  url <- if ("url" %in% from) { tabPanelBody(value = "url", tags$br(), datamods::import_url_ui(id = ns("url"), title = NULL)) }
  labsImport <- list(env = i18n("Demo Data"), file = i18n("External file"), copypaste = i18n("Copy / Paste"), googlesheets = i18n("Googlesheets"), url = i18n("URL"))
  iconsImport <- list(env = phosphoricons::ph("code", title = labsImport$env), 
                      file = phosphoricons::ph("file-arrow-down", title = labsImport$file),
                      copypaste = phosphoricons::ph("clipboard-text", title = labsImport$copypaste), 
                      googlesheets = phosphoricons::ph("cloud-arrow-down", title = labsImport$googlesheets), 
                      url = phosphoricons::ph("link", title = labsImport$url))
  if (identical(length(from), 1L)) {
    importTab <- switch(from, env = import_globalenv_ui_demo(id = ns("env"), packages = packages), 
                        file = datamods::import_file_ui(id = ns("file"), file_extensions = file_extensions),  copypaste = import_copypaste_ui(id = ns("copypaste")), googlesheets = import_googlesheets_ui(id = ns("googlesheets")), url = import_url_ui(id = ns("url")), )
  } else {
    tabsetPanelArgs <- datamods:::dropNulls(list(env, file, copypaste, googlesheets, url, id = ns("tabs-import"), type = "hidden"))
    importTab <- do.call(what = tabsetPanel, args = tabsetPanelArgs)
    importTab <- fluidRow(column(width = 3, tags$br(), tags$style(HTML(sprintf("#%s>.btn-group-vertical {width: 100%%;}", ns("from"))), HTML(sprintf(".btn-group-vertical>.btn-group>.btn {text-align: left;}"))),
                                 radioGroupButtons(inputId = ns("from"), label = i18n("How to import data?"), 
                                                   choiceValues = from, choiceNames = lapply(X = from, FUN = function(x) { tagList(iconsImport[[x]], labsImport[[x]]) }), direction = "vertical", width = "100%")), column(width = 9, importTab))
  }
  tags$div(class = "datamods-imports", 
           datamods:::html_dependency_datamods(), 
           tabsetPanel(type = "tabs", id = ns("tabs-mode"), 
                       tabPanel(title = tagList(phosphoricons::ph("download-simple", title = i18n("Import")), i18n("Import")), value = "import", importTab),
                       tabPanel(title = tagList(phosphoricons::ph("table", title = i18n("View")), i18n("View")), value = "view", tags$br(), reactable::reactableOutput(outputId = ns("view"))), 
                       tabPanel(title = tagList(phosphoricons::ph("gear-six", title = i18n("Update")), i18n("Update")), value = "update", tags$br(), update_variables_ui(id = ns("update"), title = NULL)), 
                       tabPanel(title = tagList(phosphoricons::ph("shield-check", title = i18n("Validate")), i18n("Validate")), value = "validate", tags$br(), validation_ui(id = ns("validation"), display = "inline", max_height = "240px")),
                       tabPanel(title = tagList(phosphoricons::ph("shield-check", title = i18n("Filter")), i18n("Filter")), value = "filter", tags$br(), filter_data_ui(id = ns("filter"), max_height = "240px"))), 
  
           tags$div(id = ns("confirm-button"), style = "margin-top: 20px;", datamods:::button_import(list(ns = ns))), 
           tags$div(style = "display: none;", textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())), 
           tags$script(sprintf("$('#%s').addClass('nav-justified');", ns("tabs-mode")),
                       sprintf("fadeTab({id: '%s'});", ns("tabs-mode")),
                       sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "view"),
                       sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "update"), 
                       sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "validate"),
                       sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "filter")))
}


#' Title
#'
#' @param id 
#' @param from 
#' @param file_extensions 
#' @param packages 
#'
#' @return
#' @export
#'
#' @examples
import_ui_el <- function (id, from = c("env", "file"), file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"), packages = NULL) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)
  env <- if ("env" %in% from) { tabPanelBody(value = "env", tags$br(), import_globalenv_ui_demo(id = ns("env"), title = NULL, packages = packages)) }
  file <- if ("file" %in% from) { tabPanelBody(value = "file", tags$br(), import_file_ui(id = ns("file"), title = NULL, file_extensions = file_extensions, preview_data = FALSE)) }
  labsImport <- list(env = i18n("Demo Data"), file = i18n("External file"))
  iconsImport <- list(env = phosphoricons::ph("code", title = labsImport$env), 
                      file = phosphoricons::ph("file-arrow-down", title = labsImport$file))
  if (identical(length(from), 1L)) {
    importTab <- switch(from, env = import_globalenv_ui_demo(id = ns("env"), packages = packages), 
                        file = datamods::import_file_ui(id = ns("file"), file_extensions = file_extensions, preview_data = FALSE))
  } else {
    tabsetPanelArgs <- datamods:::dropNulls(list(env, file, id = ns("tabs-import"), type = "hidden"))
    importTab <- do.call(what = tabsetPanel, args = tabsetPanelArgs)
    importTab <- fluidRow(column(width = 3, tags$br(), tags$style(HTML(sprintf("#%s>.btn-group-vertical {width: 100%%;}", ns("from"))), HTML(sprintf(".btn-group-vertical>.btn-group>.btn {text-align: left;}"))),
                                 radioGroupButtons(inputId = ns("from"), label = i18n("How to import data?"), 
                                                   choiceValues = from, choiceNames = lapply(X = from, FUN = function(x) { tagList(iconsImport[[x]], labsImport[[x]]) }), direction = "vertical", width = "100%")), column(width = 9, importTab))
  }
  tags$div(class = "datamods-imports", 
           datamods:::html_dependency_datamods(), 
           tabsetPanel(type = "tabs", id = ns("tabs-mode"), 
                       tabPanel(title = NULL, value = "import", importTab)), 
           
           tags$div(id = ns("confirm-button"), style = "margin-top: 20px;", datamods:::button_import(list(ns = ns))), 
           tags$div(style = "display: none;", textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())), 
           tags$script(sprintf("$('#%s').addClass('nav-justified');", ns("tabs-mode")),
                       sprintf("fadeTab({id: '%s'});", ns("tabs-mode"))))
}

#' Title
#'
#' @param id 
#' @param choices 
#' @param btn_show_data 
#' @param show_data_in 
#' @param trigger_return 
#' @param return_class 
#' @param reset 
#'
#' @return
#' @export
#'
#' @examples
import_globalenv_server_demo <- function (id, choices, btn_show_data = TRUE, show_data_in = c("popup", "modal"), trigger_return = c("button", "change"), return_class = c("data.frame", "data.table", "tbl_df", "raw"), reset = reactive(NULL)) {
  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)
  module <- function(input, output, session) {
    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, 
                                   status = NULL)
    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })
    output$container_valid_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        datamods:::button_import()
      }
    })
    observeEvent(input$env, {
      if (identical(input$env, "Global Environment")) {
        choices <- choices      # choices <- datamods:::search_obj("data.frame")
      } else {
        choices <- choices      # choices <- datamods:::list_pkg_data(input$env)
      }
      if (is.null(choices)) {
        choices <- i18n("No data.frame here...")
        choicesOpt <- list(disabled = TRUE)
      } else {
        choicesOpt <- list(subtext = datamods:::get_dimensions(choices))
      }
      temporary_rv$package <- attr(choices, "package")
      updatePickerInput(session = session, inputId = "data", choices = choices, choicesOpt = choicesOpt)
    })
    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) { hideUI(selector = paste0("#", ns("container_valid_btn"))) }
    })
    observeEvent(input$data, {
      if (!isTruthy(input$data)) {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
        datamods:::insert_alert(selector = ns("import"), status = "info", tags$b(i18n("No data selected!")), i18n("Use a data.frame from your environment or from the environment of a package."))
      } else {
        name_df <- input$data
        if (!is.null(temporary_rv$package)) { attr(name_df, "package") <- temporary_rv$package }
        imported <- try(datamods:::get_env_data(name_df), silent = TRUE)
        if (inherits(imported, "try-error") || NROW(imported) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
          datamods:::insert_error(mssg = i18n(attr(imported, "condition")$message))
          temporary_rv$status <- "error"
          temporary_rv$data <- NULL
          temporary_rv$name <- NULL
        } else {
          datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
          datamods:::insert_alert(selector = ns("import"), status = "success", datamods:::make_success_alert(imported, trigger_return = trigger_return, btn_show_data = btn_show_data))
          pkg <- attr(name_df, "package")
          if (!is.null(pkg)) {
            name <- paste(pkg, input$data, sep = "::")
          } else {
            name <- input$data
          }
          name <- trimws(sub("\\(([^\\)]+)\\)", "", name))
          temporary_rv$status <- "success"
          temporary_rv$data <- imported
          temporary_rv$name <- name
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Imported data"), type = show_data_in)
    })
    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
    })
    if (identical(trigger_return, "button")) {
      return(list(status = reactive(temporary_rv$status), name = reactive(imported_rv$name), data = reactive(datamods:::as_out(imported_rv$data, return_class))))
    }
    else {
      return(list(status = reactive(temporary_rv$status), name = reactive(temporary_rv$name), data = reactive(datamods:::as_out(temporary_rv$data, return_class))))
    }
  }
  moduleServer(id = id, module = module)
}


#' Title
#'
#' @param id 
#' @param choices 
#' @param validation_opts 
#' @param allowed_status 
#' @param return_class 
#' @param read_fns 
#'
#' @return
#' @export
#'
#' @examples
import_server_el <- function(id, choices, validation_opts = NULL, allowed_status = c("OK", "Failed", "Error"), return_class = c("data.frame", "data.table", "tbl_df", "raw"), read_fns = list(), filter.type = c('virtualSelect', 'range', 'slider')) {
  allowed_status <- match.arg(allowed_status, several.ok = TRUE)
  return_class <- match.arg(return_class)
  if (length(read_fns) > 0) {
    if (!rlang::is_named(read_fns)) 
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    if (!all(vapply(read_fns, rlang::is_function, logical(1)))) 
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_rv <- reactiveValues(data = NULL)
    imported_rv <- reactiveValues(data = NULL)
    observeEvent(input$hidden, {
      data_rv$data <- NULL
      data_rv$name <- NULL
      if (length(validation_opts) < 1) {
        hideTab(inputId = "tabs-mode", target = "validate")
      }
    })
    observeEvent(input$from, {
      updateTabsetPanel(session = session, inputId = "tabs-import", selected = input$from)
    })
    from_env <- import_globalenv_server_demo(id = "env", choices = choices, trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_file <- import_file_server(id = "file", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden), read_fns = read_fns)
    from_copypaste <- import_copypaste_server(id = "copypaste", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_googlesheets <- import_googlesheets_server(id = "googlesheets", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_url <- import_url_server(id = "url", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    observeEvent(from_env$data(), { 
      data_rv$data <- from_env$data()
      data_rv$name <- from_env$name()
    })
    observeEvent(from_file$data(), {
      data_rv$data <- from_file$data()
      data_rv$name <- from_file$name()
    })
    observeEvent(from_copypaste$data(), {
      data_rv$data <- from_copypaste$data()
      data_rv$name <- from_copypaste$name()
    })
    observeEvent(from_googlesheets$data(), {
      data_rv$data <- from_googlesheets$data()
      data_rv$name <- from_googlesheets$name()
    })
    observeEvent(from_url$data(), {
      data_rv$data <- from_url$data()
      data_rv$name <- from_url$name()
    })
    observeEvent(data_rv$data, {
      req(data_rv$data)
      if (is.data.frame(data_rv$data)) {
        if (length(validation_opts) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
        } else {
          status <- validation_results$status()
          if (isTRUE(status %in% allowed_status)) {
            datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
          } else {
            datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
          }
        }
        datamods:::enable_tab("tabs-mode", "view")
        datamods:::enable_tab("tabs-mode", "update")
        datamods:::enable_tab("tabs-mode", "validate")
        datamods:::enable_tab("tabs-mode", "filter")
      }
      else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    output$view <- reactable::renderReactable({
      data <- req(data_rv$data)
      reactable::reactable(data, defaultColDef = reactable::colDef(header = function(value) {
        classes <- tags$div(style = "font-style: italic; font-weight: normal; font-size: small;", datamods:::get_classes(data[, value, drop = FALSE]))
        tags$div(title = value, value, classes)
      }), columns = list(), bordered = TRUE, compact = TRUE, 
      striped = TRUE)
    })
    updated_data <- update_variables_server(id = "update", data = reactive(data_rv$data), height = "300px")
    validation_results <- validation_server(id = "validation", data = reactive({ data_rv$data }), 
                                            n_row = validation_opts$n_row, n_col = validation_opts$n_col, 
                                            n_row_label = validation_opts$n_row_label %||% "Valid number of rows", 
                                            n_col_label = validation_opts$n_col_label %||% "Valid number of columns", 
                                            btn_label = validation_opts$btn_label, rules = validation_opts$rules)
    observeEvent(validation_results$status(), {
      status <- validation_results$status()
      req(status)
      if (status %in% c("Error", "Failed")) {
        datamods:::update_tab_label("tabs-mode", "validate", tagList(phosphoricons::ph("warning-circle", weight = "fill", fill = "firebrick"), i18n("Validate")))
      } else {
        datamods:::update_tab_label("tabs-mode", "validate", i18n("Validate"))
      }
      if (status %in% allowed_status) {
        datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
      } else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    
    observeEvent(updated_data(), { data_rv$data <- updated_data() })
    
    filter_data <- filter_data_server(
      id = "filter",
      data = reactive(data_rv$data),
      widget_char = filter.type[1],
      widget_num = filter.type[2],
      widget_date = filter.type[3],
      label_na = "NA",
      drop_ids = FALSE
    )
    
    observeEvent(input$confirm, {
      removeModal()
      imported_rv$data <- filter_data$filtered()
      imported_rv$code <- filter_data$code()
      imported_rv$name <- data_rv$name %||% "imported_data"
    })
    return(list(data = reactive(datamods:::as_out(imported_rv$data, return_class)), name = reactive(imported_rv$name), code = reactive(imported_rv$code)))
  })
}


#' Title
#'
#' @param id 
#' @param choices 
#' @param validation_opts 
#' @param allowed_status 
#' @param return_class 
#' @param read_fns 
#'
#' @return
#' @export
#'
#' @examples
import_server_demo <- function(id, choices, validation_opts = NULL, allowed_status = c("OK", "Failed", "Error"), return_class = c("data.frame", "data.table", "tbl_df", "raw"), read_fns = list(), filter.type = c('virtualSelect', 'range', 'slider')) {
  allowed_status <- match.arg(allowed_status, several.ok = TRUE)
  return_class <- match.arg(return_class)
  if (length(read_fns) > 0) {
    if (!rlang::is_named(read_fns)) 
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    if (!all(vapply(read_fns, rlang::is_function, logical(1)))) 
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
  }
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_rv <- reactiveValues(data = NULL)
    imported_rv <- reactiveValues(data = NULL)
    observeEvent(input$hidden, {
      data_rv$data <- NULL
      data_rv$name <- NULL
      if (length(validation_opts) < 1) {
        hideTab(inputId = "tabs-mode", target = "validate")
      }
    })
    observeEvent(input$from, {
      updateTabsetPanel(session = session, inputId = "tabs-import", selected = input$from)
    })
    from_env <- import_globalenv_server_demo(id = "env", choices = choices, trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_file <- import_file_server(id = "file", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden), read_fns = read_fns)
    from_copypaste <- import_copypaste_server(id = "copypaste", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_googlesheets <- import_googlesheets_server(id = "googlesheets", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    from_url <- import_url_server(id = "url", trigger_return = "change", btn_show_data = FALSE, reset = reactive(input$hidden))
    observeEvent(from_env$data(), { 
      data_rv$data <- from_env$data()
      data_rv$name <- from_env$name()
    })
    observeEvent(from_file$data(), {
      data_rv$data <- from_file$data()
      data_rv$name <- from_file$name()
    })
    observeEvent(from_copypaste$data(), {
      data_rv$data <- from_copypaste$data()
      data_rv$name <- from_copypaste$name()
    })
    observeEvent(from_googlesheets$data(), {
      data_rv$data <- from_googlesheets$data()
      data_rv$name <- from_googlesheets$name()
    })
    observeEvent(from_url$data(), {
      data_rv$data <- from_url$data()
      data_rv$name <- from_url$name()
    })
    observeEvent(data_rv$data, {
      req(data_rv$data)
      if (is.data.frame(data_rv$data)) {
        if (length(validation_opts) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
        } else {
          status <- validation_results$status()
          if (isTRUE(status %in% allowed_status)) {
            datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
          } else {
            datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
          }
        }
        datamods:::enable_tab("tabs-mode", "view")
        datamods:::enable_tab("tabs-mode", "update")
        datamods:::enable_tab("tabs-mode", "validate")
        datamods:::enable_tab("tabs-mode", "filter")
      }
      else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    output$view <- reactable::renderReactable({
      data <- req(data_rv$data)
      reactable::reactable(data, defaultColDef = reactable::colDef(header = function(value) {
        classes <- tags$div(style = "font-style: italic; font-weight: normal; font-size: small;", datamods:::get_classes(data[, value, drop = FALSE]))
        tags$div(title = value, value, classes)
      }), columns = list(), bordered = TRUE, compact = TRUE, 
      striped = TRUE)
    })
    updated_data <- update_variables_server(id = "update", data = reactive(data_rv$data), height = "300px")
    validation_results <- validation_server(id = "validation", data = reactive({ data_rv$data }), 
                                            n_row = validation_opts$n_row, n_col = validation_opts$n_col, 
                                            n_row_label = validation_opts$n_row_label %||% "Valid number of rows", 
                                            n_col_label = validation_opts$n_col_label %||% "Valid number of columns", 
                                            btn_label = validation_opts$btn_label, rules = validation_opts$rules)
    observeEvent(validation_results$status(), {
      status <- validation_results$status()
      req(status)
      if (status %in% c("Error", "Failed")) {
        datamods:::update_tab_label("tabs-mode", "validate", tagList(phosphoricons::ph("warning-circle", weight = "fill", fill = "firebrick"), i18n("Validate")))
      } else {
        datamods:::update_tab_label("tabs-mode", "validate", i18n("Validate"))
      }
      if (status %in% allowed_status) {
        datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
      } else {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
      }
    })
    
    observeEvent(updated_data(), { data_rv$data <- updated_data() })
    
    filter_data <- filter_data_server(
      id = "filter",
      data = reactive(data_rv$data),
      widget_char = filter.type[1],
      widget_num = filter.type[2],
      widget_date = filter.type[3],
      label_na = "NA",
      drop_ids = FALSE
    )

    observeEvent(input$confirm, {
      removeModal()
      imported_rv$data <- filter_data$filtered()
      imported_rv$code <- filter_data$code()
      imported_rv$name <- data_rv$name %||% "imported_data"
    })
    return(list(data = reactive(datamods:::as_out(imported_rv$data, return_class)), name = reactive(imported_rv$name), code = reactive(imported_rv$code)))
  })
}

#' Title
#'
#' @param vec 
#' @param keyword 
#'
#' @return
#' @export
#'
#' @examples
find.first <- function(vec, keyword) {
  for (str in vec) {
    if (length(keyword) == 1) {
      if (grepl(keyword, str, ignore.case = TRUE)) {
        return(str)
      }
    } else {
      if (grepl(keyword[1], str, ignore.case = TRUE) && grepl(keyword[2], str, ignore.case = TRUE)) {
        return(str)
      }
    }
  }
  return(NULL)
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
c.family <- function(x) {
  if (is.null(x)) { return(NA) }
  if (x == 'none') { return('') }
  return(x)
}

#' Title
#'
#' @param xs 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
c.palette <- function(xs, n = 12) {
  if (is.null(xs)) { return(a.palettes)}
  colurs <- c()
  for (x in xs) {
    if (is.null(x)) { colurs <- c(colurs, scales::hue_pal()(n)) }
    else if (is.na(x)) { colurs <- c(colurs, scales::hue_pal()(n)) }
    else if (x == 'default') { colurs <- c(colurs, scales::hue_pal()(n)) }
    else { colurs <- c(colurs, get(x)) }
  }
  return(colurs)
}

#' Title
#'
#' @param text 
#' @param px 
#' @param py 
#' @param component 
#'
#' @return
#' @export
#'
#' @examples
text.x.y <- function(text, px, py, component){
  if (is.null(text) && (is.null(px) || is.null(py))) {
    if (component %in% c('position')) {
      val <- 'right'
    } else if (component %in% c('justification')) {
      val <- 'left'
    } 
  }
  if (is.null(px) || is.null(py)) {
    val <- text
  } else{
    val <- c(px, py)
  }
  if (!is.null(text) && !is.null(px) || !is.null(py)) {
    if (text == 'use X-Y') {
      val <- c(px, py)
    } else {
      val <- text
    }
  }
  val
}


#' Title
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

xy <- function(x, y) {
  return(glue('{x}_{y}'))
}

forest.x <- function(dat, shape.col = 'red', line.col = 'darkblue', text.cex = 1.5, digit = 3, width.ratio = c(5, 2), vline = 1, xrang = NULL, text.layout = c(0, 6, 6.8, 8.5, 10)) {
  row.names(dat) <- dat$method
  method <- rownames(dat)
  or <- sprintf(paste0('%.', digit, 'f'), dat$'or')
  orLow  <- sprintf(paste0('%.', digit, 'f'), dat$'or_lci95')
  orHigh <- sprintf(paste0('%.', digit, 'f'), dat$'or_uci95')
  OR <- paste0(or, ' (', orLow, '-', orHigh, ')')
  pVal <- ifelse(dat$pval < 0.001, ' < 0.001', sprintf(paste0('%.', digit, 'f'), dat$pval))
  
  n <- nrow(dat)
  nRow <- n + 1
  ylim <- c(1, nRow)
  layout(matrix(c(1, 2), nc = 2), width = width.ratio)
  
  xlim <- c(0, 10)
  par(mar = c(4, 2.5, 2, 1))
  plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, xlab = '', ylab = '')
  text(text.layout[1], n:1, method, adj = 0, cex = text.cex)
  text(text.layout[2], n:1, pVal, adj = 1, cex = text.cex)
  text(text.layout[3], n + 1, 'P-value', cex = text.cex, font = 2, adj = 1)
  text(text.layout[4], n + 1, 'OR', cex = text.cex, font = 2, adj = 1)
  text(text.layout[5], n:1, OR, adj = 1, cex = text.cex)
  
  par(mar = c(4, 1, 2, 1), mgp = c(2, 0.5, 0), cex.lab = text.cex, cex.axis = text.cex)
  if (is.null(xrang)) {
    xlim = c(min(as.numeric(orLow)*0.75, as.numeric(orHigh)*0.75, 0.5), max(as.numeric(orLow) * 1.25, as.numeric(orHigh) * 1.25, 1))
  } else {
    xlim = xrang
  }
  if (xlim[1] > vline) { xlim[1] = vline }
  if (xlim[2] < vline) { xlim[2] = vline }
  plot(1, xlim = xlim, ylim = ylim, type = 'n', axes = FALSE, ylab = '', xaxs = 'i', xlab = 'OR')
  arrows(as.numeric(orLow), n:1, as.numeric(orHigh), n:1, angle = 90, code = 3, length = 0.05, col = line.col, lwd = 3)
  abline(v = vline, col = 'black', lty = 2, lwd = 2)
  boxcolor = ifelse(as.numeric(or) > 1, shape.col, shape.col)
  points(as.numeric(or), n:1, pch = 15, col = boxcolor, cex = 2)
  axis(1)
}

try.with.time.limit <- function(expr, cpu = Inf, elapsed = Inf, transient = TRUE){
  y <- try({setTimeLimit(cpu, elapsed, transient); expr}, silent = TRUE) 
  if(inherits(y, "try-error")) NULL else y 
}

forest.mv <- function(dat) {
  gg = ggplot(data = dat, aes(x = exposure, y = b, ymin = lo_ci, ymax = up_ci, colour = exposure))
  gg = gg + geom_pointrange()
  gg = gg + geom_hline(yintercept = 0, lty = 2)
  gg = gg + facet_wrap(~method, strip.position = 'top', nrow = 5, scales = 'free_y')
  gg = gg + coord_flip()
  gg = gg + ylab('')
  gg = gg + theme_bw()
  gg = gg + theme(legend.position = 'null', axis.title.y = element_blank())
  gg = gg + scale_colour_brewer(palette = 'Dark2')
  gg = gg + theme(axis.text = element_text(size = 14), strip.text = element_text(size = 14))
  gg + ylim(min(dat$lo_ci) * 0.8, max(dat$up_ci) * 1.2)
}

dag.m <- function(m.t, e.t, o.t, e.m.t, m.o.t, e.o.d.t, e.o.i.t) {
  
  suppressMessages(suppressWarnings(library(ggdag)))
  suppressMessages(suppressWarnings(library(ggplot2)))
  
  dag <- dagify(
    m ~ e,
    o ~ e + m,
    exposure = "e",
    outcome = "o",
    labels = c(m = m.t, e = e.t, o = o.t),
    coords = list(x = c(e = 1, m = 2, o = 3),
                  y = c(e = 1, m = 2, o = 1))
  )
  
  size = 6
  gg <- ggdag_status(dag, stylized = TRUE, text = FALSE, check_overlap = TRUE) +  theme_dag()
  gg <- gg + guides(fill = 'none', color = 'none')
  gg <- gg + annotate(geom = 'text', label = e.t, x = 0.9, y = 0.78, size = size, hjust = 0.0)
  gg <- gg + annotate(geom = 'text', label = m.t, x = 2.0, y = 2.18, size = size, hjust = 0.5)
  gg <- gg + annotate(geom = 'text', label = o.t, x = 3.1, y = 0.78, size = size, hjust = 1.0)
  gg <- gg + annotate(geom = 'text', x = 1.5, y = 1.6, label = e.m.t, size = size - 1, hjust = 1, angle = 0)
  gg <- gg + annotate(geom = 'text', x = 2.5, y = 1.6, label = m.o.t, size = size - 1, hjust = 0, angle = 0)
  gg <- gg + annotate(geom = 'text', x = 2, y = 0.90, label = e.o.d.t, size = size - 1, hjust = 0.5)
  gg <- gg + annotate(geom = 'text', x = 2, y = 1.15, label = e.o.i.t, size = size - 1, hjust = 0.5)
  
  gg
}

