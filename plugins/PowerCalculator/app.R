#' @title
#'
#' @description
#' 
# Load Global

suppressMessages(suppressMessages(library(bslib)))
suppressMessages(suppressMessages(library(shinymeta)))

shiny::addResourcePath(
  "XINGABAO",
  system.file("extdata", package = "MRanalysisBase")
)

ui.alb <- function() {
  tagList(
    navbarPage(
      title = suppressWarnings(tags$img(src = '/XINGABAO/img/mr_logo.png', title = "MRanalysis", width = "250px")),
      theme = bslib::bs_theme(version = 5, bootswatch = "journal"),
      tabPanel(
        tags$h4("Power Calculator (Continuous outcome)"),
        fluidPage(
          titlePanel("Power Calculator (Continuous outcome)"),
          sidebarLayout(
            sidebarPanel(
              numericInput("sampleSize", "Sample size:", value = 10000),
              numericInput("alpha", "Alpha:", value = 0.05),
              numericInput("byx", "Byx:", value = 0.2),
              numericInput("r2", "R2:", value = 0.02),
              shiny::actionButton(inputId = "submit", label = "Submit", class = 'btn-danger btn-floatsession')
            ),
            mainPanel(
              tags$head(MRanalysisBase::web.statistic.baidu, tags$style(
                HTML(
                  ".panel {
                      padding: 20px;
                      border-radius: 10px;
                      box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                      margin-bottom: 20px;
                    }
                    
                    h3 {
                      margin-top: 0;
                    }"
                )
              )),
              tags$head(tags$link(rel = "shortcut icon", href = '/XINGABAO/img/favicon.ico')),
              div(
                class = "panel",
                h3("Result"),
                tableOutput("PowerContinuousResultTable")
              ),
              div(
                class = "panel",
                h3("Parameter interpretation"),
                p("Sample size: Outcome sample size (N)"),
                p("Alpha: Type-I error rate, defaults to 0.05"),
                p("βyx: Regression coefficient, representing the potential causal relationship between exposure and outcome, i.e. the beta value after MR analysis"),
                p("R2: Explanation of exposure by instrumental variables,The input required is the joint R2, which can be obtained by directly summing the R2 of each of the exposed SNPs to obtain the joint R2, while ensuring that there are no linkage disequilibrium(LD)"),
                img(src = "/XINGABAO/img/R2.png", width = "100%", height = "auto")
              )
            )
          )
        )
      ),
      tabPanel(
        tags$h4("Power Calculator (Binary outcome)"),
        fluidPage(
          titlePanel("Power Calculator (Binary outcome)"),
          sidebarLayout(
            sidebarPanel(
              numericInput("sampleSize2", "Sample size:", value = 40000),
              numericInput("alpha2", "Alpha:", value = 0.05),
              numericInput("k2", "K:", value = 1),
              numericInput("or2", "OR:", value = 1.2),
              numericInput("r22", "R2:", value = 0.02),
              actionButton(inputId = "submitBinary", label = "Submit", class = 'btn-danger btn-floatsession')
            ),
            mainPanel(
              tags$head(tags$style(
                HTML(
                  ".panel {
                      padding: 20px;
                      border-radius: 10px;
                      box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                      margin-bottom: 20px;
                    }
                    
                    h3 {
                      margin-top: 0;
                    }"
                )
              )),
              div(
                class = "panel",
                h3("Result"),
                tableOutput("PowerBinaryResultTable")
              ),
              div(
                class = "panel",
                h3("Parameter interpretation"),
                p("Sample size: Outcome sample size (N)"),
                p("Alpha: Type-I error rate, defaults to 0.05"),
                p("K: The ratio of cases to controls, for example, if there are 3400 cases and 5000 controls, then the K = 3400/5000 = 0.68."),
                p("OR: odds ratio:The effect of the exposure factor on the outcome, i.e. the multiplicative increase in the odds of the outcome occurring when the exposure factor is increased by one unit."),
                p("R2: Explanation of exposure by instrumental variables,The input required is the joint R2, which can be obtained by directly summing the R2 of each of the exposed SNPs to obtain the joint R2, while ensuring that there are no linkage disequilibrium(LD)"),
                img(src = "/XINGABAO/img/R2.png", width = "100%", height = "auto"),
              )
            )
          ),
        )
      ),
      tabPanel(
        tags$h4("Citation"),
        fluidPage(
          mainPanel(
            tags$head(tags$style(
              HTML(
                ".panel {
                    padding: 20px;
                    border-radius: 10px;
                    box-shadow: rgba(0, 0, 0, 0.1) 0px 5px 15px;
                    margin-bottom: 20px;
                  }
                  h3 {
                    margin-top: 0;
                  }"
              )
            )),
            div(class = "panel",
                p("1. Stephen Burgess, Sample size and power calculations in Mendelian randomization with a single instrumental variable and a binary outcome, International Journal of Epidemiology, Volume 43, Issue 3, June 2014, Pages 922–929, https://doi.org/10.1093/ije/dyu005"),
                p("2. Stephen Burgess, Simon G Thompson, CRP CHD Genetics Collaboration, Avoiding bias from weak instruments in Mendelian randomization studies, International Journal of Epidemiology, Volume 40, Issue 3, June 2011, Pages 755–764, https://doi.org/10.1093/ije/dyr036"),
            )
          )
        )
      )
    )
  )
}


server.ert <- function(input, output, session) {
  
  output$PowerContinuousResultTable <- renderTable({
    
    if (!is.null(input$submit) && input$submit > 0) {
      isolate({
        ps_beta_based <- function(b1, rsq, n, sig = 0.05) {
          res = pnorm(b1 * sqrt(rsq * n) - qnorm(1 - sig/2))
          data.frame(Parameter = "Power", Value = sprintf('%.4f', res), Description = "")
        }
        
        results <- ps_beta_based(
          n = input$sampleSize, 
          sig = input$alpha, 
          rsq = input$r2, 
          b1 = input$byx
        )
        return(results)
      })
    } else {
      data.frame(
        Parameter = c("Power"), 
        Value = c(0), 
        Description = c("")
      )
    }
  })
  
  output$PowerBinaryResultTable <- renderTable({
    
    if (!is.null(input$submitBinary) && input$submitBinary > 0) {
      isolate(
        {
          ps_binary <-  function(b1, rsq, n, ratio, sig = 0.05) {
            res = pnorm(sqrt(n*rsq*(ratio/(1 + ratio))*(1/(1 + ratio)))*b1 - qnorm(1 - sig/2))
            data.frame(Parameter = "Power", Value = sprintf('%.4f', res), Description = "")
          }
          results <- ps_binary(
            n = input$sampleSize2, 
            sig = input$alpha2, 
            rsq = input$r22, 
            ratio = input$k2,
            b1 = log(input$or2)
          )
          return(results)
        }
      )
    } else {
      data.frame(
        Parameter = "Power", 
        Value = 0, 
        Description = ""
      )
    }
  })
}

# Use for standalone testing, 
# comment out the following line when used as a component.
# shinyApp(ui = ui.alb, server = server.ert, options = list(host = '10.0.12.2', port = 3390))
shinyApp(ui = ui.alb, server = server.ert)