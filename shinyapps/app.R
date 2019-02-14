# This is a Shiny web application that calculates the Hip Fracture Risk for
# people with type 2 diabetes.
# Created by Paul Stevenson, Telethon Kids Institute, 2018-11-14

library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
library(V8)

jsCode <- '
  shinyjs.borderCol = function(params) {
    var defaultParams = {
      id : null,
      col : "red"
    };
    params = shinyjs.getParams(params, defaultParams);
    var el = $("#" + params.id);
    el.css("border-color", params.col);
  }
'

#### User interface ----

ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Risk Calculator"), 
  
  ## Sidebar content ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Clinical Information", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  ## Body content ----
  dashboardBody(
    tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
        #result {
          font-size: 5em;
        }
        .amber {
          color: #F39C12;
        }
        .center {
          text-align: center;
        }
        .red {
          color: #CB4335;
        }
        .right {
          text-align: right;
        }"))
    ),
    tabItems(
      # Calculator ----
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  tags$div(h1("10-Year Hip Fracture Risk Calculator for Type 2 Diabetes")),
                  tags$div(p("This application was designed for people with type 2 diabetes.")),
                  tags$div(p("Please read all footnotes before proceeding.")),
                  width = 12,
                  solidHeader = T
                )
              ),
              fluidRow(
                useShinyjs(),
                extendShinyjs(text = jsCode),
                tags$script("
                            Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                              Shiny.onInputChange(variableName, null);
                            });
                            "),
                box(
                  numericInput("age", "Age (40.0-89.9 years)", ""),
                  actionButton("age_calc", "Age Calculator", icon = icon("calculator"), class = "btn-default"),
                  radioButtons("male", "Sex", c("Male" = T, "Female" = F), selected = T, inline = T),
                  numericInput("bmi", HTML(paste0("BMI (kg/m", tags$sup(2), ")")), ""),
                  actionButton("bmi_calc", "BMI Calculator", icon = icon("calculator"), class = "btn-default"),
                  radioButtons("psn", HTML(paste0("Has PSN?", tags$sup("*"))), c("Yes" = T, "No" = F), selected = T, inline = T),
                  radioButtons("egfr", HTML(paste0("Is eGFR < 45&nbsp;mL/min/1.73 m", tags$sup("2"), "?")), c("Yes" = T, "No" = F), selected = T, inline = T),
                  actionButton("egfr_calc", "eGFR Calculator", icon = icon("calculator"), class = "btn-default"),
                  htmlOutput("egfr_caption"),
                  tags$div(
                    actionButton("button", "Calculate", class = "btn-primary", style = "color: white;"),
                    actionButton("reset_button", "Reset", class = "btn-secondary"),
                    class = "right"),
                  solidHeader = T
                ),
                box(title = h3("10-Year Probability of Hip Fracture", class = "center"),
                    tags$div(
                      textOutput("result_descriptive"),
                      htmlOutput("result"),
                      class = "center"),
                    htmlOutput("recommendation"),
                    solidHeader = T
                )
              ),
              fluidRow(
                box(
                  tags$div(p("This application was designed for people with type 2 diabetes. Results may not be accurate for people without type 2 diabetes.")),
                  tags$div(p("This application uses the CKD-EPI equation for eGFR that was originally developed for White (or other non-African-American individuals) and African-American cohorts; however, this calculator has not been validated for African-American individuals.")),
                  tags$div(p(HTML(paste("For more information about this calculator see reference:", tags$a(HTML("WA Davis <em>et al.</em> Diabetes Care 2019; 42(1): 102-109. https://doi.org/10.2337/dc18-1486"), href = "https://doi.org/10.2337/dc18-1486"))))),
                  tags$div(p(HTML(paste(tags$sup("*"), "Peripheral sensory neuropathy (PSN) is a score of >&nbsp;2/8 in the clinical portion of the Michigan Neuropathy Screening Instrument (MNSI) – reference:", tags$a(HTML("EL Feldman <em>et al.</em> Diabetes Care 1994; 17(11): 1281-1289. https://doi.org/10.2337/diacare.17.11.1281"), href = "https://doi.org/10.2337/diacare.17.11.1281"))))),
                  solidHeader = T,
                  width = 12
                )
              ),
              fluidRow(
                column(12, align="center",
                       tags$div(p("Developed by the Telethon Kids Institute Biometrics Group, 2019."))
                )
              )
      ),
      
      # about ----
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = h2("Hip Fracture Risk Calculator For People With Type 2 Diabetes - Clinical Information"),
                  h3("Important disclaimers"),
                  tags$ol(
                    tags$li("Estimation of fracture risk using a fracture risk calculator is a useful tool to provide an indication of fracture risk. However it should not replace a formal assessment of bone health by a medical practitioner."),
                    tags$li("This fracture calculator has been designed for use in people with type 2 diabetes, between the ages of 40 and 89 years."),
                    tags$li("This fracture calculator provides a risk estimate for hip fracture only."),
                    tags$li("If a person over the age of 50 years has already experienced a minimal trauma fracture of the hip or spine, a diagnosis of osteoporosis can be presumed and they should see their medical practitioner to discuss treatment options."),
                    tags$li("If a person over the age of 50 years has already been found to have very low bone mineral density (T-score at total hip, femoral neck or spine \u2264 -2.5) they should see their medical practitioner for further investigation and discuss need for treatment.")
                  ),
                  h3("Hip fracture risk- thresholds for intervention"),
                  h3("\u2265 3% - High fracture risk (Red)"),
                  p("Recommend the patient visits their medical practitioner to discuss further investigation and treatment of osteoporosis."),
                  p("The patient may benefit from falls reduction strategies, regular exercise, smoking cessation, reduction of alcohol consumption, lifestyle modification and education."),
                  h3("< 3% - Fracture risk not increased (Amber)"),
                  p("The patient may benefit from falls reduction strategies, regular exercise, smoking cessation, reduction of alcohol consumption, lifestyle modification and education."),
                  h3("NB - there is no Green option"),
                  p("Reference: Osteoporosis Australia and RACGP Recommendations: Osteoporosis Risk Assessment, Diagnosis and Management"),
                  width = 12,
                  solidHeader = T)
              ),
              fluidRow(
                column(12, align="center",
                       tags$div(p("Developed by the Telethon Kids Institute Biometrics Group, 2019."))
                )
              )
      )
    )
  )
)

#### Define server logic for the cacultor ----
server <- function(input, output, session) {

  # When calculate button is clicked ----
  result <- eventReactive(input$button, {
    
    #### validate appropriate vales in each box and highlight red if not
    # age
    if (isTruthy(input$age)) {
      if (input$age < 40.0 || input$age > 89.9) {
        js$borderCol("age", "red")
      } else {
        js$borderCol("age", "LightGray")
      }
    } else {
      js$borderCol("age", "red")
    }
    
    # bmi
    if (isTruthy(input$bmi)) {
      js$borderCol("bmi", "LightGray")
    } else {
      js$borderCol("bmi", "red")
    }
    
    # validates age is in range
    validate(
      need(input$age >= 40.0, message = F),
      need(input$age <= 89.9, message = F)
    )
    
    # Continue if all other fields have a truthy value
    req(input$age, input$bmi)
    
    # calculate L
    l <-  (+ 0.0629 * (input$age - 65.0)
           - 1.3854 * as.numeric(as.logical(input$male))
           - 0.0574 * (input$bmi - 29.4)
           + 0.8059 * as.numeric(as.logical(input$psn))
           + 0.7153 * as.numeric(as.logical(input$egfr)))
    
    # calcuate P
    p <- 1 - exp(-0.0240 * exp( l ))
    
    # return objects as a list
    list(p = p)
    
  })
  
  # Reset button
  observeEvent(input$reset_button, {
    # input box
    updateNumericInput(session, "age", value = "")
    updateRadioButtons(session, "male", selected = T)
    updateNumericInput(session, "bmi", value = "")
    updateRadioButtons(session, "psn", selected = T)
    updateRadioButtons(session, "egfr", selected = T)
    # results box
    output$result_descriptive <- renderText({ HTML("") })
    output$result <- renderText({ HTML("") })
    output$recommendation <- renderText({ HTML("") })
  })
  
  # Main percentage result
  output$result <- renderText({
    if (result()$p >= 0.03) {
      HTML(paste0("<div class = 'red'>", sprintf("%.1f", 100 * result()$p), "%</div>"))
    } else {
      HTML(paste0("<div class = 'amber'>", sprintf("%.1f", 100 * result()$p), "%</div>"))
    }
  })
  
  # Descriptive result
  output$result_descriptive <- renderText({
    paste(
      "The probability of an incident hip fracture during the next 10 years is",
      round(result()$p, 4),
      "(or",
      paste0(sprintf("%.1f", 100 * result()$p), "%).")
    )
  })
  
  # Recommendation
  output$recommendation <- renderText({
    if (result()$p >= 0.03) {
      HTML("<p>Recommend the patient visits their medical practitioner to discuss further investigation and treatment of osteoporosis.</p>
           <p>The patient may benefit from falls reduction strategies, regular exercise, smoking cessation, reduction of alcohol consumption, lifestyle modification and education.</p>")
    } else {
      HTML("<p>The patient may benefit from falls reduction strategies, regular exercise, smoking cessation, reduction of alcohol consumption, lifestyle modification and education.</p>")
    }
  })
  
  # Age Calculator modal
  observeEvent(input$age_calc, {
    showModal(modalDialog(
      title = "Age Calculator",
      dateInput("dob", "Date of Birth (dd/mm/yyyy)", value = "1947-01-01", format = "dd/mm/yyyy", startview = "year"),
      footer = tagList(
        actionButton("age_ok", "OK", class = "btn-primary", style = "color: white;"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Calculate age
  observeEvent(input$age_ok, {

    # dob
    if (isTruthy(input$dob)) {
      js$borderCol("dob", "white")
    } else {
      js$borderCol("dob", "red")
    }
    
    # Continue if all other fields have a truthy value
    req(input$dob)
    
    dob <- ymd(input$dob)
    age <- round(as.numeric(as.duration(interval(dob, today())), "years"), 1)
    updateNumericInput(session, "age", value = age)
    removeModal()
  })
  
  # BMI Calculator modal
  observeEvent(input$bmi_calc, {
    showModal(modalDialog(
      title = "BMI Calculator",
      numericInput("height", "Height (m)", ""),
      numericInput("weight", "Weight (kg)", ""),
      radioButtons("units", "Units", c("Metric" = "m", "Imperial" = "i"), selected = "m", inline = T),
      footer = tagList(
        actionButton("ok", "OK", class = "btn-primary", style = "color: white;"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Change BMI labels
  observeEvent(input$units, {
    if (input$units == "m") {
      updateNumericInput(session, "height", label = "Height (m)")
      updateNumericInput(session, "weight", label = "Weight (kg)")
    } else {
      updateNumericInput(session, "height", label = "Height (in)")
      updateNumericInput(session, "weight", label = "Weight (lb)")
    }
  })
  
  # Calculate BMI
  observeEvent(input$ok, {
    
    # weight
    if (isTruthy(input$weight)) {
      js$borderCol("weight", "white")
    } else {
      js$borderCol("weight", "red")
    }
    
    # height
    if (isTruthy(input$height)) {
      js$borderCol("height", "white")
    } else {
      js$borderCol("height", "red")
    }
    
    # Continue if all other fields have a truthy value
    req(input$weight, input$height)
    
    if (input$units == "m") {
      bmi <- round(input$weight / (input$height ^ 2), 1)
    } else {
      bmi <- round(703 * (input$weight / (input$height ^ 2)), 1)
    }
    updateNumericInput(session, "bmi", value = bmi)
    removeModal()
  })
  
  # eGFR Calculator modal
  observeEvent(input$egfr_calc, {
    showModal(modalDialog(
      title = "eGFR Calculator",
      tags$div(p(HTML("eGFR estimated by CKD-EPI equation (using CI units for serum creatinine) – reference AS Levey <em>et al.</em> Ann Intern Med 2009; 150 (9): 604-612"))),
      numericInput("creat", "Serum Creatinine (mg/dL)", ""),
      radioButtons("creat_ci", "Serum Creatinine Units", c("Conventional (mg/dL)" = T, "SI (mmol/L)" = F), selected = T, inline = T),
      radioButtons("white", "Ethnicity", c("White (or other non-African-American" = T, "African-American" = F), selected = T, inline = T),
      footer = tagList(
        actionButton("egfr_ok", "OK", class = "btn-primary", style = "color: white;"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Change eGFR serum creatinine unit labels
  observeEvent(input$creat_ci, {
    if (input$creat_ci == T) {
      updateNumericInput(session, "creat", label = "Serum Creatinine (mg/dL)")
    } else {
      updateNumericInput(session, "creat", label = "Serum Creatinine (mmol/L)")
    }
  })
  
  # Calculate eGFR
  observeEvent(input$egfr_ok, {
    
    # creat
    if (isTruthy(input$creat)) {
      js$borderCol("creat", "LightGray")
    } else {
      js$borderCol("creat", "red")
    }
    if (isTruthy(input$age)) {
      js$borderCol("age", "LightGray")
    } else {
      js$borderCol("age", "red")
      removeModal()
    }
    
    # Continue if all other fields have a truthy value
    req(input$creat, input$age)
    
    ## convert SI to CI TO DO
    creat <- input$creat
    if (!as.logical(input$creat_ci))
      creat <- creat/88.4

    egfr <- case_when(
      # White female
      creat <= 0.7 & as.logical(input$white) & !as.logical(input$male) ~ 144*((creat/0.7)^(-0.329))*(0.993^input$age),
      creat > 0.7 & as.logical(input$white) & !as.logical(input$male) ~ 144*((creat/0.7)^(-1.209))*(0.993^input$age),
      # White male
      creat <= 0.9 & as.logical(input$white) & as.logical(input$male) ~ 141*((creat/0.9)^(-0.411))*(0.993^input$age),
      creat > 0.9 & as.logical(input$white) & as.logical(input$male) ~ 141*((creat/0.9)^(-1.209))*(0.993^input$age),
      # Black female
      creat <= 0.7 & !as.logical(input$white) & !as.logical(input$male) ~ 166*((creat/0.7)^(-0.329))*(0.993^input$age),
      creat > 0.7 & !as.logical(input$white) & !as.logical(input$male) ~ 166*((creat/0.7)^(-1.209))*(0.993^input$age),
      # Black male
      creat <= 0.9 & !as.logical(input$white) & as.logical(input$male) ~ 163*((creat/0.9)^(-0.411))*(0.993^input$age),
      creat > 0.9 & !as.logical(input$white) & as.logical(input$male) ~ 163*((creat/0.9)^(-1.209))*(0.993^input$age)
    )
    
    updateRadioButtons(session, "egfr", selected = ifelse(egfr < 45, T, F))
    output$egfr_caption <- renderText({
      isolate(
        paste("eGFR for a", round(input$age, digits = 1), "year old", ifelse(as.logical(input$white), "white", "black"), ifelse(as.logical(input$male), "male", "female"), "is", paste0(round(egfr, digits = 2), "&nbsp;mL/min/1.73 m", tags$sup("2"), "."))
      )
    })
    
    removeModal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

