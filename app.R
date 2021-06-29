# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(openxlsx)
library(tidyverse)
library(boastUtils)
library(openssl)
library(AzureAuth)
library(fmsb)
library(colorspace)

# Critical Note ----
# This version of the app will not fully work as I have not included all of the
# security aspects such as the key for the encryption, Azure authentication. I
# have left placeholders so that you can get a sense of what you would need to do.

# Global Constants, Data, and Functions ----
## I've replaced the values here with prompts
LOGKEY <- openssl::sha256(charToRaw(Sys.getenv("nameOfKeyForLogins")))
KEY <- openssl::sha256(charToRaw(Sys.getenv("nameOfKey")))
TENANT <- Sys.getenv("nameOfTenantID")
APP_ID <- Sys.getenv("nameOfAuthenticationApp")
REDIRECT <- "urlOfShinyApp"

## Read in Outcomes ----
outcomes <- openxlsx::read.xlsx(xlsxFile = "dataFiles/outcomes.xlsx")
objectives <- outcomes %>%
  dplyr::filter(type == "objective") %>%
  dplyr::select(prefix, title, description)
outcomes <- outcomes %>%
  dplyr::filter(type == "outcome") %>%
  dplyr::select(prefix, title, description)
outcomeCounts <- table(outcomes$prefix)

## Read in Data Files ----
### Log ins
encyptedLogins <- readRDS("dataFiles/encryptedLogins.rds")

### Attendance
encryptedAttendance <- readRDS("dataFiles/encryptedAttendance.rds")

### Gradebook
encryptedGradeBook <- readRDS("dataFiles/encryptedGrades.rds")


# Set Up WebAccess (From Robert Carey) ----
redir_js <- "shinyjs.login = function(uri){ location.replace(uri[0]); }"

login <- function() {
  local <- boastUtils::isLocal()
  # RStudio doesn't like window redirects so we will use a different method.
  if (local) {
    jwt <- get_azure_token(
      c("openid", "offline_access"),
      TENANT, APP_ID,
      use_cache = TRUE,
      version = 2
    )
    return(jwt)
  } else {
    auth_uri <- build_authorization_uri(
      c("openid", "offline_access"),
      TENANT, APP_ID,
      redirect_uri = REDIRECT,
      version = 2
    )
    js$login(auth_uri)
  }
}

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "green",
    ## Header ----
    dashboardHeader(
      title = "Progress Check",
      titleWidth = 250,
      tags$li(
        class = "dropdown",
        bsButton(
          inputId = "logout",
          label = "Log Out",
          style = "danger",
          size = "large",
          icon = icon("sign-out-alt"),
          onclick = "window.open('institution authentication URL with redirect to shiny app', '_self')"
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenuOutput("menu"),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      useShinyjs(),
      extendShinyjs(text = redir_js, functions = c("login")),
      tabItems(
        ### Welcome Page ----
        tabItem(
          tabName = "welcome",
          withMathJax(),
          h1("Standards Progress Checker"),
          h2("Course: STAT 461"),
          p("Welcome! This app provides you with a way to check your progress in
            STAT 461 in terms of Learning Outcomes. If you are not enrolled in
            STAT 461 with Dr. Hatfield, then you will not be able to go any further.
            If you are enrolled in another class with Dr. Hatfield, you'll need
            to go to the correct app for your progress check."),
          p("There are three main pages (besides this welcome page) to this app:"),
          tags$ul(
            tags$li(
              tags$strong("Progress Check: "),
              "shows you the big picture of how many outcomes you have at each
              level, some outcomes to focus on, and your current level of
              attendance and participation.",
              tags$strong("Requires login.")
            ),
            tags$li(
              tags$strong("Outcome Tracker: "),
              "allows you to track different outcomes over time so you can see
              your progress.",
              tags$strong("Requires login.")
            ),
            tags$li(
              tags$strong("Outcomes: "),
              "lists the various outcomes for the course."
            )
          ),
          p("Both the Progress Check and Outcome Tracker pages require you to
            log in using your PSU log in."),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "azureLogin",
              label = "Log in via PSU WebAccess",
              size = "large",
              icon = icon("sign-in-alt")
            )
          ),
          br(),
          uiOutput("userSelect1")
        ),
        ### Overall Progress Page ----
        tabItem(
          tabName = "progress",
          withMathJax(),
          h2("Check Your Overall Progress"),
          uiOutput("progressDate"),
          uiOutput("totalOutcomes"),
          uiOutput("totalObjectives"),
          br(),
          h3("Learning Outcomes"),
          p("The following bar chart shows how many of the assessed learning
            outcomes you have at each level. The three levels where you have met
            the standards are Adept, Advanced, and Proficient. The three Not Yet
            levels are Progressing, Beginning, and Not Shown."),
          plotOutput("progressChart"),
          tags$script(HTML(
            "$(document).ready(function() {
            document.getElementById('progressChart').setAttribute('aria-label',
            `This plot shows how many learning outcomes you have at each level.
            You want to have as many outcomes as possible at the three levels
            where you have met the standards (Adept, Advanced, Proficient).`)
            })"
          )),
          h3("Learning Objectives"),
          p("The following radar chart shows you the summary of level for each
            of the course learning objectives. This provides you with a broader
            overview of how you are doing at this point in time. The further out
            the plot reaches, the higher your level of standards you've met for
            that objective. If there is no dot on an axis, then that axis has not
            been measured."),
          plotOutput("radarChart"),
          h3("Attendance/Participation"),
          fluidRow(
            column(
              width = 5,
              p("Your current attendance/participation level is: "),
            ),
            column(
              width = 1,
              uiOutput("attendancePercent")
            ),
            column(
              width = 5,
              uiOutput("attendanceImg")
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 1,
              p("Icon Key:")
            ),
            column(
              width = 1,
              div(
                style = "text-align: right;",
                uiOutput("correct", inline = TRUE)
              )
            ),
            column(
              width = 2,
              div(
                style = "text-align: left;",
                p("Your attendance and participation is great!")
              )
            ),
            column(
              width = 1,
              div(
                style = "text-align: right;",
                uiOutput("partial", inline = TRUE)
              )
            ),
            column(
              width = 2,
              div(
                style = "text-align: left;",
                p("Your attendance and participation is borderline.")
              )
            ),
            column(
              width = 1,
              div(
                style = "text-align: right;",
                uiOutput("incorrect", inline = TRUE)
              )
            ),
            column(
              width = 4,
              div(
                style = "text-align: left;",
                p("Your attendance and participation is low. Please reach out to
                me to talk about what we can do to get you back on track.")
              )
            )
          ),
          p("As a reminder, there is research that suggests that even missing
            just 10% of a course can have negative effects on a student's learning.
            Looking over past semesters, students attending/participating in less
            than 80% of the course tend to make less progress on the Learning
            Outcomes and have difficulty obtaining mastery.")
        ),
        ### Outcome Tracker Page ----
        tabItem(
          tabName = "tracker",
          withMathJax(),
          h2("Outcome Tracker"),
          uiOutput("trackerDate"),
          br(),
          p("Select up to 6 learning outcomes at a time to track your
            progress over the semester. You may also select a learning objective
            (i.e., a group of learning outcomes) to track."),
          p(em("Note: "), "you are limited to the first 6 outcomes in an attempt
            to keep the plot readable."),
          fluidRow(
            column(
              width = 12,
              shinyWidgets::radioGroupButtons(
                inputId = "objectiveGroup",
                label = "Select by Objective",
                choices = c("Manual select", sort(objectives$prefix)),
                selected = "Manual select",
                justified = TRUE,
                #individual = TRUE,
                #size = "normal",
                checkIcon = list(
                  yes = icon("check-square"),
                  no = icon("square")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              wellPanel(
                shinyWidgets::awesomeCheckboxGroup(
                  inputId = "outcomes",
                  label = "Select outcomes",
                  choices = outcomes$title
                )
              )
            ),
            column(
              width = 9,
              plotOutput("timeSeries"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('timeSeries').setAttribute('aria-label',
                `This plot shows your progress over time for the selected outcomes.
                The horizontal line denotes when your outcome's measure crosses
                indicates you've reached the minimum level of the standards.`)
                })"
              )),
              p("The solid, black, horizontal line indicates the minimal standard
                students need to meet or surpass. Any outcome where you've
                reached Proficiency, Advanced, or Adept status are above this
                line. Outcomes which have not be assessed are not displayed.")
            )
          )
        ),
        ### Outcomes Listing Page ----
        tabItem(
          tabName = "outcomes",
          withMathJax(),
          h2("Learning Outcomes"),
          lapply(1:nrow(objectives), function(i) {
            tagList(
              uiOutput(paste0("obj", i)),
              uiOutput(paste0("objDesc", i)),
              tags$ul(
                uiOutput(paste0("outcomes", i))
              )
            )
          })
        )
      )
    )
  )
)

# Define the Server ----
server <- function(input, output, session) {

  ## Log in via Azure (from Robert Carey) ----
  authorization <- reactiveVal()
  authorized <- reactiveVal(FALSE)
  allowed <- reactiveVal(FALSE)
  userName <- reactiveVal("initial")
  successLogin <- reactiveVal(FALSE)
  userType <- reactiveVal("student")

  observeEvent(
    eventExpr = input$azureLogin,
    handlerExpr = {
      showNotification(
        ui = "Attempting to login in via WebAccess...",
        id = "loginMsg",
        type = "message"
      )
      jwt <- login()
      authorization(jwt)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observe({
    # Grab query string to see if auth code exists.
    opts <- parseQueryString(isolate(session$clientData$url_search))

    # Continue only if authorized locally or a code is passed.
    if (length(authorization()) > 0 || !is.null(opts$code)) {
      # Is the app deployed or running locally?
      local <- boastUtils::isLocal()

      # If deployed use the code provided by redirect to authorize.
      if (!local && length(authorization()) == 0) {
        jwt <- get_azure_token(
          c("openid", "offline_access"), TENANT, APP_ID,
          auth_type = "authorization_code",
          authorize_args = list(redirect_uri = REDIRECT),
          use_cache = FALSE, auth_code = opts$code, version = 2
        )
        authorization(jwt)
      }

      # If a valid token is provided: authorized.
      authorized(is_azure_token(authorization()))

      if (authorized()) {
        auth <- decode_jwt(authorization())
        logins <- unserialize(aes_cbc_decrypt(encyptedLogins, key = LOGKEY))
        userName(
          strsplit(
            x = auth$payload$upn,
            split = "@"
          )[[1]][[1]][[1]]
        )
        if (userName() %in% logins$userName) {
          allowed(TRUE)
        } else {
          allowed(FALSE)
        }
      }

      if (allowed()) {
        ### Sidebar Update
        output$menu <- renderMenu({
          sidebarMenu(
            id = "pages",
            menuItem(
              tabName = "welcome",
              text = "Welcome",
              icon = icon("door-open")
            ),
            menuItem(
              tabName = "progress",
              text = "Progress Check",
              icon = icon("tasks")
            ),
            menuItem(
              tabName = "tracker",
              text = "Outcome Tracker",
              icon = icon("calendar-check")
            ),
            menuItem(
              tabName = "outcomes",
              text = "Learning Outcomes",
              icon = icon("list-alt")
            )
          )
        })

        userType(logins[which(logins$userName == userName()), "userType"])

        if (userType() == "special") {
          output$userSelect1 <- renderUI({
            tagList(
              h3("Special User Selection Menu"),
              selectInput(
                inputId = "studentSelect1",
                label = "Select which student to view",
                choices = c("Ztest, Sec 2", "Ztest, Sec 3")
              )
            )
          })
        } else if (userType() == "teacher") {

          students <- logins$momName[-which(
            logins$momName %in% c("Carey, Robert", "Hatfield, Neil")
            )]

          output$userSelect1 <- renderUI({
            tagList(
              h3("Special User Selection Menu"),
              selectInput(
                inputId = "studentSelect1",
                label = "Select which student to view",
                choices = c("Sample Median", "SAM", sort(students)),
                selected = "Sample Median"
              )
            )
          })
        }

        ### Send Success Alert
        sendSweetAlert(
          session = session,
          title = "Success",
          text = paste0("You have successfully logged in as ",
                        userName(), ". You now have access to restricted pages
                     via the sidebar menu."),
          type = "success"
        )

        successLogin(TRUE)
      } else {
        sendSweetAlert(
          session = session,
          title = "Failure",
          text = paste("You have not entered valid and allowed credentials.",
                       "If you believe this is an error, please contact Dr.
                     Hatfield"),
          type = "error"
        )

        successLogin(FALSE)

        gradeData <<- NULL
        gradeTimes <<- NULL
      }
    }
  })

  ## Log Out ----
  observeEvent(
    eventExpr = input$logout,
    handlerExpr = {
      updateTextInput(
        session = session,
        inputId = "userName",
        value = ""
      )
      updateTextInput(
        session = session,
        inputId = "password",
        value = ""
      )
      output$menu <- renderMenu({
        sidebarMenu(
          id = "pages",
          menuItem(
            tabName = "welcome",
            text = "Welcome",
            icon = icon("door-open")
          ),
          menuItem(
            tabName = "outcomes",
            text = "Learning Outcomes",
            icon = icon("list-alt")
          )
        )
      })
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "welcome"
      )

      updateRadioGroupButtons(
        session = session,
        inputId = "objectiveGroup",
        selected = "Manual select"
      )

      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = ""
      )

      ## Reset plots and outputs
      output$attendancePercent <- NULL
      output$attendanceImg <- boastUtils::renderIcon()
      output$progressChart <- NULL
      output$timeSeries <- NULL
      successLogin(FALSE)

      sendSweetAlert(
        session = session,
        title = "Success",
        text = paste("You have successfully logged out."),
        type = "success"
      )

      hashed <- names(AzureAuth::list_azure_tokens())[1]
      AzureAuth::delete_azure_token(
        tenant = TENANT,
        app = APP_ID,
        hash = hashed)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Allowed Action 3-Get MOM Name ----
  momName <- eventReactive(
    eventExpr = c(allowed(), input$studentSelect1),
    valueExpr = {
      if (allowed()) {
        if (userType() == "special" || userType() == "teacher") {
          input$studentSelect1
        } else {
          logins[which(logins$userName == userName()), "momName"]
        }
      } else {
        NULL
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 4a-Attendance Value ----
  atdProp <- eventReactive(
    eventExpr = momName(),
    valueExpr = {
      temp1 <- unserialize(aes_cbc_decrypt(encryptedAttendance, key = KEY))
      if (momName() %in% c("list special users here")) {
        sendSweetAlert(
          session = session,
          title = "User Name Issue",
          text = paste("Please use the Special User Seletion Menu to select a",
                       "valid option for outcome tracking."),
          type = "error"
        )
      } else if (momName() == "Sample Median") {
        round(median(temp1$atdProp, na.rm = TRUE), digits = 2)
      } else if (momName() == "SAM") {
        round(mean(temp1$atdProp, na.rm = TRUE), digits = 2)
      } else {
        round(temp1[which(temp1$Name == momName()), "atdProp"], digits = 2)
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 4b-Display Attendance ----
  observeEvent(
    eventExpr = c(input$pages, atdProp()),
    handlerExpr = {
      if (input$pages == "progress") {
        if (is.null(atdProp()) || is.na(atdProp())) {
          atdVal <- 0
        } else {
          atdVal <- atdProp()
        }
        output$attendancePercent <- renderUI({
          paste0(atdVal * 100, "%")
        })
        output$attendanceImg <- boastUtils::renderIcon(
          icon = ifelse(
            test = atdVal > 0.9,
            yes = "correct",
            no = ifelse(
              test = atdVal >= 0.8,
              yes = "partial",
              no = "incorrect"
            )
          )
        )
      }

    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Allowed Action 5a-Decrypt Gradebook ----
  rawWorkbook <- eventReactive(
    eventExpr = allowed(),
    valueExpr = {
      if (allowed()) {
        unserialize(aes_cbc_decrypt(encryptedGradeBook, key = KEY))
      } else {
        NULL
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 5b-Get Dates ----
  gradeDates <- eventReactive(
    eventExpr = rawWorkbook(),
    valueExpr = {
      names(rawWorkbook())
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  maxDate <- eventReactive(
    eventExpr = rawWorkbook(),
    valueExpr = {
      format(
        max(as.Date(gradeDates(), tryFormats = c("%m-%d-%Y"))),
        "%m-%d-%Y"
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 5c-List of Data Frames ----
  listScores <- eventReactive(
    eventExpr = rawWorkbook(),
    valueExpr = {
      temp1 <- lapply(gradeDates(), function(x){
        tempA <- openxlsx::readWorkbook(xlsxFile = rawWorkbook(), sheet = x)
        tempA[tempA == "-"] <- NA
        numCols <- names(tempA)[-1]
        tempA[numCols] <- sapply(tempA[numCols], as.numeric)
        return(tempA)
      })
      names(temp1) <- gradeDates()
      temp1
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 6a-Data for Bar Chart ----
  outcomeProgress <- eventReactive(
    eventExpr = momName(),
    valueExpr = {
      currentData <- listScores()[[maxDate()]] %>%
        dplyr::select(Name, outcomes$title)

      if (momName() %in% c("list of special users")) {

      } else if (momName() == "Sample Median") {
        currentData <- currentData %>%
          dplyr::select(-Name) %>%
          dplyr::summarize(across(where(is.numeric), ~median(.x, na.rm = TRUE)))
      } else if (momName() == "SAM") {
        currentData <- currentData %>%
          dplyr::select(-Name) %>%
          dplyr::summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))
      } else {
        currentData <- currentData %>%
          dplyr::filter(Name == momName()) %>%
          dplyr::select(-Name)
      }

      progressData <- currentData %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "Outcome",
          values_to = "Score",
          values_drop_na = TRUE
        ) %>%
        dplyr::mutate(
          category = case_when(
            Score == 1.0 ~ "Adept",
            Score >= 0.85 ~ "Advanced",
            Score >= 0.7 ~ "Proficient",
            Score >= 0.5 ~ "Progressing",
            Score >= 0.1 ~ "Beginning",
            Score <= 0.1 ~ "Not Shown"
          )
        )
      progressData$category <- factor(progressData$category, levels = c(
        "Adept", "Advanced", "Proficient", "Progressing", "Beginning", "Not Shown"
      ))

      progressData
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 6b-Data for Radar Chart ----
  objectiveProgress <- eventReactive(
    eventExpr = momName(),
    valueExpr = {
      currentData <- listScores()[[maxDate()]] %>%
        dplyr::select(Name, objectives$prefix)

      if (momName() %in% c("list of special users")) {

      } else if (momName() == "Sample Median") {
        currentData <- currentData %>%
          dplyr::select(-Name) %>%
          dplyr::summarize(across(where(is.numeric), ~median(.x, na.rm = TRUE)))
      } else if (momName() == "SAM") {
        currentData <- currentData %>%
          dplyr::select(-Name) %>%
          dplyr::summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))
      } else {
        currentData <- currentData %>%
          dplyr::filter(Name == momName()) %>%
          dplyr::select(-Name)
      }

      objectiveScores <- data.frame(
        LRT = c(1, 0),
        Comm = c(1, 0),
        Tech = c(1, 0),
        ANOVA = c(1, 0),
        DA = c(1, 0),
        SI = c(1, 0),
        Design = c(1, 0)
      )
      rbind(
        objectiveScores,
        currentData
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ## Allowed Action 6c-Data for Time Series ----
  outcomeTracking <- eventReactive(
    eventExpr = momName(),
    valueExpr = {
      gradeTimes <- dplyr::bind_rows(listScores(), .id = "date")
      gradeTimes$date <- as.Date(gradeTimes$date, format = "%m-%d-%Y")

      if (momName() %in% c("list of special users")) {

      } else if (momName() == "Sample Median") {
        currentData <- gradeTimes %>%
          dplyr::select(-Name) %>%
          dplyr::group_by(date) %>%
          dplyr::summarize(
            across(where(is.numeric), ~median(.x, na.rm = TRUE)),
            .groups = "keep"
          )
      } else if (momName() == "SAM") {
        currentData <- gradeTimes %>%
          dplyr::select(-Name) %>%
          dplyr::group_by(date) %>%
          dplyr::summarize(
            across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
            .groups = "keep"
          )
      } else {
        currentData <- gradeTimes %>%
          dplyr::filter(Name == momName()) %>%
          dplyr::select(-Name)
      }

      currentData

    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  observeEvent(
    eventExpr = outcomeTracking(),
    handlerExpr = {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        choices = sort(names(outcomeTracking())[-1])
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Display count of outcomes assessed ----
  output$totalOutcomes <- renderUI({
    paste(
      "Total Number of Outcomes assessed to date:",
      length(listScores()[[maxDate()]]) -
        unique(apply(is.na(listScores()[[maxDate()]]), 1, sum)) - 7
    )
  })

  output$totalObjectives <- renderUI({
    paste("Total Number of Objectives assessed to date:",
          7)
  })

  ## Create Outcomes Bar Chart ----
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      if (input$pages == "progress") {
        output$progressChart <- renderPlot({
          ggplot(
            data = outcomeProgress(),
            mapping = aes(x = category)
          ) +
            geom_bar(fill = psuPalette[1], col = "black", na.rm = TRUE) +
            scale_x_discrete(drop = FALSE) +
            scale_y_continuous(
              expand = expansion(mult = 0, add = c(0, 2)),
              breaks = function(x) {seq.int(0, max(x), by = 2)}
            ) +
            labs(
              title = "Outcomes at each level",
              x = "Category",
              y = "Count"
            ) +
            theme_bw() +
            theme(
              text = element_text(size = 16)
            )
        })
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Create Objectives Radar Plot ----
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      if (input$pages == "progress") {
        output$radarChart <- renderPlot(
          expr = {
            par(mai = c(0.2,0.2,0.2,0.2))
            fmsb::radarchart(
              df = objectiveProgress(),
              axistype = 1,
              seg = 4,
              pfcol = colorspace::adjust_transparency(
                col = boastUtils::psuPalette[sample(1:8, 1)],
                alpha = 0.75
              ),
              centerzero = TRUE,
              cglty = 1,
              cglwd = 1,
              cglcol = "grey",
              title = "Learning Objectives",
              caxislabels = c(rep("", 4), "Adept"),
              axislabcol = "black",
              vlcex = 1.1,
              calcex = 1
            )
          },
          alt = "Radar plot for your masters of the learning objectives"
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Update Selected Outcomes ----
  observeEvent(input$objectiveGroup, {
    if ("ANOVA" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "ANOVA",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("Comm" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "Comm",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("DA" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "DA",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("LRT" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "LRT",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("SI" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "SI",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("Tech" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "Tech",
          x = outcomes$title,
          value = TRUE
        )
      )
    } else if ("Design" %in% input$objectiveGroup) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = grep(
          pattern = "Design",
          x = outcomes$title,
          value = TRUE
        )
      )
    }
  })

  observe({
    if (length(input$outcomes) > 6) {
      updateAwesomeCheckboxGroup(
        session = session,
        inputId = "outcomes",
        selected = head(input$outcomes, 6)
      )
    }
  })

  ## Create Time Series Plot ----
  observeEvent(
    eventExpr = input$outcomes,
    handlerExpr = {
      output$timeSeries <- renderPlot({
        validate(
          need(
            expr = length(input$outcomes) >= 1,
            message = "Select at least one outcome to track."
          )
        )
        outcomeTracking() %>%
          dplyr::select(date, any_of(input$outcomes)) %>%
          tidyr::pivot_longer(
            cols = !c(date),
            names_to = "outcomes",
            values_to = "score",
            values_drop_na = TRUE
          ) %>%
          ggplot(
            mapping = aes(
              x = date,
              y = 100*score,
              color = outcomes,
              shape = outcomes
            )
          ) +
          geom_point(
            size = 4,
            na.rm = TRUE,
            position = position_dodge(width = 0.1)
          ) +
          geom_line(
            size = 1,
            position = position_dodge(width = 0.1)
          ) +
          geom_hline(
            mapping = aes(yintercept = 70),
            color = "black",
            linetype = "solid",
            size = 1
          ) +
          scale_color_manual(values = boastPalette[-5]) +
          labs(
            title = "Progress Over Time",
            x = "Date",
            y = "Score",
            color = "Outcomes",
            shape = "Outcomes"
          ) +
          theme_bw() +
          theme(
            text = element_text(size = 16)
          ) +
          scale_y_continuous(
            limits = c(0, 100),
            expand = expansion(mult = 0, add = 5)
          )
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  ## Default Sidebar Menu ----
  output$menu <- renderMenu({
    sidebarMenu(
      id = "pages",
      menuItem(
        tabName = "welcome",
        text = "Welcome",
        icon = icon("door-open")
      ),
      menuItem(
        tabName = "outcomes",
        text = "Learning Outcomes",
        icon = icon("list-alt")
      )
    )
  })

  ## Display current date ----
  output$progressDate <- renderUI({
    paste("Current as of", maxDate())
  })

  output$trackerDate <- renderUI({
    paste("Current as of", maxDate())
  })

  ## Icon Key ----
  output$correct <- renderIcon(icon = "correct")
  output$partial <- renderIcon(icon = "partial")
  output$incorrect <- renderIcon(icon = "incorrect")

  ## Write Objectives and Outcomes ----
  lapply(1:nrow(objectives), function(i) {
    ### Write Objectives ----
    output[[paste0("obj", i)]] <- renderUI({
      h3(objectives[i, "title"])
    })
    output[[paste0("objDesc", i)]] <- renderUI({
      p(objectives[i, "description"])
    })

    ### Write Outcomes ----
    output[[paste0("outcomes", i)]] <- renderUI({
      temp <- subset(outcomes, prefix == objectives[i, "prefix"])
      a <- list()
      for (j in seq_len(outcomeCounts[[objectives[i, "prefix"]]])) {
        a[[j]] <- tags$li(HTML(paste(
          tags$strong(paste0(temp[j, "title"], ":")),
          temp[j, "description"])))
      }
      tagList(a)
    })
  })

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server, config = list(log = FALSE))