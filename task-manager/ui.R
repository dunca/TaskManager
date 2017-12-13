library(shiny)
library(shinyBS)
library(DT) # always last

getNoTaskSelectedCondition <- function() {
  "typeof input.dtTasks_rows_selected  !== 'undefined' && input.dtTasks_rows_selected.length != 0"
}

getNoTasksAvailable <- function() {
  # https://rstudio.github.io/DT/shiny.html
  "typeof input.dtTasks_rows_all  !== 'undefined' && input.dtTasks_rows_all != 0"
}

navbarPage(
  "Task Manager",
  id = "navbar",
  
  tabPanel(
    "Login",
    
    fluidRow(
      column(
        12,
        align="center",
        textInput("txtManagerCredentials", "Manager credentials", placeholder = "John Doe")
      ),
      
      column(
        12,
        align="center",
        bsButton("btnLogin", "Login", icon("user"))
      )
    )
  ),
  
  tabPanel(
    "Add task",
    
    fluidRow(
      column(
        3,
        textInput("txtClientInformation", "Client information", placeholder = "Liana Stanca")
      ),
      
      column(
        3,
        textInput("txtWorkerName", "Worker name", placeholder = "Dunca Flaviu")
      )
    ),
  
    fluidRow(
      column(
        3,
        textInput("txtTaskCategory", "Task category", placeholder = "Development")
      ),
      
      column(
        3,
        dateRangeInput("dateRangeTask", "Date range")
      )
    ),
    
    fluidRow(
      column(
        3,
        textAreaInput("txtTaskDescription", "Task Description", placeholder = "Create an R based app that allows managers to insert new tasks", rows=10) 
      ),
      
      column(
        3,
        sliderInput("sliderTaskProgress", "Current progress", value = 0, min = 0, max = 100, step = 5)
      )
    ),
    
    fluidRow(
      column(
        6,
        bsButton("btnSubmitTask", "Submit", icon("upload"))
      )

    )
  ),
  
  tabPanel(
    "Tasks",
  
    fluidRow(
      column(
        3,
        selectInput("selTaskMonth", "Filter by month", choices = c("all"))
      ),
      
      column(
        3,
        selectInput("selTaskWorker", "Filter by worker", choices = c("all"))
      ),
      
      column(
        3,
        selectInput("selTaskCategory", "Filter by category", choices = c("all"))
      )
    ),
    
    hr(),
    
    fluidRow(
      dataTableOutput("dtTasks")
      # uiOutput("uiNoTasks")
    ),
    
    # https://stackoverflow.com/questions/37659214/r-shiny-conditional-panel-based-on-row-selected-in-datatable/
    conditionalPanel(
      getNoTaskSelectedCondition(),
      hr(),

      fluidRow(
        column(
          12,
          align="center",
          h3("Task settings")
        )
      ),
      
      fluidRow(
        column(
          3,
          sliderInput("sliderNewTaskProgress", "New task progress", value = 0, min = 0, max = 100, step = 5)
        ),
        
        column(
          1,
          # https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
          style="margin-top: 35px;",
          actionButton("btnSubmitNewTaskProgress", "Submit", icon("upload"))
        ),
        
        column(
          1,
          style="margin-top: 35px;",
          actionButton("btnRemoveTask", "Remove this task", icon("remove"))
        )
      )
    )
  ),
  
  tabPanel(
    "Task progress",
    
    conditionalPanel(
      getNoTaskSelectedCondition(),
      plotOutput("plotTaskProgress")
    ),

    fluidRow(
      uiOutput("uiNoTaskSelected")
    )
  ),
  
  navbarMenu(
    "Worker data",
    
    tabPanel(
      "Per worker plots",
      
      titlePanel("Per worker plots"),

      hr(),
      
      conditionalPanel(
        getNoTasksAvailable(),

        fluidRow(
          column(
            12,
            align="center",
            selectInput("selWorker", "Select a worker", choices = list())
          )
        ),
        
        plotOutput("plotWorkerTasks")
      ),

      fluidRow(
        uiOutput("uiNoTasksPerWorkerPlot")
      )
    ),
    
    tabPanel(
      "Global worker plots",
      
      titlePanel("Global worker plots"),
      
      hr(),

      conditionalPanel(
        getNoTasksAvailable(),
        plotOutput("plotWorkerTaskCount")
      ),

      fluidRow(
        uiOutput("uiNoTasksGlobalWorkerPlot")
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(
      12,
      align="right",
      uiOutput("footerMessageUi")
    )
  )
)