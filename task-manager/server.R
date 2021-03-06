library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(DBI)

# TODO
# 
# 
#

dbName <- "tasks.db"
dbTasksTableName = "tasks"

# the prefix of task progress table names. Eg. the first tasks will have its progress stored in "taskProgress1"
dbTaskProgressTablePrefix = "taskProgress"
dbConnection <- dbConnect(RSQLite::SQLite(), dbName)

source("helper_methods.r")

lastTaskId <- 0;

managerAccounts <- data.frame(email = c("m1@r.com", "m2@r.com"), pass=c("123456", "123456"))

availableTables <- dbListTables(dbConnection)

if (dbTasksTableName %in% availableTables) {
  tasks <- dbReadTable(dbConnection, dbTasksTableName)

  rowCount <- nrow(tasks)

  # the table exists, and it's got rows
  if (rowCount != 0) {
    lastTaskId <- tasks[nrow(tasks),]$TaskId
    print(paste("Read task database. Last task id is ", lastTaskId))
  } else {
    print("Read task database, but it has no rows")
  }

} else {
  taskColumnNames <- c("ClientDescription", "TaskCategory", "TaskDescription", "TaskWorkerName", "LegalResponsible",
                     "TaskStartDate", "TaskDeadline", "TaskProgress", "AuthorInfo", "Month", "TaskComment", "TaskId")

  # https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector
  # give it a matrix, since colnames(x) does not work if the dataframe is empty
  tasks <- data.frame(matrix(ncol = length(taskColumnNames), nrow = 0))
  colnames(tasks) <- taskColumnNames

  print("Task database is empty, created empty dataframe")
}

taskProgressChanges <- list()

# read task progress tables into taskProgressChanges
for (tableName in availableTables) {
  if (startsWith(tableName, dbTaskProgressTablePrefix)) {
    # extract the task id from the table name. Eg. taskProgress123 -> 123
    currentTaskId <- as.numeric(gsub("[a-zA-Z]+(\\d+)$", "\\1", tableName))

    # sucks, if taks ids don't start from 1, a lot of possitions will be unused
    taskProgressChanges[[currentTaskId]] <- dbReadTable(dbConnection, tableName)
  }
}

tasksChangedCounter <- 0;

function(input, output, session) {
  filteredTasks <- NULL
  authenticatedUser <- NULL

  observe({
    updateTaskSubmissionButtonState()

    updateButton(session, "btnLogin", disabled = isNullOrEmpty(input$txtManagerEmail) || isNullOrEmpty(input$txtManagerPassword))
  })
  
  # login button was pressed
  observeEvent(
    input$btnLogin,
    {
      email <- input$txtManagerEmail
      pass <- input$txtManagerPassword

      if (!authenticate(email, pass)) {
        output$footerMessageUi <- renderUI({
          (h4("Invalid credentials", style="color: red;"))
        })

        return()
      }

      authenticatedUser <<- input$txtManagerEmail
      
      output$footerMessageUi <- renderUI({
        h4("Logged in as ", span(strong(authenticatedUser), style="color: green;"))
      })
    }
  )
  
  # task submission button was pressed
  observeEvent(
    input$btnSubmitTask,
    {
      addNewTask()
    }
  )
  
  # a task was selected from within the task lisk
  observeEvent(
    input$dtTasks_rows_selected,
    {
      selectedTask <- getSelectedTask()
      selectedTaskProgress <- selectedTask$TaskProgress
      
      updateSliderInput(session, "sliderNewTaskProgress", value = selectedTaskProgress)
    }
  )
  
  # task progress submission button was pressed
  observeEvent(
    input$btnSubmitNewTaskProgress,
    {
      newTaskProgress <- input$sliderNewTaskProgress
      selectedTaskId <- getSelectedTaskRowId()
      selectedTask <- getSelectedTask();

      # selectedTask$TaskProgress <- newTaskProgress # not working?!
      tasks[selectedTaskId,]$TaskProgress <<- newTaskProgress
      
      #taskProgressChanges_ <- taskProgressChanges[[selectedTaskId]]
      taskProgressChanges_ <- taskProgressChanges[[selectedTask$TaskId]]
      newTaskChange <- data.frame(Date=as.character(Sys.Date()), Progress=newTaskProgress, stringsAsFactors = FALSE)
      #taskProgressChanges[[selectedTaskId]] <<- rbind(taskProgressChanges_, newTaskChange)
      taskProgressChanges[[selectedTask$TaskId]] <<- rbind(taskProgressChanges_, newTaskChange)
      
      notifyTasksChanged(paste("Modifed progress for task with description'", selectedTask$TaskDescription, "'"))

      updateTasksDatabaseTable()
      dbWriteTable(dbConnection, getTaskProgressTableName(selectedTask$TaskId), newTaskChange, append=TRUE)
    }
  )

  # task comments submission button was pressed
  observeEvent(
    input$btnSubmitNewTaskComment,
    {
      newTaskComment <- input$txtNewTaskComment;

      selectedTaskId <- getSelectedTaskRowId()
      selectedTask <- getSelectedTask();

      tasks[selectedTaskId,]$TaskComment <<- newTaskComment
      notifyTasksChanged(paste("Modifed comment for task with description'", selectedTask$TaskDescription, "'"))

      updateTasksDatabaseTable()
    }
  )
  
  # task removal button was pressed
  observeEvent(
    input$btnRemoveTask,
    {
      removeSelectedTask()
    }
  )

  # selected tab changed
  observeEvent(
    input$navbar,
    {
      updateTaskSubmissionButtonState()

      # make filters show up in other sessions (on tab change)
      updateFilteringMenus()

      # # if there are no tasks, update the datatable placeholder message
      # output$uiNoTasks <- renderUI({
      #   if (nrow(tasks) == 0) {
      #     getCenteredShinyColumn("No tasks found")
      #   }
      # })
      
      # if no task is selected, update the plot placeholder message
      output$uiNoTaskSelected <- renderUI({
        if (nrow(tasks) == 0 || is.null(getSelectedTaskRowId())) {
          getCenteredShinyColumn("No task selected")
        }
      })
      
      # update the worker selection menu in "Per worker plots"
      # taskCount <- nrow(tasks)
      # selected <- if (taskCount == 0) NULL else tasks$TaskWorkerName[[1]]
      updateSelectInput(session, "selWorker", choices=unique(tasks$TaskWorkerName))

      # if there are no tasks, update the plot placeholder in "Per worker plots"
      output$uiNoTasksPerWorkerPlot <- renderUI({
        if (nrow(tasks) == 0) {
          getCenteredShinyColumn("No tasks found")
        }
      })

      output$plotWorkerTasks <- renderPlot({
        selectedWorker <- input$selWorker

        if (nrow(tasks) == 0) {
          return()
        }
        
        tasksSubset = subset(tasks, tasks$TaskWorkerName == selectedWorker)

        maxTaskCount <- 0

        # get the maximum number of tasks by task progress
        for (taskProgress in unique(tasksSubset$TaskProgress)) {
          taskCount <- length(which(tasksSubset$TaskProgress == taskProgress))

          if (taskCount > maxTaskCount) {
            maxTaskCount <- taskCount
          }
        }
        
        ggplot(tasksSubset, aes(x=as.factor(TaskProgress))) + geom_bar(width=0.5) + 
          xlab("Task completion [%]") +  ylab("Number of tasks") + 
          ggtitle("Number of tasks by task progress") +
          scale_y_continuous(breaks=round(seq(0, maxTaskCount, by=1), 0))
      })
      
      # if there are no tasks, update the plot placeholder in "Global worker plots"
      output$uiNoTasksGlobalWorkerPlot <- renderUI({
        if (nrow(tasks) == 0) {
          getCenteredShinyColumn("No tasks found")
        }
      })

      # update the worker task count plot in "Global worker plots"
      output$plotWorkerTaskCount <- renderPlot({
        if (nrow(tasks) == 0) {
          return()
        }
        
        maxTaskCount <- 0

        # get the maximum number of tasks per worker
        for (workerName in unique(tasks$TaskWorkerName)) {
          taskCount <- length(which(tasks$TaskWorkerName == workerName))

          if (taskCount > maxTaskCount) {
            maxTaskCount <- taskCount
          }
        }

        ggplot(tasks, aes(x=tasks$TaskWorkerName, fill=tasks$TaskWorkerName)) + geom_bar(width = 0.5) +
          ggtitle("Number of tasks per worker") + xlab("Workers") + ylab("Number of tasks") +
          labs(fill="Workers") + scale_y_continuous(breaks=round(seq(0, maxTaskCount, by=1), 0))
      })
    }
  )
  
  output$dtTasks <- renderDataTable({
    getTasks()
  })

  output$plotTaskProgress <- renderPlot({
    #selectedTaskId <- getSelectedTaskRowId()
    selectedTaskId <- getSelectedTask()$TaskId
    taskProgressDt <- taskProgressChanges[[selectedTaskId]]
    
    # https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation
    ggplot(data=taskProgressDt, aes(x=Date, y=Progress, group=1)) + geom_line() + geom_point() + 
      xlab("Date") + ylab("Task progress [%]") + ggtitle(paste("Progress in time for task #", selectedTaskId)) +
      scale_y_continuous(breaks=round(seq(min(taskProgressDt$Progress), max(taskProgressDt$Progress), by=10), 0))
  })

  # output$plotWorkerTasks <- renderPlot({
  #   selectedWorker <- input$selWorker
    
  #   if (nrow(tasks) == 0) {
  #     return()
  #   }
    
  #   tasksSubset = subset(tasks, tasks$TaskWorkerName == selectedWorker)
    
  #   ggplot(tasksSubset, aes(x=as.factor(TaskProgress))) + geom_bar(width=0.5) + 
  #     xlab("Task completion percentage") +  ylab("Number of tasks") + 
  #     ggtitle("Number of tasks by task progress")
  # })
  
  # https://stackoverflow.com/a/24685003
  getTasks <- reactivePoll(
    100, 
    session,
    checkFunc = function() {
     tasksChangedCounter
    },
    valueFunc = function() {
      # if (nrow(tasks) == 0) {
      #   return()
      # }

      tasksSubset <- tasks
      
      if (input$selTaskMonth != "all") {
        tasksSubset <- subset(tasksSubset, tasksSubset$Month == input$selTaskMonth)
      }
      
      if (input$selTaskWorker != "all") {
        tasksSubset <- subset(tasksSubset, tasksSubset$TaskWorkerName == input$selTaskWorker)
      }
      
      if (input$selTaskCategory != "all") {
        tasksSubset <- subset(tasksSubset, tasksSubset$TaskCategory == input$selTaskCategory)
      }
      
      if (input$selTaskWorker == "all" & input$selTaskMonth == "all" & input$selTaskCategory == "all") {
        filteredTasks <<- NULL
      } else {
        filteredTasks <<- tasksSubset
      }

      datatable(tasksSubset, selection = "single")
    }
  )
  
  addNewTask = function() {
    start <- as.character(input$dateRangeTask[1])
    deadline <- as.character(input$dateRangeTask[2])
    
    # https://stackoverflow.com/questions/22603847/how-to-extract-month-from-date-in-r
    # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
    month = strftime(input$dateRangeTask[1], "%m")
    
    lastTaskId <<- lastTaskId + 1
    taskId <- lastTaskId

    newTask <- data.frame("ClientDescription" = input$txtClientInformation, "TaskCategory" = input$txtTaskCategory, 
                       "TaskDescription" = input$txtTaskDescription, "TaskWorkerName" = input$txtWorkerName,
                       "LegalResponsible" = input$selLegalResponsible, "TaskStartDate" = start, "TaskDeadline" = deadline, 
                       "TaskProgress" = input$sliderTaskProgress, "AuthorInfo" = authenticatedUser, "Month" = month, 
                       "TaskComment" = "", "TaskId" = taskId, stringsAsFactors = FALSE)
     
    # http://www.dummies.com/programming/r/how-to-add-observations-to-a-data-frame-in-r/
    # https://stackoverflow.com/a/1236721
    tasks <<- rbind(tasks, newTask)

    #dbWriteTable(dbConnection, dbTasksTableName, newTask, append=TRUE)
    updateTasksDatabaseTable()
    
    initialProgressDf <- data.frame(Date=start, Progress=input$sliderTaskProgress, stringsAsFactors = FALSE)
    taskProgressChanges[[taskId]] <<- initialProgressDf

    dbWriteTable(dbConnection, getTaskProgressTableName(taskId), initialProgressDf)

    notifyTasksChanged("New task added")

    updateFilteringMenus()
  }
  
  # removes the currently selected task from the internal task dataframe
  removeSelectedTask = function() {
    selectedTaskId = getSelectedTaskRowId()

    # delete task progress table
    currentTaskId <- tasks[selectedTaskId,]$TaskId
    dbRemoveTable(dbConnection, getTaskProgressTableName(currentTaskId))

    tasks <<- tasks[-c(selectedTaskId),]
    # taskProgressChanges[-c(selectedTaskId)]
    taskProgressChanges[currentTaskId] <- NULL
    
    notifyTasksChanged("Task removed")

    updateFilteringMenus()

    updateTasksDatabaseTable()
  }
  
  getSelectedTaskRowId = function() {
    selectedRowId <- input$dtTasks_rows_selected
    print(paste("Selected task id is", selectedRowId))

    if (is.null(filteredTasks)) {
      return(selectedRowId)    
    }
   
    selectedRow <- filteredTasks[selectedRowId,]
    taskId <- selectedRow$TaskId

    print(paste("Data is filtered. Real selected task id is", taskId))    

    # https://stackoverflow.com/questions/21013198/return-row-numbers-for-a-particular-value-in-a-column-in-a-dataframe
    return(which(tasks$TaskId == taskId)[1])
  }
  
  getSelectedTask = function() {
    tasks[getSelectedTaskRowId(),]
  }
  
  notifyTasksChanged = function(message) {
    tasksChangedCounter <<- tasksChangedCounter + 1
    
    if (!is.null(message)) {
      # https://shiny.rstudio.com/reference/shiny/latest/showNotification.html
      showNotification(message)
    }
  }
  
  updateMonthsList = function() {
    months <- tasks$Month
    months <- sort(unique(months))

    myUpdateSelectInput("selTaskMonth", c("all", months))
  }
  
  updateWorkerNameList = function() {
    workerNames <- tasks$TaskWorkerName 
    workerNames <- sort(unique(workerNames))
    
    myUpdateSelectInput("selTaskWorker", c("all", workerNames))
  }
  
  updateCategoryList = function() {
    categories <- tasks$TaskCategory 
    categories <- sort(unique(categories))
    
    myUpdateSelectInput("selTaskCategory", c("all", categories))
  }
  
  updateFilteringMenus = function() {
    updateMonthsList()
    updateWorkerNameList()
    updateCategoryList()
  }

  updateTaskSubmissionButtonState = function() {
    disableSubmitButton <- isNullOrEmpty(input$txtClientInformation) || 
      isNullOrEmpty(input$txtTaskDescription) || 
      isNullOrEmpty(input$txtTaskCategory) ||
      isNullOrEmpty(input$txtWorkerName) ||
      is.null(authenticatedUser)

    updateButton(session, "btnSubmitTask", disabled = disableSubmitButton)
  }

  myUpdateSelectInput = function(id, choices) {
    updateSelectInput(session, id, choices=choices)
  }

  authenticate = function(email, pass) {
    accountCount <- nrow(managerAccounts)

    for (i in 1:accountCount) {
      pair <- managerAccounts[i,]
      e <- pair[,1]
      p <- pair[,2]

      if (email == e && pass == p) {
        return(TRUE)
      }
    }

    return(FALSE)
  }

  # overrides the "tasks" database table with the current "tasks" dataframe values
  updateTasksDatabaseTable = function() {
    dbWriteTable(dbConnection, dbTasksTableName, tasks, overwrite=TRUE)
  }

  getTaskProgressTableName = function(taskId) {
    return(paste(dbTaskProgressTablePrefix, taskId, sep=""))
  }
}
