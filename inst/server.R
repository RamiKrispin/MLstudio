
#------------------------------ Server Function -------------------------------------
server <- function(input, output,session) {
  #------------------------------ input_df - set initial parameters -------------------------------------

  input_df <- reactiveValues(counter = 0,
                             data_name = NULL,
                             ts_obj = NULL,
                             mts_obj = NULL,
                             df_list = NULL,
                             df_class = NULL,
                             names_list = NULL,
                             df = NULL,
                             class = NULL,
                             var_summary = NULL)

  #------------------------------ Data tab 1 summary boxes -------------------------------------

  output$installed_datasets <- renderValueBox({
    valueBox(
      length(prev_table$r_datasets), "Installed R Datasets Available", icon = icon("folder-open"),
      color = "green"
    )
  })

  output$in_memory_df <- renderValueBox({
    valueBox(
      length(prev_table$data_frame_list), "In Memory Data Frame", icon = icon("superscript"),
      color = "light-blue"
    )
  })

  output$load_datasets <- renderValueBox({
    valueBox(
      ifelse(is.null(input_df$df_list), 0, length(input_df$df_list)), "Loaded Datasets", icon = icon("list"),
      color = "maroon"
    )
  })

  #------------------------------ Data tab 2 summary boxes -------------------------------------
  output$data_name <- renderValueBox({
    valueBox(
      input$select_df, input_df$class, icon = icon("folder-open"),
      color = "green"
    )
  })

  output$num_var <- renderValueBox({
    valueBox(
      ifelse(is.ts(input_df$df),frequency(input_df$df), ncol(input_df$df)),
      ifelse(is.ts(input_df$df),"Frequency", "Variables"),
      icon = icon("superscript"),
      color = "light-blue"
    )
  })

  output$num_obs <- renderValueBox({
    valueBox(
      ifelse(is.ts(input_df$df),length(input_df$df),nrow(input_df$df)), "Observations", icon = icon("list"),
      color = "maroon"
    )
  })

  #------------------------------ Selecting the Data Input -------------------------------------
  prev_table <- reactiveValues(inputs_list = NULL, # Get the list of avilable dataset to load
                               data_frame_list = df_list, # List of avilable dataframes in memory
                               time_series_list = ts_list, # List of avilable time series in memory
                               r_datasets = installed_datasets, # List of avilable datasets within the installed packages
                               file_name = NULL, # If loading csv file, the name of the file
                               file_path = NULL, # If loading csv file, the path of the file
                               class = NULL, # Identify the class of the selected dataset
                               df_name = NULL  # The name of the selected dataset
  )


  observeEvent(input$data_source,{
    #------------------------------ Loading from data frame or package -------------------------------------
    prev_table$inputs_list <- switch(input$data_source,
                                     "data_frame" = {# Case I - load in memory data frames
                                       # If threre is no any data frame available in memory
                                       if(length(prev_table$data_frame_list) == 0){
                                         showModal(modalDialog(
                                           title = "Warning - No Data Frame",
                                           HTML(paste("There is no any data frame avialable",
                                                      "to load in the R Global Environment",
                                                      sep = "<br/>")
                                           ), size = "s"
                                         ))

                                         df_return_list <- NA
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('0')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       } else { # Otherwise return the list of available data frames in memory
                                         df_return_list <- prev_table$data_frame_list
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('1')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       }
                                       df_return_list
                                     },
                                     "time_series" = {# Case II - load in memory time series
                                       # If threre is no any data frame available in memory
                                       if(length(prev_table$time_series_list) == 0){
                                         showModal(modalDialog(
                                           title = "Warning - No Time Series Data",
                                           HTML(paste("There is no any time series data avialable",
                                                      "to load in the R Global Environment",
                                                      sep = "<br/>")
                                           ), size = "s"
                                         ))

                                         df_return_list <- NA
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('0')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       } else { # Otherwise return the list of available time series in memory
                                         df_return_list <- prev_table$time_series_list
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('1')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       }
                                       df_return_list
                                     },
                                     "inst_pack" = {# Case III - load datasets from installed packages
                                       # If threre is no any dataset available in the installed packages
                                       if(length(prev_table$r_datasets) == 0){
                                         showModal(modalDialog(
                                           title = "Warning - No Datasets",
                                           HTML(paste("There is no any dataset avialable",
                                                      "to load from the installed R packages",
                                                      sep = "<br/>")
                                           ), size = "s"
                                         ))
                                         dataset_list <- NA
                                         output$load_flag <- reactive('0')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       } else {
                                         dataset_list <- prev_table$r_datasets
                                         output$load_flag <- reactive('1')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       }
                                       dataset_list

                                     }

    )
  })



  #------------------------------ Setting the csv file path-------------------------------------
  observeEvent(input$file1,{
    output$load_flag <- reactive('0')
    inFile <- input$file1
    if(!is.null(inFile$datapath)){
      prev_table$file_name <- inFile$name
      prev_table$file_path <- inFile$datapath
      output$load_flag <- reactive('2')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    } else{
      output$load_flag <- reactive('0')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    }

  })

  #------------------------------ Loading from data frame or package -------------------------------------
  # Feed the list of data frames and
  #avialable datasets to the menue selection
  output$df_list <- renderUI({
    if(input$data_source == "data_frame" ) {
      selectInput("df_to_load", "Select Data Frame",
                  choices = prev_table$inputs_list )
    } else if(input$data_source == "time_series" ) {
      selectInput("df_to_load", "Select Series",
                  choices = prev_table$inputs_list )
    } else if(input$data_source == "inst_pack" ){
      selectInput("df_to_load", "Select Dataset",
                  choices = prev_table$inputs_list )
    }
  })

  # Load the data according to the user selection
  df_tbl_view <- reactive({
    prev_table$class <- NULL
    if(input$data_source == "data_frame" & length(prev_table$data_frame_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      df_view <- get(input$df_to_load)
      if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
        prev_table$class <- "data.frame"
        df_view <- as.data.frame(df_view)
      } else if(length(class(df_view)) > 1){
        prev_table$class <- class(df_view)[1]
        df_view <- as.data.frame(df_view)
      } else{
        prev_table$class <- class(df_view)
        df_view <- as.data.frame(df_view)
      }
    } else if(input$data_source == "time_series" & length(prev_table$time_series_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      input_df$ts_obj <- get(input$df_to_load)
      df_view <- get(input$df_to_load)
      if(is.mts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
      } else if(is.ts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
        names(df_view) <- c("date", prev_table$df_name)
      }
      if(length(class(input_df$ts_obj)) > 1 & "ts" %in% class(input_df$ts_obj)){
        prev_table$class <- "ts"
      } else if(length(class(input_df$ts_obj)) > 1){
        prev_table$class <- class(input_df$ts_obj)[1]
      } else{
        prev_table$class <- class(input_df$ts_obj)
      }
      # Loading from installed package
    } else if(input$data_source == "inst_pack" & length(prev_table$r_datasets) != 0){
      df_view <- NULL
      dataset_name <- NULL
      dataset_name <- substr(input$df_to_load,
                             regexpr("-", input$df_to_load) + 2,
                             nchar(trimws(input$df_to_load)))
      package_name <- substr(input$df_to_load,
                             1, (regexpr("-", input$df_to_load) - 2)
      )
      if(!paste("package:", package_name, sep = "") %in% search()){
        p <- NULL
        p <- as.list(package_name)
        do.call("require", p)
      }
      # Loading the selected dataset
      prev_table$df_name <- dataset_name
      if(!is.na(dataset_name)){
        if(dataset_name != "NA"){
          df_view <- try(get(dataset_name), silent = TRUE)
          if(class(df_view) == "try-error" & !is.na(dataset_name)){

            showModal(modalDialog(
              title = "Warning - Cannot Load the Dataset",
              HTML(paste("Cannot Load the Dataset:",
                         "- The package index name is not match the package name",
                         "- or the dataset format cannot be loaded",
                         sep = "<br/>")
              ), size = "s"
            ))
            output$load_flag <- reactive('0')
            outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
          }
        }
      }
      if(class(df_view) != "try-error"){
        output$load_flag <- reactive('2')
        outputOptions(output, "load_flag", suspendWhenHidden = FALSE)

        if(is.mts(df_view)){
          input_df$mts_obj <- df_view
          df_view <- data.frame(date=time(df_view), as.matrix(df_view))
          prev_table$class <- "mts"
        } else if(is.ts(df_view)){
          input_df$ts_obj <- df_view
          df_view <- data.frame(date=time(df_view), as.matrix(df_view))
          names(df_view) <- c("date", prev_table$df_name)
          prev_table$class <- "ts"
        } else if(any(class(df_view) %in% c("data.frame","matrix", "data.table", "table"))){
          if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
            prev_table$class <- "data.frame"
            df_view <- as.data.frame(df_view)
          } else if(length(class(df_view)) > 1){
            prev_table$class <- class(df_view)[1]
            df_view <- as.data.frame(df_view)
          } else{
            prev_table$class <- class(df_view)
            df_view <- as.data.frame(df_view)
          }

        }

      }
    } else if(input$data_source == "import" & !is.null(prev_table$file_path)){
      df_view <- NULL
      prev_table$class <- NULL
      prev_table$df_name <- substr(prev_table$file_name,1,regexpr(".", prev_table$file_name, fixed = T)-1)
      df_view <- read.csv(prev_table$file_path, stringsAsFactors = FALSE)
      prev_table$class <- class(df_view)

    } else {
      df_view <- NULL
    }
    return(df_view)
  })

  # View of the data
  output$view_table <- DT::renderDataTable(
    df_tbl_view()[1:10,],
    server = FALSE,
    rownames = FALSE,
    options = list(pageLength = 10,
                   lengthMenu = NULL)
  )

  #------------------------------ Loading a selected dataset  -------------------------------------
  observeEvent(input$load, {
    name <- prev_table$df_name
    type <- NULL
    type <- ifelse(prev_table$class == "data.frame", "Data Frame",
                   ifelse(prev_table$class == "ts", "Time Series",
                          ifelse(prev_table$class == "mts", "Multiple Time Series",
                                 ifelse(prev_table$class == "matrix", "Matrix",
                                        prev_table$class ))))



    if(!name %in% input_df$data_name){
      input_df$data_name <- c(input_df$data_name, name)
      if(is.null(input_df$loaded_table)){
        input_df$loaded_table <- data.frame(name = name,
                                            var = ncol(df_tbl_view()),
                                            row = nrow(df_tbl_view()),
                                            class = type,
                                            stringsAsFactors = FALSE)

      } else {
        temp <- data.frame(name = name,
                           var = ncol(df_tbl_view()),
                           row = nrow(df_tbl_view()),
                           class = type,
                           stringsAsFactors = FALSE)
        input_df$loaded_table <- rbind(input_df$loaded_table,temp)

        temp <- NULL
      }
      if(is.null(input_df$df_list)){
        if(prev_table$class != "ts"){
          input_df$df_list <- list(df_tbl_view())
        } else {
          input_df$df_list <- list(input_df$ts_obj)
        }
        input_df$df_class <- list(type)

      } else {
        if(prev_table$class != "ts"){
          input_df$df_list[[length(input_df$df_list) + 1]] <- df_tbl_view()
        } else {
          input_df$df_list[[length(input_df$df_list) + 1]] <- input_df$ts_obj
        }
        input_df$df_class[[length(input_df$df_list)]] <- type
      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
      input_df$names_list <- names(input_df$df_list)
    } else{
      if(prev_table$class != "ts"){
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- df_tbl_view()
      } else {
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- input_df$ts_obj
      }
      input_df$df_class[[which(names(input_df$df_list) == name)]] <- type
    }
  })
  #------------------------------ Setting the condition for the "Remove" button  -------------------------------------
  observeEvent(input_df$loaded_table,{
    if(is.null(input_df$loaded_table)){
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    } else {
      output$loaded_table_flag <- reactive("1")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }

  })
  #------------------------------ Activate the "Remove" button -------------------------------------
  observeEvent(input$remove,{

    if(length(input_df$df_list)>1){
      input_df$df_list[[input$list_loaded_df_rows_selected]] <- NULL
      input_df$df_class[[input$list_loaded_df_rows_selected]] <- NULL
      input_df$loaded_table <- input_df$loaded_table[-input$list_loaded_df_rows_selected,]
      input_df$data_name <- names(input_df$df_list)
      input_df$names_list  <- input_df$data_name
    } else {
      input_df$df_list <- NULL
      input_df$loaded_table <- NULL
      input_df$data_name <- NULL
      input_df$names_list <- NULL
      input_df$df_class <- NULL
      input_df$names_list <- "NA"
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
  })
  #------------------------------ Loaded dataset table -------------------------------------
  output$list_loaded_df <- DT::renderDataTable(
    data.frame(input_df$loaded_table),
    colnames = c("Dataset Name", "Num. of Variables", "Num. of Obs", "Data Type"),
    selection = list(selected = 1, mode = 'single'),
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  #------------------------------ DATA TAB 2 -------------------------------------

  observeEvent({
    input_df$names_list
  },{
    output$loaded_ds_list  <- renderUI({
      selectInput("select_df", "Select Dataset",
                  choices = input_df$names_list
      )
    })

  })

  observeEvent(input$select_df, {
    if(!is.null(input$select_df)){
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      input_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      output$data_tab2_table <- DT::renderDataTable(
        data.frame(input_df$df),selection = list(selected = 1, mode = 'single'),
        options = list(pageLength = 10,
                       lengthMenu = c(10, 25, 50))
      )

    } else{
      input_df$df <- NULL
      input_df$class <- NULL
      output$data_tab2_table <- NULL
    }
  })
  #------------------------------ Data tab 2 - Data Prep -------------------------------------
  #------------------------------ Data tab 2 - Creating Variables Table -------------------------------------
  dplyr_df <- reactiveValues(df_name = NULL,
                             group_by = NULL,
                             var_summarise = NULL,
                             sum_by_flag = 0,
                             dplyr_fun_option = NULL,
                             df_summary = NULL)

  observeEvent({input$data_option
    input_df$df
    input$select_df
  }, {
    if(!is.ts(input_df$df)){
      # Variable Attributes
      if(input$data_option == "var_attr" &
         !is.null(input_df$df) &
         !is.null(input_df$loaded_table)
      ){
        var.names <- names(input_df$df)
        var.class <- NULL
        for(i in 1:ncol(input_df$df)){
          if(length(class(input_df$df[,i])) > 1){
            if("factor" %in% class(input_df$df[,i])){
              var.class <- c(var.class, "factor")
            } else {
              var.class <- c(var.class, "NA")
            }
          } else {
            var.class <- c(var.class, class(input_df$df[,i])[1])
          }
        }
        input_df$var_summary <- data.frame(var.names, var.class, stringsAsFactors = FALSE)
        names(input_df$var_summary) <- c("Name", "Class")

        output$data_tab2_var <- DT::renderDataTable(
          input_df$var_summary,
          server = FALSE, rownames = FALSE,
          selection = list(selected = 1, mode = 'single'),
          options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10, dom = 'p')
        )

      } else if(input$data_option == "data_summary" &
                !is.null(input_df$df) &
                !is.null(input_df$loaded_table)) {
        output$group_by  <- renderUI({
          selectizeInput("group_by_summary", "Group by",
                         choices = names(input_df$df),
                         multiple = TRUE)
        })
        output$summarise_var  <- renderUI({
          selectInput("summarise_vars", "Summarise by",
                      choices = names(input_df$df),
                      multiple = TRUE)
        })

        output$summary_name <- renderUI({
          textInput("summary_df_name", label = "Set the Table Name", value = paste(input$select_df, "summary", sep = "_"))
        })

      }
    } else {
      output$data_tab2_ts <- plotly::renderPlotly({


        if(!input$ts_plot_log){
          plotly::plot_ly( x = time(input_df$df), y = input_df$df, type  = "scatter", mode = input$ts_prep_mode)
        } else if(input$ts_plot_log){
          plotly::plot_ly( x = time(input_df$df),
                   y = log(input_df$df, base = exp(1)), type  = "scatter", mode = input$ts_prep_mode) %>%
            plotly::layout(title = "Log Transformation")
        }

      })
    }
  })


  observeEvent({
    input$group_by_summary
    input$data_option
    input$select_df
  },{
    if(input$data_option == "data_summary" &
       !is.null(input$group_by_summary)){
      dplyr_df$group_by <- input$group_by_summary
      output$group_by_flag <- reactive({"1"})
      outputOptions(output, "group_by_flag", suspendWhenHidden = FALSE)
    } else {
      output$group_by_flag <- reactive({"0"})
      outputOptions(output, "group_by_flag", suspendWhenHidden = FALSE)
    }
  })

  observeEvent({
    input$group_by_summary
    input$data_option
    input$summarise_vars
    input$select_df

  },{

    if(input$data_option == "data_summary" &
       !is.null(input$group_by_summary) &
       !is.null(input$summarise_vars)){
      dplyr_df$var_summarise <- input$summarise_vars
      output$var_summarise_flag <- reactive({"1"})
      outputOptions(output, "var_summarise_flag", suspendWhenHidden = FALSE)
      dplyr_df$sum_by_flag <- 1
      dplyr_df$dplyr_fun_option <- c("Count",
                                     "Mean",
                                     "Std Deviation",
                                     "Minimum",
                                     "Maximum" )
    } else {

      output$var_summarise_flag <- reactive({"0"})
      outputOptions(output, "var_summarise_flag", suspendWhenHidden = FALSE)
      dplyr_df$sum_by_flag <- 0
      dplyr_df$dplyr_fun_option <- c("Count")
    }

  })

  observeEvent({
    dplyr_df$sum_by_flag
  }, {
    output$dplyr_fun <- renderUI({
      checkboxGroupInput(inputId = "dplyr_funs",
                         label = "Summarise",
                         choices = dplyr_df$dplyr_fun_option,
                         selected = "Count",
                         inline = FALSE)
    })
  })


  observeEvent(input$run_summary, {

    if(is.null(input$dplyr_funs)){
      showModal(modalDialog(
        title = "Warning - Select Summary Function",
        HTML(paste("No summary function was selceted",
                   "Please select at least one function",
                   sep = "<br/>")
        ), size = "s"
      ))
      output$dplyr_table_flag <- reactive("0")
      outputOptions(output, "dplyr_table_flag", suspendWhenHidden = FALSE)
    } else if(!is.null(input$dplyr_funs) &
              !is.null(input$group_by_summary) &
              is.null(input$summarise_vars)
    ){

      dplyr_str <- NULL
      dplyr_str <- paste("input_df$df %>% dplyr::group_by(", paste(input$group_by_summary, collapse = ","),
                         ") %>% dplyr::summarise(Count = n())", sep = " ")

      dplyr_df$df_summary <- eval(parse(text = dplyr_str))
      output$dplyr_table <- DT::renderDataTable(
        dplyr_df$df_summary,
        server = FALSE,
        rownames = FALSE,
        options = list(pageLength = 10,
                       lengthMenu = NULL)
      )
      output$dplyr_table_flag <- reactive("1")
      outputOptions(output, "dplyr_table_flag", suspendWhenHidden = FALSE)
    } else if(!is.null(input$dplyr_funs) &
              !is.null(input$group_by_summary) &
              !is.null(input$summarise_vars)
    ){

      dplyr_str <- NULL
      sum_str <- NULL

      for(f in input$dplyr_funs){
        if(f == "Count"){
          sum_str <- c(sum_str, paste(input$summarise_vars, "_count = n()", sep = "", collapse = ","))
        }
        if(f == "Mean"){
          sum_str <- c(sum_str, paste(input$summarise_vars, "_mean = mean(", input$summarise_vars ," ,na.rm = TRUE)", sep = "", collapse = ","))
        }
        if(f == "Std Deviation"){
          sum_str <- c(sum_str, paste(input$summarise_vars, "_sd = sd(", input$summarise_vars ," ,na.rm = TRUE)", sep = "", collapse = ","))
        }
        if(f == "Minimum"){
          sum_str <- c(sum_str, paste(input$summarise_vars, "_min = min(", input$summarise_vars ," ,na.rm = TRUE)", sep = "", collapse = ","))
        }
        if(f == "Maximum"){
          sum_str <- c(sum_str, paste(input$summarise_vars, "_max = max(", input$summarise_vars ," ,na.rm = TRUE)", sep = "", collapse = ","))
        }
      }
      dplyr_str <- paste("input_df$df %>% dplyr::group_by(", paste(input$group_by_summary, collapse = ","),
                         ") %>% dplyr::summarise(", paste(sum_str, collapse = ","), ")", sep = " ")


      dplyr_df$df_summary <- eval(parse(text = dplyr_str))
      output$dplyr_table <- DT::renderDataTable(
        dplyr_df$df_summary,
        server = FALSE,
        rownames = FALSE,
        options = list(pageLength = 10,
                       lengthMenu = NULL)
      )

      output$dplyr_table_flag <- reactive("1")
      outputOptions(output, "dplyr_table_flag", suspendWhenHidden = FALSE)
    }
  })
  output$class_df_flag <- reactive({
    ifelse(is.ts(input_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag", suspendWhenHidden = FALSE)

  observeEvent(input$load_summary_table, {
    name <- type <- temp <- NULL
    name <- input$summary_df_name
    type <- "Data Frame"



    if(!name %in% input_df$data_name){
      input_df$data_name <- c(input_df$data_name, name)
      if(is.null(input_df$loaded_table)){
        input_df$loaded_table <- data.frame(name = name,
                                            var = ncol(dplyr_df$df_summary),
                                            row = nrow(dplyr_df$df_summary),
                                            class = type,
                                            stringsAsFactors = FALSE)

      } else {
        temp <- data.frame(name = name,
                           var = ncol(dplyr_df$df_summary),
                           row = nrow(dplyr_df$df_summary),
                           class = type,
                           stringsAsFactors = FALSE)
        input_df$loaded_table <- rbind(input_df$loaded_table,temp)

        temp <- NULL
      }
      if(is.null(input_df$df_list)){
        if(prev_table$class != "ts"){
          input_df$df_list <- list(as.data.frame(dplyr_df$df_summary))
          input_df$df_class <- list(type)
        }


      } else {
        if(prev_table$class != "ts"){
          input_df$df_list[[length(input_df$df_list) + 1]] <- as.data.frame(dplyr_df$df_summary)
          input_df$df_class[[length(input_df$df_list)]] <- type
        }

      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
      input_df$names_list <- names(input_df$df_list)
    } else{
      if(prev_table$class != "ts"){
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- as.data.frame(dplyr_df$df_summary)
        input_df$df_class[[which(names(input_df$df_list) == name)]] <- type
      }

    }
  })
  #------------------------------ Data tab 2 - Creating Variable Summary -------------------------------------
  observeEvent({input$data_tab2_var_rows_selected
    input$select_df
    input_df$df},{
      if(!is.ts(input_df$df)){
        r1 <- input$data_tab2_var_rows_selected
        if(is.numeric(input_df$df[, r1]) | is.integer(input_df$df[, r1])){
          var.mean <- mean(input_df$df[, r1], na.rm = TRUE)
          var.min  <- min(input_df$df[, r1], na.rm = TRUE)
          var.max  <- max(input_df$df[, r1], na.rm = TRUE)
          var.median <- median(input_df$df[, r1], na.rm = TRUE)
          var.sd <- sd(input_df$df[, r1])
          summary.vec <- c(var.mean, var.min, var.max, var.median, var.sd)
          var_s <- data.frame(summary.vec)
          names(var_s) <- names(input_df$df)[r1]
          row.names(var_s) <- c("Mean", "Min", "Max", "Median", "Standard Deviation")
          p <- plotly::plot_ly(y = ~ input_df$df[, r1], type = "box", name = names(input_df$df)[r1],
                       boxpoints = "all", jitter = 0.3,
                       pointpos = -1.8)%>%
            plotly::layout(yaxis = list(title = "Range"))
        } else if(is.factor(input_df$df[, r1])){
          var.n.levels <- length(levels(input_df$df[, r1]))
          var.levels <- NULL
          for(i in 1:var.n.levels){var.levels <- c(var.levels,levels(input_df$df[, r1])[i])}
          var_s <- c(var.n.levels)
          var_s <- data.frame(var_s)
          row.names(var_s) <- c("Number of Levels")
          names(var_s) <- names(input_df$df)[r1]
          factor.df <- dplyr::group_by(input_df$df, get(names(input_df$df)[r1])) %>%
            dplyr::summarise(count = n())
          names(factor.df) <- c(names(names(input_df$df)[r1]), "Count")
          p <- plotly::plot_ly(data = factor.df, name = "Levels",
                       x =  ~ get(names(factor.df)[1]),
                       y =  ~ get(names(factor.df)[2]),
                       type = "bar") %>%
            plotly::layout(yaxis = list(title = "Count"),
                   xaxis = list(title = "Levels"))
        } else if(lubridate::is.Date(input_df$df[, r1])){
          var_s <- NULL
          var_s <- data.frame(c(as.character(min(input_df$df[, r1])),
                                as.character(max(input_df$df[, r1]))), row.names = c("Start/Min Date", "End/Max Date"))
          names(var_s) <- names(input_df$df)[r1]
          p <- NULL
        }

        # render the data summary into table
        output$data_tab2_var_summary <- renderTable(var_s, rownames = TRUE)
        output$data_tab2_summary_plot <- plotly::renderPlotly(p)
      } else {
        ts_table <- data.frame(c(paste(start(input_df$df), collapse = "-"),
                                 paste(end(input_df$df), collapse = "-"),
                                 min(input_df$df, na.rm = TRUE),
                                 max(input_df$df, na.rm = TRUE),
                                 round(sd(input_df$df, na.rm = TRUE),2)),
                               row.names = c("Start Date",
                                             "End Date", "Min Value",
                                             "Max Value","Standard Deviation"))
        names(ts_table) <- input$select_df
        output$ts_table <- renderTable(ts_table, rownames = TRUE)

      }

    })
  #------------------------------ Data tab 2 - Midifing Variables Attributes -------------------------------------
  observeEvent(input$var_modify,{
    if(!is.ts(input_df$df)){
      r2 <- input$data_tab2_var_rows_selected
      input_df$df[,r2] <- switch(input$class_selection,
                                 "numeric" = as.numeric(input_df$df[,r2]),
                                 "factor" = as.factor(input_df$df[,r2]),
                                 "character" = as.character(input_df$df[,r2]),
                                 "date" = {eval(parse(text =
                                                        paste("lubridate::",
                                                              input$date_format,
                                                              "('",
                                                              as.character(input_df$df[,input$data_tab2_var_rows_selected]),
                                                              "')",
                                                              sep = "")))
                                 }
      )
      input_df$df_list[[which(names(input_df$df_list) == input$select_df)]] <- input_df$df
    }
  })

  observeEvent({input$date_format
    input$data_tab2_var_rows_selected
    input$class_selection
    input$select_df
  },{
    if(!is.ts(input_df$df)){
      new.date <- input_df$df[1,input$data_tab2_var_rows_selected]
      new.date <- as.character(new.date)
      output$date_prev <-  renderPrint(eval(parse(text =
                                                    paste("lubridate::",
                                                          input$date_format,
                                                          "('",
                                                          new.date[1],
                                                          "')",
                                                          sep = "")))
      )
    }
  })




  observeEvent(input$tabs,{
    if(input$tabs != "data1" & is.null(input_df$df_list)){
      showModal(modalDialog(
        title = "Warning - No Loaded Dataset",
        HTML(paste("There is no any loaded dataset ",
                   "Please select input and load it",
                   sep = "<br/>")
        ), size = "s"
      ))
    }
  })
  #------------------------------ Data tab 2 - End -------------------------------------
  #------------------------------ Visualization Tab Start -------------------------------------
  # Selecting the Dataset
  # Setting reactive values
  vis_df <- reactiveValues(df = NULL,
                           class = NULL,
                           var_factor = NULL,
                           var_numeric = NULL,
                           var_date = NULL)

  # Setting the data selection
  observeEvent({
    input_df$names_list
  },{

    output$loaded_ds_list_vis  <- renderUI({
      selectInput("select_df_vis", "Select Dataset",
                  choices = input_df$names_list
      )
    })

  })


  observeEvent({
    input$var_modify
    input$select_df_vis
  }, {
    if(!is.null(input$select_df_vis)){
      vis_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df_vis)]]
      )
      vis_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df_vis)]]

      vis_df$var_numeric <- vis_df$var_factor <- NULL
      if(!is.ts(vis_df$df)){
        for(i in 1:ncol(vis_df$df)){
          if(is.factor(vis_df$df[,i])){
            vis_df$var_factor <- c(vis_df$var_factor, names(vis_df$df)[i])
          } else if(is.numeric(vis_df$df[,i]) | is.integer(vis_df$df[,i])){
            vis_df$var_numeric <- c(vis_df$var_numeric,names(vis_df$df)[i])
          }
        }
      }
    } else{

      vis_df$df <- NULL
      vis_df$class <- NULL
      vis_df$var_factor <- NULL
      vis_df$var_numeric <- NULL



    }
  })


  observeEvent({input$var_modify
    input$select_df_vis},{
      if(!is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        ###################### NEED TO ADD CASE FOR ONLY ONE VARIABE !!!!!!
        if(length(vis_df$var_numeric) == 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density"))
          })
          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })

          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None", as.character(vis_df$var_factor))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = "NA"
              )
            }
          })

        } else if(length(vis_df$var_numeric) > 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Scatter" = "scatter",
                                       "Line" = "line",
                                       "Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density",
                                       "Correlation" = "cor"))
          })

          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })

          output$vis_x <- renderUI({
            selectInput("plot_x", "Select the X Axis",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })

          output$vis_y <- renderUI({
            selectInput(
              "plot_y", "Select the Y Axis",
              choices = vis_df$var_numeric,
              selected = vis_df$var_numeric[2]
            )
          })

          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None", as.character(vis_df$var_factor))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = "NA"
              )
            }
          })
        }

      } else if(is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        output$vis_x  <- renderUI({
          selectInput("plot_x", "Select Variables",
                      choices = "No Available Numeric Variables"
          )
        })
      } else if(is.ts(vis_df$df)){
        output$vis_plot_type <- renderUI({
          selectInput("plot_type", "Select the Plot Type",
                      choices = list("Scatter" = "scatter",
                                     "Line" = "line",
                                     "Boxplot" = "box",
                                     "Seasonal Plot" = "seasonal_plot",
                                     "Lags Plot" = "lags_plot"))
        })
      }

    })

  observeEvent({input$var_modify
    input$plot_factor
    input$plot_var
    input$plot_x
    input$plot_y
    input$plot_type
    vis_df$df
    input$select_df_vis

  },{

    output$main_plot <- plotly::renderPlotly({

      if(!is.ts(vis_df$df)){
        p <- x <- y <- color <-   NULL

        if(length(vis_df$var_numeric) > 1){
          y <- vis_df$df[,input$plot_y]
        } else if(length(vis_df$var_numeric) == 1){
          y <- NA
        }

        if(input$plot_type == "box" | input$plot_type == "density"){
          x <- vis_df$df[, input$plot_var]
        } else {
          x <- vis_df$df[,input$plot_x]
        }

        if(input$plot_factor != "None" & input$plot_factor != "NA" & !is.null(input$plot_factor)){
          color <- vis_df$df[,input$plot_factor]
          type <- vis_df$df[,input$plot_factor]
        } else {
          color <-  NULL
          type <- input$plot_var
        }

        p <- switch(input$plot_type,
                    "scatter" = {
                      plotly::plot_ly(x = x, y = y, color = color) %>%
                        plotly::layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "line" = {
                      plotly::plot_ly(x = x, y = y, mode = "lines", color = NULL)%>%
                        plotly::layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "box" = {
                      plotly::plot_ly(y = x, type = "box", color = color,
                              name = names(vis_df$df)[which(names(vis_df$df) == input$plot_factor)],
                              boxpoints = "all", jitter = 0.3,
                              pointpos = -1.8)%>%
                        plotly::layout(yaxis = list(title = names(vis_df$df)[which(names(vis_df$df) == input$plot_x)]),
                               xaxis = list(title = "")
                        )
                    },
                    "hist" = {
                      p_hist <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        p_hist <- plotly::plot_ly(x = vis_df$df[,input$plot_var], type = "histogram")
                      } else if(input$plot_factor != "None" &
                                input$plot_factor != "NA" &
                                !is.null(input$plot_factor)){

                        plot_list <- l <- NULL
                        for(l1 in levels(vis_df$df[,input$plot_factor])){
                          hist.sub.df <- subset(vis_df$df, vis_df$df[,input$plot_factor] == l1)
                          l <- length(plot_list)
                          plot_list[[l + 1]] <- plotly::plot_ly(hist.sub.df,
                                                        x = hist.sub.df[,input$plot_var],
                                                        name = l1) %>%
                            plotly::layout(xaxis = list(title = l1),
                                   title = input$plot_var)

                        }
                        p_hist <- plotly::subplot(plot_list, titleX = TRUE, shareX = TRUE) %>%
                          plotly::hide_legend()
                      }
                      p_hist
                    },
                    "density" = {
                      plot_den <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        dens <- density(x)
                        dens.df <- data.frame(x = dens$x, y = dens$y)
                        min_y <- 0
                        max_y <- max(dens.df$y)
                        plot_den <- plotly::plot_ly(data = dens.df, x  = ~x,
                                            y = ~y)
                      } else if(input$plot_factor != "None" &
                                input$plot_factor != "NA" &
                                !is.null(input$plot_factor)){

                        plot_list_den <- l <-  NULL
                        for(l2 in levels(vis_df$df[, input$plot_factor])){
                          df.den <- subset(vis_df$df,
                                           vis_df$df[, input$plot_factor] == l2)
                          l <- length(plot_list_den)
                          dens <- density(df.den[,input$plot_var])
                          dens.df <- data.frame(x = dens$x, y = dens$y)
                          plot_list_den[[l + 1]] <- plotly::plot_ly(data = dens.df,
                                                            x = ~x,
                                                            y = ~y)%>%
                            plotly::layout(xaxis = list(title = l2),
                                   title = input$plot_var)
                        }

                        plot_den <- plotly::subplot(plot_list_den, titleX = TRUE, shareX = TRUE)%>%
                          plotly::hide_legend()
                      }
                      plot_den


                    },
                    "cor" = {
                      c <- NULL
                      c <- round(cor(vis_df$df[, which(colnames(vis_df$df) %in% vis_df$var_numeric)]), 3)
                      plotly::plot_ly(x = vis_df$var_numeric, y = vis_df$var_numeric, z = c,
                              key = c, type = "heatmap", source = "heatplot")
                    }
        )
      } else if(is.ts(vis_df$df)){
        ts.df <- data.frame(dec_left = floor(time(vis_df$df)),
                            dec_right = round((time(vis_df$df) - floor(time(vis_df$df))) *
                                                frequency(vis_df$df) + 1),
                            value = as.numeric(vis_df$df))
        p <- switch(input$plot_type,
                    "line" = {
                      plotly::plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter", mode = "line")
                    },
                    "scatter" = {
                      plotly::plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter")
                    },
                    "box" = {
                      plotly::plot_ly(data = ts.df, y = ~ value ,
                              color = ~ as.factor(dec_right),
                              type = "box",
                              boxpoints = "all", jitter = 0,
                              pointpos = -1.8)

                    },

                    "seasonal_plot" = {
                      if(frequency(vis_df$df) == 1){
                        p <- plotly::plot_ly()
                        showModal(modalDialog(
                          title = "Warning - Seasonal Plot is Not Available",
                          HTML(paste("Seasonal plot is not available",
                                     "for time series object with yearly frequancy",
                                     sep = "<br/>")
                          ), size = "s"
                        ))
                        p
                      } else {
                        ts.df_wide <- reshape2::dcast(ts.df, dec_right ~ dec_left )
                        p <- plotly::plot_ly()

                        for(f in 2:ncol(ts.df_wide)){
                          p <- p %>% add_trace(x = ts.df_wide[,1], y = ts.df_wide[,f],
                                               name = paste("time", names(ts.df_wide)[f], sep = " " ),
                                               mode = "line")
                        }
                        p
                      }
                    },
                    "lags_plot" = {
                      lag <- NULL
                      lag_plots <- NULL
                      max.lags <- 12
                      for(g in 1:max.lags){
                        if(g == 1){
                          lag <- c(NA, ts.df$value[- nrow(ts.df)])
                        } else {
                          lag <- c(NA,lag[-nrow(ts.df)])
                        }
                        lag_plots[[g]] <- plotly::plot_ly(x = lag, y = ts.df$value,
                                                  name = paste("Lag", g, sep = " ")) %>%
                          plotly::layout(xaxis = list(title = paste("Lag", g, sep = " "),
                                              range = c( min(na.omit(as.numeric(lag))),
                                                         max(na.omit(as.numeric(lag))))),
                                 yaxis = list(title = paste("Series", sep = ""),
                                              range = c( min(na.omit(as.numeric(ts.df$value))),
                                                         max(na.omit(as.numeric(ts.df$value))))),
                                 title = paste(input$select_df_vis,"Series vs Lags", sep = " "),
                                 annotations = list(
                                   # x = median(na.omit(as.numeric(lag))),
                                   # y = median(na.omit(as.numeric(ts.df$value))),
                                   showarrow = FALSE,
                                   # arrowhead = 4,
                                   # arrowsize = 0.5,
                                   # ax = 20,
                                   # ay = -20,
                                   xref = paste("x", g, sep = ""),
                                   yref = paste("y", g, sep = ""),
                                   text = paste("Lag", g, sep = " "))
                          )
                      }

                      plotly::subplot(lag_plots,
                              titleX = FALSE, titleY = TRUE,
                              shareX = FALSE, shareY = FALSE,
                              margin = 0.05,
                              nrows = ceiling(length(lag_plots) / 3))%>%
                        plotly::hide_legend()
                    }
        )

      }




    })
    return(p)
  })

  output$class_df_flag_vis <- reactive({
    ifelse(is.ts(vis_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag_vis", suspendWhenHidden = FALSE)

  #------------------------------ Regression and Classification Models -------------------------------------
  models_df <- reactiveValues(df = NULL, # Load the selected data frame
                              var_list = NULL, # Create a variable list
                              independent_var = NULL, # Create the independent variables list
                              var_dep_class = NULL, # The class of the dependent variable
                              dataset_name = NULL
  )

  # Select the dataset
  observeEvent({
    input$var_modify
    input_df$names_list
  },{
    if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) == 0){
      output$models1_df_list  <- renderUI({
        output$model_tab_input <- reactive("0")
        outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
        models_df$var_list <-  models_df$df <- NULL
        showModal(modalDialog(
          title = "Warning - No Available Data Frame",
          HTML(paste("No available data frame in the platform",
                     "Use the Data tab to load data",
                     sep = "<br/>")
          ), size = "s"
        ))
        output$models1_df_list  <- renderUI({
          models_df$dataset_name <- NA
          selectInput("models1_select_df", "Select Dataset",
                      choices = "NA"
          )
        })
      })
    } else if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) > 0){
      output$model_tab_input <- reactive("1")
      outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
      output$models1_df_list  <- renderUI({
        models_df$dataset_name <- input_df$names_list[which(input_df$df_class == "Data Frame")]
        selectInput("models1_select_df", "Select Dataset",
                    choices = models_df$dataset_name
        )
      })
    }
  })

  # Update the dataset selection
  observeEvent({
    input$var_modify
    input$models1_select_df

  },{
    if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) > 0){
      output$model_tab_input <- reactive("1")
      outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
      models_df$df <- input_df$df_list[[which(input_df$names_list == input$models1_select_df)]]
    } else {
      output$model_tab_input <- reactive("0")
      outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
      models_df$df <- NULL
    }
  })

  # Dependent variable
  observeEvent({
    input$var_modify
    input$models1_select_df

  }, {
    if(!is.null(models_df$df)){
      models_df$var_list <- names(models_df$df)
      output$model_tab_ind <- reactive("1")
      outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
      output$models1_var_list  <- renderUI({
        selectInput("models1_select_var", "Select the Dependent Variable",
                    choices = c("Select Variable",models_df$var_list)
        )
      })
    } else if(is.null(models_df$df)){
      models_df$var_list <- NULL
      output$model_tab_ind <- reactive("0")
      outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
    }

  })


  # Independent variable
  observeEvent(input$models1_select_var, {

    if(input$models1_select_var != "Select Variable"){

      models_df$var_dep_class <- class(models_df$df[,which(names(models_df$df) == input$models1_select_var)])
      models_df$independent_var <- setdiff(names(models_df$df), c(input$models1_select_var, "name"))
      output$model_tab_ind <- reactive("1")
      outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
      output$models1_independent_list  <- renderUI({

        selectizeInput(inputId = "models1_independent",
                       label = "Select the Independent Variable",
                       choices = models_df$independent_var,
                       multiple = TRUE, #selectize = TRUE,
                       # options = list(
                       #   placeholder = 'Please select an option below',
                       #   onInitialize = I('function() { this.setValue(""); }')
                       # ),
                       selected = models_df$independent_var)

      })
    } else if(input$models1_select_var == "Select Variable" | is.null(input$models1_select_var)){

      models_df$independent_var <- NULL
      models_df$var_dep_class <- NULL
      output$model_tab_ind <- reactive("0")
      outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
    }
  })

  observeEvent(input$models1_independent, {
    #print(input$models1_independent)
  })

  observeEvent({
    models_df$var_dep_class
    input$models1_select_var
  },{
    if(!is.null(models_df$var_dep_class)){
      if(is.factor(models_df$df[,which(names(models_df$df) == input$models1_select_var)])){
        if(length(levels(models_df$df[,which(names(models_df$df) == input$models1_select_var)])) == 2){
          output$model_binomial <- reactive("1") # set condition for binomial model
          outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
          h2o_df$binomial <- NULL
          h2o_df$binomial <- "binomial"
        } else if(length(levels(models_df$df[,which(names(models_df$df) == input$models1_select_var)])) > 2){
          output$model_binomial <- reactive("2") # set condition for multinomial model
          outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
          h2o_df$binomial <- NULL
          h2o_df$binomial <- "multinomial"
        } else {
          output$model_binomial <- reactive("0") # not engough levels for binomial/multinomial
          outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
        }
        output$dep_var_class <- reactive("1") # flag for factor variable
        outputOptions(output, "dep_var_class", suspendWhenHidden = FALSE)
      } else if (models_df$var_dep_class == "numeric" |
                 models_df$var_dep_class == "integer") {
        output$dep_var_class <- reactive("2") # flag for numeric/integer variable
        outputOptions(output, "dep_var_class", suspendWhenHidden = FALSE)
        #
        output$model_binomial <- reactive("0") # reseting the binomial flag
        outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
      }
    }
  })

  #------------------------------ H2O Connection -------------------------------------
  h2o_df <- reactiveValues(status = FALSE,
                           num_cpus = NULL,
                           free_mem = NULL,
                           df = NULL,
                           x = NULL,
                           y = NULL,
                           train = NULL,
                           test = NULL,
                           valid = NULL,
                           model = NULL,
                           binomial = NULL)


  observeEvent( input$model_package,{
    if("H2O" %in% input$model_package & !h2o_df$status){
      if(!"h2o" %in% installed.packages()){

        showModal(modalDialog(
          title = "Warning - H2O is not Available",
          HTML(paste("The H2O package is not installed.",
                     "Please install the package to continue.",
                     "More infromation is available here - https://www.h2o.ai/download/",
                     sep = "<br/>")
          ), size = "s"
        ))
        output$h2o_flag <- reactive("0")
        outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
      } else {
        require(h2o)
        try(h2o.init(nthreads=-1,
                     max_mem_size = paste(ceiling(get_free_ram()/1024^2),"g", sep = "")),
            silent = TRUE)
        if(h2o.clusterIsUp()){
          output$h2o_flag <- reactive("1")
          outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
          h2o_df$status <- TRUE
          cluster_status <- h2o.clusterStatus()
          h2o_df$free_mem <- as.numeric(cluster_status$free_mem)
          h2o_df$num_cpus <- as.numeric(cluster_status$num_cpus)
        } else {
          showModal(modalDialog(
            title = "Warning - H2O is not Connect",
            HTML(paste("Couldn't connect to H2O cluster,",
                       "please check in R if the package installed",
                       sep = "<br/>")
            ), size = "s"
          ))
          output$h2o_flag <- reactive("0")
          outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
        }

      }
    } else if(!"H2O" %in% input$model_package &  h2o_df$status){
      try(h2o.shutdown(prompt=FALSE), silent = TRUE)
      h2o_df$status <- FALSE
      h2o_df$free_mem <- NULL
      h2o_df$num_cpus <- NULL
      output$h2o_flag <- reactive("0")
      outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    }
  })

  output$h2o_status_box <- renderValueBox({
    valueBox(
      ifelse(h2o_df$status, "Connected","Disconnected" ), "H2O Status", icon = icon("signal"),
      color = ifelse(h2o_df$status, "green","red" )
    )
  })

  output$h2o_cluster_mem <- renderValueBox({
    valueBox(
      paste(round((h2o_df$free_mem / 1024^3), 2), "GB", sep = ""),
      "H2O Cluster Total Memory", icon = icon("microchip"),
      color = "maroon"
    )
  })

  output$h2o_cpu <- renderValueBox({
    valueBox(
      h2o_df$num_cpus,
      "Number of CPUs in Use", icon = icon("microchip"),
      color = "light-blue"
    )
  })

  observeEvent(input$h2o_run_class, {
    print(input$models1_independent)
    if(!is.null(input$models1_independent)){
      h2o.removeAll()
      # Check if there are any ordered factor
      ordered_factor <- NULL
      ordered_factor <- which(lapply(models_df$df, is.ordered) == TRUE)
      if(length(ordered_factor) > 0){
        if(input$models1_select_var == colnames(models_df$df)[ordered_factor]){
          showModal(modalDialog(
            title = "Warning - Ordered Factor",
            HTML(paste("H2O doesn't support ordered factor class.",
                       "Please select different dependent variable",
                       sep = "<br/>")
            ), size = "s"
          ))
          h2o_df$df <- NULL
        }else if(input$models1_select_var != colnames(models_df$df)[ordered_factor]){
          showModal(modalDialog(
            title = "Warning - Ordered Factor",
            HTML(paste("H2O doesn't support ordered factor class.",
                       paste("the variable '",
                             colnames(models_df$df)[ordered_factor],
                             "' will be exclude", sep = ""),
                       sep = "<br/>")
            ), size = "s"
          ))
          h2o_df$df <- as.h2o(models_df$df[, -ordered_factor])
        }} else if(length(ordered_factor) == 0){
          h2o_df$df <- as.h2o(models_df$df)
        }
      if(!is.null(h2o_df$df)){
        h2o_df$y <- h2o_df$x <- h2o_df$model <- NULL
        h2o_df$train <- h2o_df$test <- h2o_df$valid  <- NULL

        h2o_df$y <- match(input$models1_select_var, names(h2o_df$df))
        h2o_df$x <- match(input$models1_independent, names(h2o_df$df))

        n_folds <- NULL
        if(input$nfolds_flag){
          n_folds <- input$nfolds
        } else {
          n_folds <- 0
        }

        if(input$h2o_validation){

          splits <- h2o.splitFrame(
            data = h2o_df$df,
            ratios = c(input$h2o_split_v[1],(input$h2o_split_v[2] - input$h2o_split_v[1])),
            destination_frames = c("train", "valid", "test"), seed = 1234
          )
          h2o_df$train <- splits[[1]]
          h2o_df$valid <- splits[[2]]
          h2o_df$test  <- splits[[3]]

          if(input$binomial_models == "h2o_rf"){
            # Random Forest Model
            h2o_df$model <- NULL

            h2o_df$model <- h2o.randomForest(
              training_frame = h2o_df$train,
              validation_frame = h2o_df$valid,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              ntrees = input$h2o_rf_ntree,
              max_depth = input$h2o_rf_max_depth,
              col_sample_rate_change_per_level = input$h2o_rf_col_sample_rate_change_per_level,
              col_sample_rate_per_tree = input$h2o_rf_col_sample_rate_per_tree,
              sample_rate = input$h2o_rf_sample_rate,
              histogram_type = input$rf_histogram_type
            )

            if(!is.null(h2o_df$model)){
              output$h2o_rf_flag <- reactive("1")
              outputOptions(output, "h2o_rf_flag", suspendWhenHidden = FALSE)
              output$h2o_rf_model_text <- renderText(
                paste("Random Forest output for the",input$models1_select_df, "dataset", sep = " ")
              )


              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              valid_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$valid))
              valid_df <- as.data.frame(h2o_df$valid[, h2o_df$y])
              valid_cm <- caret::confusionMatrix(valid_pred$predict, valid_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_rf_cm_table  <- function(){
                cm_fun_v(train = train_cm$table, valid = valid_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

              # RMSE plot with validation set
              output$h2o_rf_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training",
                        showlegend = TRUE, line = list(color = "rgb(31, 119, 180)", width = 2)) %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_rmse,
                            type = "scatter", mode = "lines+markers", name = "Validation",
                            showlegend = TRUE, line = list(color = "rgb(255, 127, 14)", width = 2))%>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Classification error plot with validation set
              output$h2o_rf_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training",
                        showlegend = TRUE, line = list(color = "rgb(31, 119, 180)", width = 2)) %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_classification_error,
                            type = "scatter", mode = "lines+markers", name = "Validation",
                            showlegend = TRUE, line = list(color = "rgb(255, 127, 14)", width = 2)) %>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })



              # Logloss plot with validation set
              output$h2o_rf_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training",
                        showlegend = TRUE, line = list(color = "rgb(31, 119, 180)", width = 2)) %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_logloss,
                            type = "scatter", mode = "lines+markers", name = "Validation",
                            showlegend = TRUE, line = list(color = "rgb(255, 127, 14)", width = 2)) %>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_rf_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = NULL,
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_rf_flag <- reactive("0")
              outputOptions(output, "h2o_rf_flag", suspendWhenHidden = FALSE)
            }

          } else if(input$binomial_models == "h2o_gbm"){
            # GBM Model
            h2o_df$model <- NULL

            h2o_df$model <- h2o.gbm(
              training_frame = h2o_df$train,
              validation_frame = h2o_df$valid,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              ntrees = input$h2o_gbm_ntree,
              max_depth = input$h2o_gbm_max_depth,
              learn_rate = input$h2o_gbm_learn_rate,
              learn_rate_annealing = input$h2o_gbm_learn_rate_annealing,
              min_rows = input$h2o_gbm_min_rows,
              min_split_improvement = input$h2o_gbm_min_split_improvement,
              histogram_type = input$gbm_histogram_type
            )

            if(!is.null(h2o_df$model)){
              output$h2o_gbm_flag <- reactive("1")
              outputOptions(output, "h2o_gbm_flag", suspendWhenHidden = FALSE)
              output$h2o_gbm_model_text <- renderText(
                paste("GBM output for the",input$models1_select_df, "dataset", sep = " ")
              )

              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              valid_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$valid))
              valid_df <- as.data.frame(h2o_df$valid[, h2o_df$y])
              valid_cm <- caret::confusionMatrix(valid_pred$predict, valid_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_gbm_cm_table  <- function(){
                cm_fun_v(train = train_cm$table, valid = valid_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

              # RMSE plot with validation set
              output$h2o_gbm_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_rmse,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )

              })

              # Classification error plot with validation set
              output$h2o_gbm_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_classification_error,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Logloss plot with validation set
              output$h2o_gbm_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~number_of_trees, y =  ~ validation_logloss,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_gbm_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = "GBM - Variable Importance",
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_gbm_flag <- reactive("0")
              outputOptions(output, "h2o_gbm_flag", suspendWhenHidden = FALSE)
            }

          } else if(input$binomial_models == "h2o_dl"){
            # Deep Learning Model
            h2o_df$model <- NULL

            if(input$h2o_dl_num_hidden == 1){
              hidden <- c(input$h2o_dl_layer1)
            } else if(input$h2o_dl_num_hidden == 2){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2)
            } else if(input$h2o_dl_num_hidden == 3){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2, input$h2o_dl_layer3)
            } else if(input$h2o_dl_num_hidden == 4){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2, input$h2o_dl_layer3, input$h2o_dl_layer4)
            }
            h2o_df$model <- h2o.deeplearning(
              training_frame = h2o_df$train,
              validation_frame = h2o_df$valid,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              hidden = hidden,
              epochs = input$h2o_dl_epochs,
              l1 = input$h2o_dl_l1,
              l2 = input$h2o_dl_l2
            )

            if(!is.null(h2o_df$model)){
              output$h2o_dl_flag <- reactive("1")
              outputOptions(output, "h2o_dl_flag", suspendWhenHidden = FALSE)
              output$dataset_name <- renderText(
                paste("Deep Learning output for the",input$models1_select_df, "dataset", sep = " ")
              )
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              valid_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$valid))
              valid_df <- as.data.frame(h2o_df$valid[, h2o_df$y])
              valid_cm <- caret::confusionMatrix(valid_pred$predict, valid_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_dl_cm_table  <- function(){
                cm_fun_v(train = train_cm$table, valid = valid_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

              # RMSE plot with validation set
              output$h2o_dl_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~ epochs, y =  ~ validation_rmse,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )

              })

              # Classification error plot with validation set
              output$h2o_dl_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~ epochs, y =  ~ validation_classification_error,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )
              })

              # Logloss plot with validation set
              output$h2o_dl_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  add_trace(x = ~ epochs, y =  ~ validation_logloss,
                            type = "scatter", mode = "lines+markers", name = "Validation")%>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_dl_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = "Variable Importance",
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_dl_flag <- reactive("0")
              outputOptions(output, "h2o_dl_flag", suspendWhenHidden = FALSE)
            }

          } else if(input$binomial_models == "h2o_glm"){
            # GLM Model
            h2o_df$model <- NULL

            if(input$h2o_glm_lambda_search){
              lambda_search <- TRUE
              lambda_min_ratio <- input$h2o_glm_lambda_min_ratio
              nlambdas <- input$h2o_glm_nlambdas
            } else {
              lambda_search <- FALSE
              lambda_min_ratio <- NULL
              nlambdas <- NULL
            }

            h2o_df$model <- h2o.glm(
              training_frame = h2o_df$train,
              validation_frame = h2o_df$valid,
              x = h2o_df$x,
              y = h2o_df$y,
              family = h2o_df$binomial,
              alpha = input$h2o_glm_alpha,
              solver = input$h2o_glm_solver,
              max_iterations = input$h2o_glm_max_iterations,
              lambda_search = lambda_search,
              lambda_min_ratio = lambda_min_ratio,
              nlambdas = nlambdas
            )

            if(!is.null(h2o_df$model)){
              output$h2o_glm_flag <- reactive("1")
              outputOptions(output, "h2o_glm_flag", suspendWhenHidden = FALSE)
              output$h2o_glm_model_text <- renderText(
                paste("GLM output for the",input$models1_select_df, "dataset", sep = " ")
              )
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              valid_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$valid))
              valid_df <- as.data.frame(h2o_df$valid[, h2o_df$y])
              valid_cm <- caret::confusionMatrix(valid_pred$predict, valid_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_glm_cm_table  <- function(){
                cm_fun_v(train = train_cm$table, valid = valid_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

            } else {
              output$h2o_glm_flag <- reactive("0")
              outputOptions(output, "h2o_glm_flag", suspendWhenHidden = FALSE)
            }
          }


          # If not using validation
        } else if(!input$h2o_validation){
          splits <- h2o.splitFrame(
            data = h2o_df$df,
            ratios = c(input$h2o_split),
            destination_frames = c("train", "test"), seed = 1234
          )
          h2o_df$train <- splits[[1]]
          h2o_df$test  <- splits[[2]]

          if(input$binomial_models == "h2o_rf"){
            # Random Forest
            h2o_df$model <- NULL

            h2o_df$model <- h2o.randomForest(
              training_frame = h2o_df$train,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              ntrees = input$h2o_rf_ntree,
              max_depth = input$h2o_rf_max_depth,
              histogram_type = input$rf_histogram_type,
              col_sample_rate_change_per_level = input$h2o_rf_col_sample_rate_change_per_level,
              col_sample_rate_per_tree = input$h2o_rf_col_sample_rate_per_tree,
              sample_rate = input$h2o_rf_sample_rate
            )
            if(!is.null(h2o_df$model)){
              output$h2o_rf_flag <- reactive("1")
              outputOptions(output, "h2o_rf_flag", suspendWhenHidden = FALSE)
              output$h2o_rf_model_text <- renderText(
                paste("Random Forest output for the",input$models1_select_df, "dataset", sep = " ")
              )
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])
              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_rf_cm_table <- function(){
                cm_fun(train = train_cm$table, test = test_cm$table)
              }
              # RMSE plot without validation set
              output$h2o_rf_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })


              # Classification error plor without validation set
              output$h2o_rf_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Logloss plot without validation set
              output$h2o_rf_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_rf_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = "Random Forest - Variable Importance",
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_rf_flag <- reactive("0")
              outputOptions(output, "h2o_rf_flag", suspendWhenHidden = FALSE)
            }

          } else if(input$binomial_models == "h2o_gbm"){
            # GBM Model
            h2o_df$model <- NULL

            h2o_df$model <- h2o.gbm(
              training_frame = h2o_df$train,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              ntrees = input$h2o_gbm_ntree,
              max_depth = input$h2o_gbm_max_depth,
              learn_rate = input$h2o_gbm_learn_rate,
              learn_rate_annealing = input$h2o_gbm_learn_rate_annealing,
              min_rows = input$h2o_gbm_min_rows,
              min_split_improvement = input$h2o_gbm_min_split_improvement,
              histogram_type = input$gbm_histogram_type
            )

            if(!is.null(h2o_df$model)){
              output$h2o_gbm_flag <- reactive("1")
              outputOptions(output, "h2o_gbm_flag", suspendWhenHidden = FALSE)
              output$h2o_gbm_model_text <- renderText(
                paste("GBM output for the",input$models1_select_df, "dataset", sep = " ")
              )
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_gbm_cm_table  <- function(){
                cm_fun(train = train_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

              # RMSE plot with validation set
              output$h2o_gbm_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )

              })

              # Classification error plot with validation set
              output$h2o_gbm_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Logloss plot with validation set
              output$h2o_gbm_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss", domain = c(0, 0.95)),
                    xaxis = list(title = "Number of Trees", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_gbm_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = "Variable Importance",
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_gbm_flag <- reactive("0")
              outputOptions(output, "h2o_gbm_flag", suspendWhenHidden = FALSE)
            }


          } else if(input$binomial_models == "h2o_glm"){
            h2o_df$model <- NULL

            if(input$h2o_glm_lambda_search){
              lambda_search <- TRUE
              lambda_min_ratio <- input$h2o_glm_lambda_min_ratio
              nlambdas <- input$h2o_glm_nlambdas
            } else {
              lambda_search <- FALSE
              lambda_min_ratio <- NULL
              nlambdas <- NULL
            }


            h2o_df$model <- h2o.glm(
              training_frame = h2o_df$train,
              x = h2o_df$x,
              y = h2o_df$y,
              family = h2o_df$binomial,
              alpha = input$h2o_glm_alpha,
              solver = input$h2o_glm_solver,
              max_iterations = input$h2o_glm_max_iterations,
              lambda_search = lambda_search,
              lambda_min_ratio = lambda_min_ratio,
              nlambdas = nlambdas
            )

            if(!is.null(h2o_df$model)){
              output$h2o_glm_flag <- reactive("1")
              outputOptions(output, "h2o_glm_flag", suspendWhenHidden = FALSE)
              output$h2o_glm_model_text <- renderText(
                paste("GLM output for the",input$models1_select_df, "dataset", sep = " ")
              )
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_glm_cm_table  <- function(){
                cm_fun(train = train_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

            } else {
              output$h2o_glm_flag <- reactive("0")
              outputOptions(output, "h2o_glm_flag", suspendWhenHidden = FALSE)
            }

          } else if(input$binomial_models == "h2o_dl"){
            # Deep Learning Model
            h2o_df$model <- NULL

            if(input$h2o_dl_num_hidden == 1){
              hidden <- c(input$h2o_dl_layer1)
            } else if(input$h2o_dl_num_hidden == 2){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2)
            } else if(input$h2o_dl_num_hidden == 3){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2, input$h2o_dl_layer3)
            } else if(input$h2o_dl_num_hidden == 4){
              hidden <- c(input$h2o_dl_layer1, input$h2o_dl_layer2, input$h2o_dl_layer3, input$h2o_dl_layer4)
            }

            h2o_df$model <- h2o.deeplearning(
              training_frame = h2o_df$train,
              x = h2o_df$x,
              y = h2o_df$y,
              nfolds = n_folds,
              hidden = hidden,
              epochs = input$h2o_dl_epochs,
              l1 = input$h2o_dl_l1,
              l2 = input$h2o_dl_l2
            )
            if(!is.null(h2o_df$model)){
              output$h2o_dl_flag <- reactive("1")
              outputOptions(output, "h2o_dl_flag", suspendWhenHidden = FALSE)

              output$h2o_dl_model_text <- renderText(
                paste("Deep Learning output for the",input$models1_select_df, "dataset", sep = " ")
              )
              # Setting the confusion matrix output
              train_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$train))
              train_df <- as.data.frame(h2o_df$train[, h2o_df$y])
              train_cm <- caret::confusionMatrix(train_pred$predict, train_df[,1])

              test_pred <- as.data.frame(h2o.predict(
                object = h2o_df$model,
                newdata = h2o_df$test))
              test_df <- as.data.frame(h2o_df$test[, h2o_df$y])
              test_cm <- caret::confusionMatrix(test_pred$predict, test_df[,1])

              output$h2o_dl_cm_table  <- function(){
                cm_fun(train = train_cm$table, test = test_cm$table)
              }
              sh <- as.data.frame(h2o.scoreHistory(h2o_df$model))

              # RMSE plot with validation set
              output$h2o_dl_class_rmse_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_rmse,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "RMSE Score History",
                    yaxis = list(title = "RMSE", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )

              })

              # Classification error plot with validation set
              output$h2o_dl_class_error_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_classification_error,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Classification Error Score History",
                    yaxis = list(title = "Classification Error", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )
              })

              # Logloss plot with validation set
              output$h2o_dl_class_logloss_plot <- plotly::renderPlotly({
                plotly::plot_ly(data = sh, x = ~ epochs, y =  ~ training_logloss,
                        type = "scatter", mode = "lines+markers", name = "Training") %>%
                  plotly::layout(
                    title = "Logloss Score History",
                    yaxis = list(title = "Logloss", domain = c(0, 0.95)),
                    xaxis = list(title = "Epochs", domain = c(0, 0.95))
                  )
              })

              # Variable importance plot
              output$h2o_dl_class_var_imp_plot <- plotly::renderPlotly({
                var_imp <- h2o.varimp(h2o_df$model)
                var_imp <- var_imp[order(var_imp$scaled_importance),]
                var_order <- var_imp$variable
                var_imp$variable <- factor(var_imp$variable, levels = var_order)
                plotly::plot_ly(data = var_imp, y = ~ variable, x = ~ round(scaled_importance,2),
                        type = "bar", orientation = 'h'
                ) %>%
                  plotly::layout(
                    title = "Variable Importance",
                    yaxis = list(title = ""),
                    xaxis = list(title = "Scaled Importance"),
                    margin = list(l = 155)
                  )
              })

            } else {
              output$h2o_dl_flag <- reactive("0")
              outputOptions(output, "h2o_dl_flag", suspendWhenHidden = FALSE)
            }

          }

        }
      }
    } else {
      showModal(modalDialog(
        title = "Error - Independent Variable is Missing",
        HTML(paste("The independent variable is missing,",
                   "please select the independent variable/s to continue.",
                   sep = "<br/>")
        ), size = "s"
      ))
    }
  })



  #------------------------------ Server Function - End -------------------------------------
}

