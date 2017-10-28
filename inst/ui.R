#------------------------------ UI Function -------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "ML Studio"),
#------------------------------ Side Bar Function -------------------------------------
  dashboardSidebar(
    sidebarMenu(id = "tabs", 
      menuItem("Data", tabName = "data", icon = icon("table"), startExpanded = TRUE,
               menuSubItem("Load", tabName = "data1"),
               menuSubItem("Prep", tabName = "data2")
               ),
      menuItem("Visualization", icon = icon("bar-chart-o"), tabName = "vis"),
      menuItem("Models", icon = icon("cog"), tabName = "models",
               menuSubItem("Regression & Classification", tabName = "models1")
               )
    )
  ),
#------------------------------ Dashboard Body -------------------------------------
  dashboardBody(
#------------------------------ Tags Style -------------------------------------    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
#------------------------------ Tabs Start -------------------------------------     
tabItems(
#------------------------------ Tabs Data Start-------------------------------------
tabItem(tabName = "data1",
#------------------------------ Tabs Data - fluid page start -------------------------------------
fluidPage(
#------------------------------ Tabs Data - fluid row 1 -------------------------------------
  fluidRow(
    infoBoxOutput("installed_datasets"),
    infoBoxOutput("in_memory_df"),
    infoBoxOutput("load_datasets")
  ),
#------------------------------ Tabs Data - fluid row 2 -------------------------------------
  fluidRow(
    box(
      width = 4, height = 100,
      selectInput('data_source', 'Select Data Source', 
                  list(
                       "R Data Frame" = "data_frame",
                       "R Time Series" = "time_series",
                       "Installed Package Dataset" = "inst_pack",
                       "Import CSV File" = "import"
                       ))
    ),
    box(width =  4, height = 100,
        conditionalPanel(condition = "input.data_source.includes('data_frame') || input.data_source.includes('inst_pack') || input.data_source.includes('time_series')",
                        uiOutput("df_list")
        ),
        conditionalPanel(condition = "input.data_source == 'import'",
                         
                         
                         dropdownButton(
                           fileInput('file1', 'Choose CSV File',
                                     accept=c('text/csv', 
                                              'text/comma-separated-values,text/plain', 
                                              '.csv')),
                           awesomeCheckbox(inputId = "csv_header", 
                                           label = "Header", 
                                           value = TRUE),
                           radioButtons('sep', 'Separator',
                                        c(Comma=',',
                                          Semicolon=';',
                                          Tab='\t'),
                                        ','),
                           radioButtons('quote', 'Quote',
                                        c(None='',
                                          'Double Quote'='"',
                                          'Single Quote'="'"),
                                        '"'),
                           circle = TRUE, status = "danger", 
                           icon = icon("file-text", lib = "font-awesome"), width = "300px",
                           tooltip = tooltipOptions(title = "Click to set csv file parameters !")
                         )
                         )
                         
    ),
    box(width =  4, height = 100,
        conditionalPanel(condition = "(output.load_flag == '2' && input.data_source == 'inst_pack') ||  (output.load_flag == '2' && input.data_source == 'import' ) || (output.load_flag == '1' && input.data_source == 'data_frame' ) || ( output.load_flag == '1' && input.data_source == 'inst_pack') || (output.load_flag == '1' && input.data_source == 'time_series')",
      actionButton("load", "Load")
        ),
      conditionalPanel(condition =  "output.loaded_table_flag == '1'",
      actionButton("remove", "Remove")
      )
    )
  ),
fluidRow(
  box(width = 7, title = "Preview Table", 
      div(style = 'overflow-x: scroll',
  DT::dataTableOutput('view_table'))
  ),
  
  box(width = 5, title = "Loaded Datasets",
      div(style = 'overflow-x: scroll',
          DT::dataTableOutput('list_loaded_df'))
  )
)




#------------------------------ Tabs Data - fluid row 2 -------------------------------------
)
#------------------------------ Tabs Data - fluid page end -------------------------------------

),
#------------------------------ Tabs Data2 Start-------------------------------------
tabItem(tabName = "data2",
        fluidPage(
        fluidRow(
          conditionalPanel(condition =  "output.loaded_table_flag == '1'",
          infoBoxOutput("data_name"),
          infoBoxOutput("num_var"),
          infoBoxOutput("num_obs")
          )
        ),
        fluidRow(
          conditionalPanel(condition =  "output.loaded_table_flag == '1'",
          box(width = 4, title = "Select Dataset",
              uiOutput("loaded_ds_list"),
              conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false ",
              selectInput('data_option', 'Select Option', 
                          list(
                            "Variables Attributes" = "var_attr",
                            "Data Summarise" = "data_summary"
                          ))
              ),
              conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
              radioButtons("class_selection", label = "Variables Modification", 
                           choices = list(Numeric = "numeric", Factor = "factor", 
                                          Character = "character",
                                          Date = "date"),
                           selected = "numeric"),
              conditionalPanel(condition =  "input.class_selection == 'date' && output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
                               selectInput('date_format', "Select the Date Format",
                                           list(
                                             YMD = "ymd",
                                             YDM = "ydm",
                                             MYD = "myd",
                                             MDY = "mdy",
                                             DMY = "dmy",
                                             DYM = "dym"
                                           )),
                               #titlePanel(h5("Date Preview")),
                               tags$h5("Date Preview"),
                               verbatimTextOutput("date_prev")
                               ),
              actionButton("var_modify", "Modify")
              ),
              conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == true ",
                               tableOutput("ts_table")             
              )
              
            )),
          conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
          box(width = 4, title = "List of Variables",
              DT::dataTableOutput("data_tab2_var")
            
          ),
          box(width = 4, title = "Variable Summary",
              plotlyOutput("data_tab2_summary_plot",height = 200),
              tableOutput("data_tab2_var_summary")
          )
          ),
          conditionalPanel(condition = "output.loaded_table_flag == '1' && input.data_option == 'data_summary'",
                           box(width = 4, title = "Create a Summary",
                               uiOutput("group_by"),
                               conditionalPanel(condition = "output.loaded_table_flag == '1' && input.data_option == 'data_summary' && output.group_by_flag == '1'",
                                                uiOutput("summarise_var")
                                                
                                              ),
                               conditionalPanel(condition = "output.loaded_table_flag == '1' && input.data_option == 'data_summary' && output.group_by_flag == '1'",
                                                uiOutput("dplyr_fun"),
                                                uiOutput("summary_name"),
                                                actionButton("run_summary", "Run")
                               )                 
                               ),
                           conditionalPanel(condition = "output.dplyr_table_flag == '1'  && input.data_option == 'data_summary' && output.group_by_flag == '1'",
                           box(width = 4, title = "Summary",
                               div(style = 'overflow-x: scroll',
                               DT::dataTableOutput('dplyr_table')
                               )
                               )
                           )
                          ),
          
          conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == true ",
                           box(width = 8, title = "Time Series Plot",
                               dropdownButton(
                                 tags$h3("List of Input"),
                                 materialSwitch(inputId = "ts_plot_log", label = "Log Transformation", 
                                                status = "primary", right = FALSE),
                                 awesomeRadio(inputId = "ts_prep_mode", 
                                              label = "Radio buttons", 
                                              choices = c("lines","lines+markers", "markers")
                                              , selected = "lines"),
                                 circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",
                                 tooltip = tooltipOptions(title = "Plot Setting")
                               ),
                               plotlyOutput("data_tab2_ts")
                               
                           )
                           )
        
        ),
        
        fluidRow(
          conditionalPanel(condition = "output.loaded_table_flag == '1'",
                           div(style = 'overflow-x: scroll',
                               DT::dataTableOutput("data_tab2_table")) 
                           )
        )
        )
    
  ),
#------------------------------ Tabs Data End-------------------------------------
#------------------------------ Tabs Visualization Start-------------------------------------

tabItem(tabName = "vis",
        conditionalPanel(condition =  "output.loaded_table_flag == '1'",
        fluidPage(
          fluidRow(
            box(width = 2,
                uiOutput("loaded_ds_list_vis"),
                uiOutput("vis_plot_type")
                
               ),
            conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag_vis == false && input.plot_type != 'cor' ",
            box(width = 2,
                conditionalPanel(condition = "input.plot_type != 'density' && input.plot_type != 'box' && input.plot_type != 'hist'",
                  uiOutput("vis_x"),
                  uiOutput("vis_y")
                ),
                conditionalPanel(condition = "input.plot_type == 'density' || input.plot_type == 'hist' || input.plot_type == 'box'",
                 uiOutput("vis_one_var")
                                 )
                ),
            conditionalPanel(condition = "input.plot_type == 'scatter' || input.plot_type == 'box'|| input.plot_type == 'hist' || input.plot_type == 'density'",
            box(width = 2,
                uiOutput("vis_factor"))
            )
            )
          ),
          fluidRow(
            box(width = 12, title = "plot",
                withSpinner(plotlyOutput("main_plot"))
                
                )
          )
        )
        )
),
#------------------------------ Tabs Visualization End-------------------------------------
#------------------------------ Tabs Classification Start-------------------------------------
tabItem(tabName = "models1",
        fluidRow(conditionalPanel(condition = "input.model_package.includes('H2O')",
                                  infoBoxOutput("h2o_status_box"),
                                  infoBoxOutput("h2o_cpu"),
                                  infoBoxOutput("h2o_cluster_mem")  
                                  )
        ),
        fluidRow(
                           box(width = 2, title = "Model Inputs",

                               conditionalPanel(condition = "input.model_package.includes('H2O')",
                               uiOutput("models1_df_list"),
                               conditionalPanel(condition = "output.model_tab_input == '1' || output.model_tab_input == '2'",
                                                uiOutput("models1_var_list"),
                                                uiOutput("models1_independent_list")
                               )
                               ),
                               awesomeCheckboxGroup(inputId = "model_package", 
                                                    label = "Set Packages", 
                                                    choices = c("H2O"), selected = NULL, 
                                                    inline = TRUE)
        
        ),
        conditionalPanel(condition = "output.dep_var_class == '1' && output.h2o_flag == '1'",
        tabBox(
          title = "Model Setting & Output", width = 10,
          id = "class_setting", height = "500px",
         
          tabPanel("Model Setting",
                   fluidRow(
                   box(width = 3, title = "Model Setting",
                   selectInput("binomial_models", "Select Classification Model",
                               choices = c("Deep Learning (H2O)" = "h2o_dl",
                                           "GLM (H2O)" = "h2o_glm",
                                           "GBM (H2O)" = "h2o_gbm",
                                           "Random Forest (H2O)" = "h2o_rf")
                   ),
                   materialSwitch(inputId = "h2o_validation", 
                                  label = "Add Validation Partition", 
                                  status = "primary", right = FALSE),
                   conditionalPanel(condition = "input.h2o_validation == true",
                                    sliderInput("h2o_split_v", "Set the Training/Testing/Validation Partitions:",
                                                min = 0.05, max = 1,
                                                value = c(0.6,0.8))),
                   conditionalPanel(condition = "input.h2o_validation == false",
                                    sliderInput("h2o_split", "Set the Training/Testing Partitions:",
                                                min = 0.05, max = 1,
                                                value = 0.7)),
                   materialSwitch(inputId = "nfolds_flag", 
                                  label = "N-fold Cross-Validation", 
                                  status = "primary", right = FALSE),
                   conditionalPanel(condition = "input.nfolds_flag == true",
                                    sliderInput("nfolds", "Set the Number of folds:",
                                                min = 3, max = 10, step = 1,
                                                value = 5))
                   ),
                   box(width = 3, title = "Model Tuning",
                   conditionalPanel( condition = "input.binomial_models == 'h2o_rf'",
                                     dropdownButton(
                                       tags$h4("More Tunning Parameters"),
                                       selectInput("rf_histogram_type", "Optimal Split Histogram Type",
                                                   choices = c("AUTO" = "AUTO",
                                                               "UniformAdaptive" = "UniformAdaptive",
                                                               "Random" = "Random",
                                                               "QuantilesGlobal" = "QuantilesGlobal",
                                                               "RoundRobin" = "RoundRobin"
                                                   ),
                                                   selected = "AUTO"
                                       ),
                                       sliderInput("h2o_rf_col_sample_rate_change_per_level", "Column Sample Rate Change Per Level",
                                                   min = 0, max = 2,
                                                   value = 1, step = 0.01
                                       ),
                                       sliderInput("h2o_rf_col_sample_rate_per_tree", "Column Sample Rate Per Tree",
                                                   min = 0, max = 1,
                                                   value = 1, step = 0.01
                                       ),
                                       sliderInput("h2o_rf_sample_rate", "Row Sampling Rate",
                                                   min = 0, max = 1,
                                                   value = 0.632, step = 0.01
                                       ),
                                       circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "More Tunning ")
                                     ),
                                     sliderInput("h2o_rf_ntree", "Number of Trees",
                                                 min = 25, max = 1000,
                                                 value = 50, step = 25),
                                     sliderInput("h2o_rf_max_depth", "Maximum Tree Depth",
                                                 min = 1, max = 30,
                                                 value = 20
                                     )
                                     
                                    ),
                   conditionalPanel( condition = "input.binomial_models == 'h2o_gbm'",
                                     dropdownButton(
                                       tags$h4("More Tunning Parameters"),
                                       selectInput("gbm_histogram_type", "Optimal Split Histogram Type",
                                                   choices = c("AUTO" = "AUTO",
                                                               "UniformAdaptive" = "UniformAdaptive",
                                                               "Random" = "Random",
                                                               "QuantilesGlobal" = "QuantilesGlobal",
                                                               "RoundRobin" = "RoundRobin"
                                                               ),
                                                   selected = "AUTO"
                                       ),
                                       sliderInput("h2o_gbm_learn_rate", "Learning Rate",
                                                   min = 0, max = 1,
                                                   value = 0.1
                                       ),
                                       sliderInput("h2o_gbm_learn_rate_annealing", "Learning Rate",
                                                   min = 0, max = 1,
                                                   value = 1
                                       ),
                                       sliderInput("h2o_gbm_min_rows", "Min. Rows",
                                                   min = 5, max = 20,
                                                   value = 1
                                       ),
                                       sliderInput("h2o_gbm_min_split_improvement", "Min. Split Improvement",
                                                   min = 1e-10, max = 1e-3,
                                                   value = 1e-10
                                       ),
                                       circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "More Tunning ")
                                     ),
                                     sliderInput("h2o_gbm_ntree", "Number of Trees",
                                                 min = 25, max = 1000,
                                                 value = 50, step = 25),
                                     sliderInput("h2o_gbm_max_depth", "Maximum Tree Depth",
                                                 min = 1, max = 30,
                                                 value = 5
                                     )
                                     
                                     
                                     
                   ),
                   fluidRow(
                   conditionalPanel( condition = "input.binomial_models == 'h2o_dl'",
                                     fluidRow(
                                     column(width = 2, 
                                     dropdownButton(
                                       tags$h4("Layer Setting"),
                                       sliderInput("h2o_dl_num_hidden", "Number of Hidden Layers",
                                                   min = 1, max = 4, 
                                                   value = 2, step = 1
                                       ),
                                       conditionalPanel(condition =  "input.h2o_dl_num_hidden == 1",
                                                        sliderInput("h2o_dl_layer1", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        )
                                                        ),
                                       conditionalPanel(condition =  "input.h2o_dl_num_hidden == 2",
                                                        sliderInput("h2o_dl_layer1", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer2", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        )
                                       ),
                                       conditionalPanel(condition =  "input.h2o_dl_num_hidden == 3",
                                                        sliderInput("h2o_dl_layer1", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer2", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer3", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        )
                                       ),
                                       conditionalPanel(condition =  "input.h2o_dl_num_hidden == 4",
                                                        sliderInput("h2o_dl_layer1", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer2", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer3", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        ),
                                                        sliderInput("h2o_dl_layer4", "Number of Hidden Layers",
                                                                    min = 1, max = 1000,
                                                                    value = 200, step = 1
                                                        )
                                       ),
                                       
                                       circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "Layer Setting ")
                                     )),
                                     column(width = 2, offset = 2, 
                                     dropdownButton(
                                       tags$h4("Layer Setting"),
                                       circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "Early Stop")
                                     )))
                                     ),
                                     sliderInput("h2o_dl_epochs", "Number of Epochs",
                                                 min = 1, max = 10000,
                                                 value = 10, step = 1),
                                     sliderInput("h2o_dl_l1", "L1 Regularization",
                                                 min = 0, max = 1,
                                                 value = 0, step = 1e-5
                                     ),
                                     sliderInput("h2o_dl_l2", "L2 Regularization",
                                                 min = 0, max = 1,
                                                 value = 0, step = 1e-5
                                     )
                                     
                   ),
                   
                   conditionalPanel( condition = "input.binomial_models == 'h2o_glm'",
                                     dropdownButton(
                                       tags$h4("More Tunning Parameters"),
                                       selectInput("glm_solver", "Solver",
                                                   choices = c("AUTO" = "AUTO",
                                                               "IRLSM" = "IRLSM",
                                                               "L_BFGS" = "L_BFGS",
                                                               "COORDINATE_DESCENT" = "COORDINATE_DESCENT",
                                                               "COORDINATE_DESCENT_NAIVE" = "COORDINATE_DESCENT_NAIVE"
                                                   ),
                                                   selected = "AUTO"
                                       ),
                                       sliderInput("h2o_glm_max_iterations", "Solver Max Iterations",
                                                   min = 10, max = 1000,
                                                   value = 50, step = 10),
                                       circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                       tooltip = tooltipOptions(title = "More Tunning ")
                                     ),
                                     sliderInput("h2o_glm_alpha", "Alpha Parameter",
                                                 min = 0, max = 1,
                                                 value = 0.5, step = 0.01), 
                                     materialSwitch(inputId = "h2o_glm_lambda_search", 
                                                    label = "Lambda Search", 
                                                    status = "primary", right = FALSE,
                                                    value = TRUE),
                                     conditionalPanel(condition = "input.h2o_glm_lambda_search == true",
                                                      sliderInput("h2o_glm_lambda_min_ratio", "Lambda Min. Ratio",
                                                                  min = 0.0001, max = 0.001,
                                                                  value = 0.0001, step = 0.0001), 
                                                      sliderInput("h2o_glm_nlambdas", "Number of Lambdas",
                                                                  min = 10, max = 200,
                                                                  value = 100, step = 1) 
                                                      )
                                     
                   ),
                   actionButton("h2o_run_class", "Run Model")
                      ),
                   box(width = 3, title = "Confusion Matrix",
                       tableOutput("cm_table"))
                   )
                   ),
          tabPanel("Variable Importance", withSpinner(plotlyOutput("var_imp_plot"))),
          conditionalPanel( condition = "input.binomial_models == 'h2o_glm'",
          tabPanel("RMSE", withSpinner(plotlyOutput("rmse_plot")))),
          tabPanel("Classification Error", withSpinner(plotlyOutput("classification_error_plot"))),
          tabPanel("Logloss", withSpinner(plotlyOutput("logloss_plot")))
          
        )
      )
    )

)
#------------------------------ Tabs Classification End-------------------------------------


)
)
)
