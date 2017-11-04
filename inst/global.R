#------------------------------ Setting the Pipe Operator -------------------------------------
`%>%` <- magrittr::`%>%`
#------------------------------ Loading Functions -------------------------------------
col_num <- function(df){
  if(ncol(df)%%3 !=0){
    x <- ncol(df)%/%3 +1
  } else {x <- ncol(df)%/%3}
  return (x)
}

# Checking the available memory (RAM)
get_free_ram <- function(){
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.integer(x)
  } else {
    stop("Only supported on Windows OS")
  }
}
#------------------------------ Confusion Matrix Function -------------------------------------

cm_fun <- function(train, test){
  cm_train <- cm_test <- data.frame(matrix(NA, ncol = dim(train)[1], nrow = dim(train)[2]))
  for(i in 1:(dim(train)[1])){
    cm_train[i,] <- train[i,]
    cm_test[i,] <- test[i,]
  }
  colnames(cm_train) <- colnames(train)
  colnames(cm_test) <- colnames(test)

  cm_train <- data.frame(rownames(train), cm_train)
  names(cm_train) <- c("Prediction", colnames(train))
  cm_test <- data.frame(rownames(test), cm_test)
  names(cm_test) <- c("Prediction", colnames(test))
  cm_df <- rbind(cm_train, cm_test)
  colnames(cm_df) <- c("Prediction", colnames(train))


  options(knitr.table.format = "html")
  table_out <- knitr::kable(cm_df) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE) %>%
    kableExtra::group_rows("Training Set", 1, dim(train)[1]) %>%
    kableExtra::group_rows("Testing Set", (dim(train)[1] + 1), dim(train)[1] * 2) %>%
    kableExtra::add_header_above(c("", "Reference" = dim(train)[1] ))
  return(table_out)
}


cm_fun_v <- function(train, test, valid){
  cm_train <- cm_test <- cm_valid <- data.frame(matrix(NA, ncol = dim(train)[1], nrow = dim(train)[2]))
  for(i in 1:(dim(train)[1])){
    cm_train[i,] <- train[i,]
    cm_test[i,] <- test[i,]
    cm_valid[i,] <- valid[i,]
  }
  colnames(cm_train) <- colnames(train)
  colnames(cm_test) <- colnames(test)
  colnames(cm_valid) <- colnames(valid)

  cm_train <- data.frame(rownames(train), cm_train)
  names(cm_train) <- c("Prediction", colnames(train))
  cm_valid <- data.frame(rownames(valid), cm_valid)
  names(cm_valid) <- c("Prediction", colnames(valid))
  cm_test <- data.frame(rownames(test), cm_test)
  names(cm_test) <- c("Prediction", colnames(test))
  cm_df <- rbind(cm_train, cm_valid, cm_test)
  colnames(cm_df) <- c("Prediction", colnames(train))


  options(knitr.table.format = "html")
  table_out <- knitr::kable(cm_df) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE) %>%
    kableExtra::group_rows("Training Set", 1, dim(train)[1]) %>%
    kableExtra::group_rows("Validation Set", (dim(train)[1] + 1), dim(train)[1] * 2) %>%
    kableExtra::group_rows("Testing Set", (dim(train)[1] * 2 + 1), dim(train)[1] * 3) %>%
    kableExtra::add_header_above(c("", "Reference" = dim(train)[1] ))
  return(table_out)
}
#------------------------------ Accuracy Matrix Function -------------------------------------

accuracy_fun <- function(train, test){
  a_df <- data.frame(matrix(0, ncol = 2, nrow = 4))
  names(a_df) <- c("Training", "Testing")
  a_df[1,] <- c(train$overall[1], test$overall[1])
  a_df[2,] <- c(paste("[", round(train$overall[3], 2), ", ", round(train$overall[4], 2), "]", sep = "") ,
                paste("[", round(test$overall[3], 2),", ", round(test$overall[4], 2), "]", sep = ""))
  a_df[3,] <- c(train$overall[2], test$overall[2])
  a_df[4,] <- c(train$overall[2], test$overall[2])
}
#------------------------------ Creating list of the installed packages datasets -------------------------------------
packages.list <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
d <- data(package = packages.list$Package)

dataset.df <- data.frame(package = d$results[,"Package"], dataset =  d$results[,"Item"] ,
                         space = regexpr(" ",d$results[,"Item"]),
                         stringsAsFactors = FALSE )

dataset.df$dataset.fixed <- ifelse(dataset.df$space != -1,
                                   substr(dataset.df$dataset, 1, (dataset.df$space - 1)),
                                   dataset.df$dataset)


installed_datasets <- as.list(paste(dataset.df$package,"-" ,dataset.df$dataset.fixed, sep = " "))
rm(dataset.df, d, packages.list)
