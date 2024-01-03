#'count rows Analysis set
#'
#'Function to check if the number of rows of a data frame matches
#'the specified number of rows.
#'@param Dataset The dataset to be tested
#'@param ScriptName Name The script name in which this function is executed. This one
#'filename will be placed in message if number of rows is not equal
#'@param Number_lines_begin The number of rows to calculate any difference
#'@param type The name of the data file
#'@param slack If TRUE(default), the message is also sent to Slack.
#'@export
count_rows_analysis_set <- function(Dataset, ScriptName, Number_lines_begin, type = "Analysisset", slack = T) {
  if(type == "Analysis set"){
    msg <- "Analysis set does not have the same number of rows after adding new data. Check script: "
  } else if(type == "Export") {
    msg <- "Analysis set does not have the same number of rows after export build. Check script: "
  } else {
    msg <- "Unknown call type; check call function count_rows_analysis_set()"
  }

  ## Calculate the difference
  Number_lines_end <- nrow(Dataset)
  Difference <- Number_lines_end - Number_lines_begin
  if(Difference != 0){
    if((slack == T) & ("slackr" %in% rownames(utils::installed.packages())) ){
      slackr::slackr_msg(c(msg, ScriptName, "difference is: ", Difference))
    }
    stop(paste(msg, ScriptName, "difference is: ", Difference))
  }
}
