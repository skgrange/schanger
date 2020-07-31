#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", ".draw", "draw", ".chain", "chain", ".iteration", 
    "value_predict", "value", "id", "n"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
