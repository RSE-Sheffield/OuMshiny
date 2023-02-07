get_colspec <- function() {

  column_spec <- readr::cols_only(
    arguments = "c",
    argument_position = "f",
    arguer_position = "f",
    rater_position = "f",
    response_ratings = "i",
    argument_id = "i",
    argument_index = "i",
    condition = "f"
  )

  return(column_spec)
}
