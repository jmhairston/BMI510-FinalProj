#' Standardize Column Names to smallCamelCase
#'
#' This function takes a tibble and standardizes its variable names to smallCamelCase
#' using `dplyr::rename_with`, `janitor::make_clean_names`, and `snakecase::to_small_camel_case`.
#' @param data A tibble whose column names are to be standardized.
#' @importFrom dplyr rename_with
#' @importFrom janitor make_clean_names
#' @importFrom snakecase to_small_camel_case
#' @return A tibble with standardized column names.
#' @export
#' @examples
#' library(tibble)
#' data = tibble(`Column one` = 1:5, `COLUMN TWO` = letters[1:5])
#' standardized_data = standardizeNames(data)
#' names(standardized_data)
standardizeNames = function(data) {
  require(dplyr)
  require(janitor)
  require(snakecase)

  data %>%
    dplyr::rename_with(~ snakecase::to_small_camel_case(janitor::make_clean_names(.)))
}
