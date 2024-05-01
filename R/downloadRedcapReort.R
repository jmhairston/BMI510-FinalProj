#' Download REDCap Report
#'
#' This function downloads a report from REDCap using API parameters and returns it as a tibble.
#' The given API token is read from an environment variable for security.
#'
#' @param redcapTokenName The name of the environment variable storing the REDCap API token.
#' @param redcapUrl The base URL of the REDCap API.
#' @param redcapReportId The identifier for the specific REDCap report to download.
#' @importFrom httr POST
#' @importFrom readr read_csv
#' @importFrom dplyr as_tibble
#' @return A tibble containing the downloaded REDCap report.
#' @export
#' @examples
#' # Ensure that REDCap API token is stored in .Renviron as "MY_REDCAP_TOKEN"
#' downloadRedcapReport("MY_REDCAP_TOKEN", "https://redcap.example.com/api/", "12345")
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  require(httr)
  require(readr)
  require(dplyr)

  # Retrieve the API token from environment
  api_token = Sys.getenv(redcapTokenName)
  if (api_token == "") stop("API token is not set in environment variables.")

  form_data <- list(token = api_token,
    content = 'report',
    format = 'csv',
    report_id = redcapReportId,
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )
  
  response <- httr::POST(url = redcapUrl, body = form_data, encode = "form")
  content <- httr::content(response)
  result = read_csv(content) %>% as_tibble()
  
  return(result)
}
