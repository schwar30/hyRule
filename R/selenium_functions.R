#' Gets List of Relevant Chrome Versions
#'
#' @description This is a helper function to be used alongside RSelenium. Returns a list of
#'     available chrome versions on the user's computer
#'
#' @details RSelenium often crashes when there is an incorrect chrome association, so this
#'     is a great helper function that allows us to more quickly and accurately guess chrome
#'     versions without too much pain.
#'
#' @export
get_chrome_list <- function() {

  ### Checks to see if chrome is installed
  chromeplat <- wdman:::chrome_check(verbose = T)[['platform']]

  ### Actually gets the chrome versions
  raw_chrome_ver <- binman::list_versions('chromedriver')[[chromeplat]]

  ### Filters out some junk versions that don't actually exist
  raw_chrome_ver <- raw_chrome_ver[grepl('^.*\\..*\\..*\\..*$', raw_chrome_ver)]

  ### Returns the list of chrome versions
  raw_chrome_ver

}
