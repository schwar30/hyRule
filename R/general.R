#' Logical Length 0
#'
#' @description This function simply returns a logical of whether or not the length
#'     of an object is equal to zero. Can be used for any single dimensional object.
#'
#' @param x A one-dimensional object to test length
#'
#' @details Often the only reason we care about length of an object is to check 
#'    whether the object has any dimensions, as many times a length of 0 is more
#'    the result of either an error or lack of data. Returning a logical for length
#'    helps make conditionals much more streamlined.
#'
#' @export
len0 <- function(x) length(x) == 0

#' Logical Number of Row 0
#'
#' @description This function simply returns a logical of whether or not the number
#'     of rows of a data.frame or matrix is equal to zero. Must be a data.frame or
#'     matrix to make sense.
#'     
#' @param x A matrix or data.frame to test number of rows
#'
#' @details Often the only reason we care about the number of rows a data.frame has
#'     (or technically a matrix) is to see if there is actually any data in the object.
#'     If there is not any data, it doesn't make sense to continue processes generally,
#'     so returning a logical for nrow helps make conditionals more streamlined
#'
#' @export
nrow0 <- function(x) nrow(x) == 0

#' Logical Inherits Error
#'
#' @description This function simply returns a logical of whether or not an object
#'     resulted in an error, whether it be from the use of [try] or [tryCatch].
#'     
#' @param x An object to test for whether an error occured
#'
#' @details There are many times when we want to determine if a try-error or a 
#'     standard error occured. This helps make conditionals more streamlined, 
#'     but needs to be paired with the functions that will create objects with
#'     an error class, such as [try] and [tryCatch] 
#'
#' @export
is_error <- function(x) inherits(x, "try-error") | inherits(x, "error")

#' Transform Data Frame
#'
#' @description Similar to the standard [t] function except that it preserves the data.frame.
#'     [t] has the nasty consequence of transforming a data.frame into a matrix, which is 
#'     often not desired.
#'     
#' @param x A data.frame object, typically.
#' @param strings_as_factors Logical value for making strings factors. Default to FALSE.
#'
#' @details One of the most important abilities with data frames is to swap its dimensions.
#'     However, when the dimensions are swapped via [t], the object is always switched to a
#'     matrix, which isn't always desirable. This function simplifies the process by transforming,
#'     forcing into a data.frame, and then removes any uselsess row names that would otherwise
#'     be associated with the process
#'
#' @export
trans_data_frame <- function(x, strings_as_factors = F) {
  
  ### Transforms a data.frame() to the transformed matrix,
  ### then returns back to a data frame.
  new_frame <- data.frame(t(data.frame(x)), stringsAsFactors = strings_as_factors)
  
  ### Removes the placeholder row names
  rownames(new_frame) <- NULL
  
  ### Returns the new data.frame
  new_frame
  
}

#' Converts to Numeric
#'
#' @description This function converts a value into class numeric. The main difference 
#'     between this function and the standard [as.numeric] function is that this function
#'     suppresses the warning associated with values being converted into NA.
#'     
#' @param x A value or string to be converted to numeric
#'
#' @details RSelenium often crashes when there is an incorrect chrome association, so this
#' is a great helper function that allows us to more quickly and accurately guess chrome
#' versions without too much pain.
#'
#' @export
silent_as_numeric <- function(x) {
  
  ### Suppresses the NA warning 
  suppressWarnings(as.numeric(x))
  
}