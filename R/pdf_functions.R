#' Gets list of indices between specified indicies
#'
#' @description This is a function that will get all in between values of what needs to be a vector.
#' Maybe at some point I'll figure out how to include other object types, but maybe that's not all that
#' essential anyway.
#'
#' @param vector Desired vector to get
#' @param indices Specific indices to calculate in between values
#' @param include_start Includes the first index, even if not in the selected indices
#' @param include_end Includes the last index, even if not in the selected indices
#'
#' @details The power of this function is getting sequencing between selected values. In the past,
#' I've used this to sequence things such as pdf's which often have different titles that I either
#' need to remove or append.
#'
#' Keep in mind that this currently only works with vectors and only exports lists of what the sequences
#' actually are.
#'
#' @export
get_in_between <- function(vector, indices, include_start = T, include_end = T) {

  ### This doesn't currently work for any other objects other than vectors,
  ### since this was pretty much built out for pdf extraction anyway
  if(!is.vector(vector)) stop('Object must be a vector')

  ### We need the index values to be numerics as well, or it just doesn't make any sense
  if(!is.numeric(indices)) return(stop('Indicies must be numeric'))

  ### Indices need to be strictly numerics
  if(NA %in% indices) return(stop('Indices can not include NA values'))

  ### Requires include_start and include_end to be logical
  if(!is.logical(include_start) | !is.logical(include_end)) return(stop('Start and end must be logical'))

  ### Gets a vector of all rows
  full_index <- 1:length(vector)

  ### Doesn't allow for bad index selection
  if(any(indices > length(vector))) return(stop('Indicies can not exceed vector length'))

  ### Creates an empty list for the intervals
  interval_list <- list()

  ### Saves so the base argument isn't overwritten and makes sure the ind
  new_indices <- indices[order(indices)]

  ### This will include the end value, only if desired
  if(include_end & !length(list) %in% new_indices) new_indices <- c(new_indices, length(vector) + 1)

  ### This will include the first value, only if desired
  if(include_start & !0 %in% new_indices) new_indices <- c(0, new_indices)

  ### The goal is to get a list of inbetween values, so if only one index is listed,
  ### This won't make any sense
  if(length(new_indices) < 2) return(stop('Index selection must be greater than 1'))

  ### Iterates across the number of indexes
  for (j in 1:(length(new_indices) - 1)) {

    ### Starts attaching multiple intexes to a list
    lower <- new_indices[j] + 1
    higher <- new_indices[j + 1] - 1

    ### This prevents the function from returning values that would
    ### otherwise result by indices put sequentially together
    if(lower > higher) next

    ### Makes sure that a valid, non infinite interval, is selected
    new_interval <- tryCatch(lower:higher, error = function(e) e)

    ### Exits if there is an issue creating the sequence
    if(inherits(new_interval, 'error')) return(stop('Invalid Index Selection'))

    ### Attaches the new interval to the list. Only appends new ranges
    interval_list[[length(interval_list) + 1]] <- new_interval

  }

  ### Returns the list of intervals
  interval_list

}

#' Fixes spacing issues
#'
#' @description Adjusts multi-spaces to be condensed to single spaces and removes
#' any spaces that would originate at the beginning or end of a vector
#'
#' @param vector String vector to have spacing corrected
#'
#' @details Not anything super-fancy, but in a pinch, this function can remove
#' the extra spaces that are often given when removing pdf information. Often, there
#' are also spaces listed at the beginning or the end which we want to remove, and
#' this does that as well
#'
#' @export
fix_spaces <- function(vector) {

  ### I don't want to overwrite the pdf
  new_vector <- vector

  ### Fixes duplicate spaces
  new_vector <- gsub('\\s+', ' ', new_vector)

  ### Removes beginning and end spaces
  new_vector <- gsub('^\\s|\\s$', '', new_vector)

  ### Returns the new list with better
  new_vector

}
