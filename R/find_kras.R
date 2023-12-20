#' Determine the KRAS Mutation Status
#'
#' This function analyzes a vector representing biological measurement markers (BMMs) 
#' to determine the KRAS mutation status. It categorizes the status into 'Mutant', 
#' 'Wild-type', or 'Unknown' based on the frequency of each type in the input vector.
#'
#' @param bmmtr A character vector representing biological measurement markers.
#' Each element should be one of "Wild-type", "Mutant", "Unknown", or "Failure".
#'
#' @return A character string indicating the KRAS mutation status. 
#' It returns 'Mutant' if any 'Mutant' is present in the vector. 
#' If no 'Mutant' is found, it compares the count of 'Wild-type' with the sum of 
#' 'Failure' and 'Unknown'. If 'Wild-type' count is higher, it returns 'Wild-type', 
#' otherwise, it returns 'Unknown'.
#'
#' @examples
#' find_kras(c("Wild-type", "Wild-type", "Unknown"))
#' find_kras(c("Mutant", "Unknown", "Failure"))
#' find_kras(c("Wild-type", "Failure", "Failure"))
#'
#' @export

find_kras <- function(bmmtr) {
  wild_num <- sum(bmmtr == "Wild-type")
  unknown_num <- sum(bmmtr == "Unknown")
  failure_num <- sum(bmmtr == "Failure")
  if ("Mutant" %in% bmmtr) {
    return("Mutant")
  } else if (wild_num > (failure_num + unknown_num)) {
    return("Wild-type")
  } else {
    return("Unknown")
  }
}