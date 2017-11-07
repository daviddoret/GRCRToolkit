#' get_caller
#'
#' A friendly R debugging / troubleshooting function that returns the nth parent calling function from the stack.
#' \cr Various testing showed this function reacts properly but I must admit I'm far from understanding all R system intricacies so please don't rely too much on it in code that automatically fires thermonuclear missiles if that can be avoided without incurring too high budgetary costs.
#'
#' @param level (Scalar integer) The level to climb up the stack.
#'
#' @return (character) The calling function.
#'
#' @examples
#' get_caller()
#' f1 <- function(level = NULL) { return(get_caller(level = level)) }
#' f1()
#' f1(level = 0)
#' f1(level = 1)
#' f1(level = 2)
#' f2 <- function(level = NULL) { return(get_caller(level = level)) }
#' f2()
#' f2(level = 0)
#' f2(level = 1)
#' f2(level = 2)
#'
#' @export
get_caller <- function(
  level = 0,
  verbosity = NULL,
  ...) {
  level <- vp(level, 1, "integer", limit_min = 0) # Must be a safe function call to prevent infinite loops.
  #level <- sys.nframe() - level # Reverse the level from the bottom of the stack.
  verbosity <- vp(verbosity, 0, "integer", limit_min = 0) # Must be a safe function call to prevent infinite loops.
  sys_parent <- sys.parent(n = level)
  if (is.null(sys_parent)) {
    return(NULL)
  } else {
    sys_function <- sys.function(-level)
    sys_call <- sys.call(-level)
    if (is.null(sys_function) | is.null(sys_call)) {
      return(NULL)
    } else {
      return(deparse(match.call(
        definition = sys_function,
        call = sys_call,
        expand.dots = TRUE,
        envir = parent.frame(n = level))))
    }
  }
}
