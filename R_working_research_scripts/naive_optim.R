


naive_optim <- function(fun, target, initial_position, initial_step_size, max_steps) {

  step0 <- initial_position
  step1 <- initial_position + initial_step_size

  for(i in 1:max_steps){

    x0 <- fun(step0)
    x1 <- fun(step1)

    direction <- NULL
    if(x0 < x1){ direction <- "left is down, right is up"}
    if(x0 == x1){ direction <- "flat"}
    if(x0 > x1){ direction <- "right is down, left is up"}

    situation <- NULL
    if(target < x0) { situation <- "target is lower than x0" }
    if(target < x0) { situation <- "target is lower than x0" }


  }

}
