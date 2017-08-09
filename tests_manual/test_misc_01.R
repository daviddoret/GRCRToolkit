
v <- c(1, 2, 3)
names(v) <- c("a", "b", "c")

y <- function(a,b,c){
  print(a)
  print(b)
  print(c)
}

l <- as.list(v)

do.call(y, args=l)

append("x", v)



