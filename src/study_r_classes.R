# numerical vec
vec <- c(1, 2, 3, 4)
$class(vec)
class(vec)
attributes(vec)

# categorical vec
vec <- c("a", "b", "b", "c", "c")
class(vec)

x <- factor(vec)
levels(x)
attributes(x)

# logcial vec - boolean
x <- c(TRUE, FALSE, TRUE, TRUE)
as.numeric(x)
as.logical(x)

# assign numeric dtype
as.numeric(x) # as numeric translate characters to numbers (not alphabetical number)
as.logical(x)

# integers
int_sequ  <- -3 : 12

# random normal
rnorm(12)

## LIST
x <- list("a" = 1, "b" = 10, "c" = 94, "e", 120)
x[[5]]
