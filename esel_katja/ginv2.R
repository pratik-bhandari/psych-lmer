ginv2 <- function(x) # define a function to make the output nicer
  fractions(provideDimnames(ginv(x),
                            base=dimnames(x)[2:1]))