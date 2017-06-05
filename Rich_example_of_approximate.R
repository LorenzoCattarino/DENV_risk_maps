#-------------------------------------- Create look up functions to map R0 back to FOI


###
######### Example from Rich
###

# Reference FOI value
foi <- seq(0, 2, length.out = 5000)

# Corresponding R0, derived through some function
r0 <- foi * foi

# Function to interpolate new R0 values
foi_from_r0_spline <- splinefun(r0, foi)

# A new R0 value
x <- runif(1, min(r0), max(r0))

# The corresponding FOI value
foi_from_r0_spline(x)

# Function to interpolate new R0 values where you can set tolerance of initial FOI
foi_from_r0_adaptive <- approximate(function(x) x * x,
                                    a = 0,
                                    b = 2,
                                    inverse = TRUE,
                                    tol = 1e-5)
# The corresponding FOI value - 2
foi_from_r0_adaptive(x)

###
##########
###
