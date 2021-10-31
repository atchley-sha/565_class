##Static trip equillibrium: ideal, but hard
a <- matrix(c(.005, 0, -1, 0,
       0, .02, 0, -1,
       1, 1, 0, 0,
       0, 0, 1, -1),
       nrow=4, byrow=T)

b <- matrix(c(-15, -10, 1000, 0))

solve(a) %*% b


##Iterative assignment algorithms

#All or nothing:
#1. everyone takes fastest path, get travel time
#2. everyone takes new fastest path, get travel time
#matrix(c(iteration, Va, Vb, Ta, Tb)), take column averages
#
#
#Incremental:
#1. split into shares (e.g. 0.4, 0.3, 0.2, 0.1)
#2. for each share (usu. largest to smallest) assign to new fastest route
#3. ???
#4. profit
#
#
#Successive averages (FHWA):
#V_n = (1-\phi)*V_{n-1} + \phi F,
#where \phi = 1/n and F is all-or-nothing volume