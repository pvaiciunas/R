# Helper functions

# Sample face
face <- function(dimension) matrix(seq(1:dimension), nrow = dimension, byrow = T)

# Sample Cube
cube <- function(dimension) {
# (F) - Red
# (R) - Blue 
# (B) - Orange
# (L) - Green
# (U) - White
# (D) - Yellow
	
	Reds <- matrix(paste(1:dimension^2, "R", sep = ""), nrow = dimension, byrow = T)
	Blues <- matrix(paste(1:dimension^2, "B", sep = ""), nrow = dimension, byrow = T)
	Oranges <- matrix(paste(1:dimension^2, "O", sep = ""), nrow = dimension, byrow = T)
	Greens <- matrix(paste(1:dimension^2, "G", sep = ""), nrow = dimension, byrow = T)
	Whites <- matrix(paste(1:dimension^2, "W", sep = ""), nrow = dimension, byrow = T)
	Yellows <- matrix(paste(1:dimension^2, "Y", sep = ""), nrow = dimension, byrow = T)
	
	list(Red = Reds, Blue = Blues, Orange = Oranges, Green = Greens, White = Whites, Yellow = Yellows)
}


#Column inversion
C <- function(mat) mat[,ncol(mat):1]

# Row Inversion
R <- function(mat) mat[nrow(mat):1,



# Rotation axis matrix coordinates
#F through B
x <- function(mat) {
	x <- list(mat, R(t(mat)), R(C(mat)), C(t(mat)))
	names(x) <- c("U","R","D","L")
	x
}

#R through L
y <- function(mat) {
	y <- list(C(t(mat)), C(t(mat)), C(t(mat)), C(t(mat)))
	names(y) <- c("F","U","B","D")
	y
}	

#U through D
z <- function(mat) {
	z <- list(C(mat), C(mat), C(mat), C(mat))
	names(z) <- c("F","R","B","L")
	z
}