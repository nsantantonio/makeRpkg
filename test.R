one <- function(len) { matrix(1, nrow = len) }

two <- function(x, y, z = c(1, 2,3)) {
	aPlusB <- function(a, b) a + b
	aPlusBSq <- function(a, b) {
		apb <- a + b
		apb^2
	}

	if(aPlusB(x, y) > aPlusBSq(x + y, z)) {
		print("this is arbitrary")
	}
}


# some comments
three <- function() {
# some other functions 
    print("this is also arbitrary")
}
