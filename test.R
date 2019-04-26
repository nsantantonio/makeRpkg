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



# blah blah blah



# some comments
three <- function() {
# some other functions 
    print("this is also arbitrary")
}

# pop = RGSC[[lastRGSCgen]]; GSfit = GSmodel[[lastGSmodel]]; nSel = 20; nCrosses = nNuclear; use = ebv; pullGeno = pullSnpGeno; weightLoci = FALSE; maxPropPerParent = 0.05; truncPop = 0.2
maxVar <- function(pop, GSfit, nCrosses, truncPop = 1, weightLoci = FALSE, use = ebv, pullGeno = pullSnpGeno, maxPropPerParent = 1, verbose = FALSE, nProgeny = 1, ...){
	nCombos <- choose(nInd(pop) * truncPop, 2)
	nEx <- if(nCombos < nCrosses) ceiling(nCrosses / nCombos) else 1 
	maxP <- if(maxPropPerParent == 0 | nCombos <  nCrosses) nCrosses else if(maxPropPerParent == 1) 1 else maxPropPerParent * nCrosses
	
	parVal <- ebv(pop)
	# getAcc(pop)
	rownames(parVal) <- pop@id
	# if(truncPop < 1) pop <- selectInd(pop, nInd = nInd(pop) * truncPop, use = "ebv")
	if(truncPop < 1) pop <- truncSel(pop, nSel = nInd(pop) * truncPop, use = use)
	M <- pullGeno(pop)
	K <- if(weightLoci) genCov(M, u = c(GSfit@markerEff)) else genCov(M)
	covL <- data.frame(which(lower.tri(K), arr.ind = TRUE), selCrit = K[lower.tri(K)])
	selCrit <- data.frame(covL, p1 = colnames(K)[covL$col], p2 = colnames(K)[covL$row]) 
	
	lenSel <- 0
	while(lenSel < nCrosses / nEx){
		if(lenSel > 0) {
			cat("Not enough possible crosses with maxP =", maxP, "! Increasing maxP to", maxP + 1, "and retrying...\n")
			maxP <- maxP + 1
		}
		selection <- getSel(selCrit, n = nCrosses, high = FALSE, maxP = maxP)
		lenSel <- nrow(selection)
	}
	if(verbose) print(table(selection))
	
	if(nEx > 1) selection <- selection[rep(1:nrow(selection), times = nEx)[1:nCrosses], ]
	if(nProgeny > 1) selection <- selection[rep(1:nrow(selection), each = nProgeny), ] 
	makeCross(pop, crossPlan = selection) 
}
