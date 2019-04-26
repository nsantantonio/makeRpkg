f <- commandArgs(TRUE)
checkCode <- TRUE
checkAddToGit <- FALSE
l <- readLines(f)
pkgName <- gsub("\\.R|.*\\/", "", f)

yes <- c("Y", "y", "YES", "Yes", "yes")
cat("Preparing to make package:", pkgName, "...\n")

comments <-grep("^\\#", l) 
indented <- grep("^\\s", l)
blank <- which(l == "")
ignore <- sort(Reduce(union, list(comments, indented, blank)))

func <- grep("function", l)
parenStart <- grep("\\{", l)
parenEnd <- grep("\\}", l)
funcStart <- func[!func %in% ignore]

if (checkCode){
	fse <- grepl("function|^\\}", l[-ignore])
	l[-ignore][fse]
	if(any(!fse)) warning("Unexpected indentation or code present. May result in unexpected behavior...")
	if(any(!funcStart %in% parenStart)) stop("All functions, including one-liners, must be inside braces {}!")
}

shortFunc <- parenStart[parenStart %in% funcStart & parenStart %in% parenEnd]
longFuncStart <- parenStart[parenStart %in% funcStart & !parenStart %in% parenEnd]
longFuncEnd <- parenEnd[!(parenEnd %in% ignore | parenEnd %in% parenStart)]

shortFuncNames <- trimws(gsub("<-.*", "", l[shortFunc]))
longFuncNames <- trimws(gsub("<-.*", "", l[longFuncStart]))

shortFuncl <- lapply(shortFunc, function(x) l[x])
longFuncl <- list()
for (i in 1:length(longFuncStart)) longFuncl[[i]] <- l[longFuncStart[i]:longFuncEnd[i]]
names(shortFuncl) <- shortFuncNames
names(longFuncl) <- longFuncNames

funcList <- c(shortFuncl, longFuncl)

existingFiles <- system("ls", intern = TRUE)
if(pkgName %in% existingFiles) existingFiles <- system(paste0("ls ", pkgName), intern = TRUE)
Rdir <- paste0(pkgName, "/R")
if ("R" %in% existingFiles){
	existingRfiles <- system(paste0("ls ", Rdir), intern = TRUE)
	toOverwrite <- existingRfiles[existingRfiles %in% paste0(names(funcList), ".R")]
	if (length(toOverwrite) > 0) {
		cat("Some R files already exist! The following R files will be overwitten:\n", paste0(toOverwrite, "\n"), "do you want to overwrite these files? (y/n): ")
		answer <- readLines("stdin", n = 1)
		toOverwrite <- gsub("\\.R", "", toOverwrite)
		if (!answer %in% yes) funcList <- funcList[!names(funcList) %in% toOverwrite]
	}
} else {
	system(paste0("if [ ! -d ", Rdir, " ]; then
	mkdir -p ", Rdir, "
	fi"))
}

if (length(funcList) > 0) {
	cat("The following R files will be written:\n", paste0(names(funcList), ".R\n")) 
} else {
		cat("No R files written. Exiting R.\n"); quit(save = "no")
}

cat("Do you want me to prepare Roxygen R files? (y/n): ")
RoxygenAnswer <- readLines("stdin", n = 1)
useRoxygen <- RoxygenAnswer %in% yes

if (useRoxygen) {
	header <- lapply(funcList, "[[", 1)
	grep("\\)\\{|\\)\\s\\{", header)
	header <- gsub(".*function\\(|\\)\\{.*|\\)\\s\\{.*", "", header)
	argL <- strsplit(header, ",\\s*(?![^()]*\\))", perl = TRUE)
	names(argL) <- names(funcList)
	argL <- lapply(argL, function(x) x[x != "..."])
	defL <- list()
	for(i in names(argL)){
		defaults <- grepl("=", argL[[i]])
		defArgs <- gsub(".*\\=\\s| \\=", "", argL[[i]])
		defArgs[!defaults] <- "" 
		defL[[i]] <- defArgs
		argL[[i]] <- gsub("=.*|\\s=.*", "", argL[[i]]) 
	}
	argL <- lapply(argL, function(x) gsub(".*\\=\\s| \\=", "", x))

	for (i in names(funcList)) {
		params <- paste0("#' @param ", argL[[i]], " [value]")
		params[defL[[i]] != ""] <- paste0(params[defL[[i]] != ""], ". Default is ", defL[[i]][defL[[i]] != ""])
		funcList[[i]] <- c(c(paste0("#' ", i, " function"),
							 "#'",
							 "#' function to (do something)",
							 "#'",
							 params,
							 "#' @return [value]",
							 "#' @details [fill in details here]",
							 "#' @examples none",
							 "#' @export"), 
							 funcList[[i]])
	}
}

for (i in names(funcList)) writeLines(funcList[[i]], paste0(Rdir, "/",i, ".R"))

if (!"DESCRIPTION" %in% existingFiles) {
	desc <- c(paste0("Package: ", pkgName), "Version: 0.1", "Author: Nicholas Santantonio <ns722@cornell.edu>", "Encoding: UTF-8")
	writeLines(desc, paste0(pkgName, "/DESCRIPTION"))
}

if (useRoxygen){
	cat("Do you want me to try to build the R package'", pkgName, "'? (y/n): ")
	shouldBuild <- readLines("stdin", n = 1)
	if (shouldBuild %in% yes) {
		# system(paste0("R -e 'setwd(", paste0('"', pkgName, "'"), ");library(devtools);document()'"))
		setwd(pkgName)
		library(devtools)
		document()
		setwd("../")
		system(paste0("R CMD build ", pkgName))
	}
	cat("Do you want me to try to install the R package'", pkgName, "'? (y/n): ")
	shouldInstall <- readLines("stdin", n = 1)
	if (shouldInstall %in% yes) system(paste0("R CMD INSTALL ", pkgName))

	if(checkAddToGit){	
		cat("Do you want me to add the package to git? (y/n): ")
		addToGit <- readLines("stdin", n = 1)
		if(addToGit){
			cat("where do you want me to put the package? Enter path: ")
			path <- readLines("stdin", n = 1)
			system(paste("mv", pkgName, path))
			system(paste("cd", path, "; git init"))
		}
	}
}
