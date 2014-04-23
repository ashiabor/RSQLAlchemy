pkg<-"RSQLAlchemy"
require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("RSQLite", quietly=TRUE) || stop("RSQLite package not found")
library(package=pkg,character.only=TRUE)
if(!(exists("path") && file.exists(path)))
path<-getwd()
test.suite <- defineTestSuite(name=paste("RSQLAlchemy","unit tests"),
                              dirs = path,
                              testFileRegexp = 'test_.*.R$')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)