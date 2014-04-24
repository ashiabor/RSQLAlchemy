## R_SQLAlchemy
## Control
## Author: Awo Ashiabor
##
## control is a class that tracks internal variables during the session
## control class has one field variables
##  control fields:
##                variables:=> internal variables
##  control Methods:  
##                "addVariable"

control <- setRefClass("control",
                       fields=list(variables ="list")
)

control$accessors("variables")

control$methods(
  
  addVariable=function(name, value) {  
    variables[[name]] <<- value
    invisible(variables)  
})


