######################################################################
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",

  # Define the slots
  slots = c(
    x = "numeric",
    y = "numeric"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)

# create a method to assign the value of a coordinate
setGeneric(name="setCoordinate",
           def=function(theObject,xVal,yVal)
           {
             standardGeneric("setCoordinate")
           }
)

setMethod(f="setCoordinate",
          signature="FirstQuadrant",
          definition=function(theObject,xVal,yVal)
          {
            theObject@x <- xVal
            theObject@y <- yVal
            return(theObject)
          }
)

x <- FirstQuadrant()
x
#An object of class "FirstQuadrant"
#Slot "x":
#  [1] 0
#
#Slot "y":
#  [1] 0

y <- FirstQuadrant(x=5,y=7)
y
#An object of class "FirstQuadrant"
#Slot "x":
#  [1] 5
#
#Slot "y":
#  [1] 7

y@x
#[1] 5
y@y
#[1] 7

# generates an error
#z <- FirstQuadrant(x=3,y=-2)
#Error in validObject(.Object) :
#  invalid class “FirstQuadrant” object: A negative number for one of the coordinates was given.
# z
#Error: object 'z' not found

z <- FirstQuadrant(x=2.5,y=10)
z
#An object of class "FirstQuadrant"
#Slot "x":
#  [1] 2.5
#
#Slot "y":
#  [1] 10

# produces an error
#z <- setCoordinate(z,-3.0,-5.0)
#z

#An object of class "FirstQuadrant"
#Slot "x":
#  [1] -3
#
#Slot "y":
#  [1] -5
