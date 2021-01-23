# class specifications

library(methods)

# specify super class animal
setClass("animal",
  slots = list(name = "character", weight = "numeric", female = "logical"),
  validity = function(object) {
    if (is.na(object@name) | object@name == "" | length(object@name) == 0) {
      return("name has to be a non empty character string.")
    }
    if (object@weight <= 0) {
      return("weight must be positive.")
    }
    TRUE
  }
)

# specify middle classes prey and predator
setClass("prey",
  slots = list(hide = "numeric"),
  contains = "animal",
  validity = function(object) {
    if (0 > object@hide | object@hide > 1) {
      return("hide must be between 0 and 1")
    }
    TRUE
  }
)

setClass("predator",
  slots = list(seek = "numeric"),
  contains = "animal",
  validity = function(object) {
    if (0 > object@seek | object@seek > 1) {
      return("seek must be between 0 and 1")
    }
    TRUE
  }
)

# function that generates validity functions for all prey and predator animals
# checks if weight and behavior attribute lie in a specified interval [lower,upper]
# type == "prey" then behavior hide will be checked and the same with the
# "predator" and "seek" pair.
validate_individual_animals <- function(weight_lower,
                                        weight_upper,
                                        type = c("prey", "predator"),
                                        behavior_lower,
                                        behavior_upper) {
  type <- match.arg(type)
  behavior <- switch(type,
    "prey" = "hide",
    "predator" = "seek"
  )
  function(object) {
    weight <- slot(object, "weight")
    behavior_value <- slot(object, behavior)
    if (weight_lower > weight | weight > weight_upper) {
      return(paste("weight must be between", weight_lower, "and", weight_upper))
    }
    if (behavior_lower > behavior_value | behavior_value > behavior_upper) {
      return(paste(behavior, "must be between", behavior_lower, "and", behavior_upper))
    }
    TRUE
  }
}

# specify lowest classes
setClass("mouse",
  contains = "prey",
  validity = validate_individual_animals(
    weight_lower = 0.5,
    weight_upper = 1,
    type = "prey",
    behavior_lower = 0.6,
    behavior_upper = 1
  )
)

setClass("rabbit",
  contains = "prey",
  validity = validate_individual_animals(
    weight_lower = 1,
    weight_upper = 5,
    type = "prey",
    behavior_lower = 0.3,
    behavior_upper = 0.8
  )
)
setClass("deer",
  contains = "prey",
  validity = validate_individual_animals(
    weight_lower = 15,
    weight_upper = 30,
    type = "prey",
    behavior_lower = 0.2,
    behavior_upper = 0.7
  )
)

setClass("hawk",
  contains = "predator",
  validity = validate_individual_animals(
    weight_lower = 3,
    weight_upper = 8,
    type = "predator",
    behavior_lower = 0.6,
    behavior_upper = 1
  )
)
setClass("lynx",
  contains = "predator",
  validity = validate_individual_animals(
    weight_lower = 20,
    weight_upper = 60,
    type = "predator",
    behavior_lower = 0.5,
    behavior_upper = 0.9
  )
)
