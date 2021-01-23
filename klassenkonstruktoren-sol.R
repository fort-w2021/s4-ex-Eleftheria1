# constructor of super class animal
animal <- function(name, weight, female) {
  if (missing(name)) {
    name <- sample(c("Jerry", "Jimmy", "Jason", "Jenny", "Jane"), 1)
  }
  if (missing(weight)) {
    weight <- runif(1, 0.1, 60)
  }
  if (missing(female)) {
    female <- sample(c(TRUE, FALSE), 1)
  }
  new("animal",
    name = name, weight = weight, female = female
  )
}

# constructor for middle classes prey and predator
prey <- function(name, weight, female, hide) {
  if (missing(hide)) {
    hide <- runif(1)
  }
  new("prey",
    animal(name, weight, female),
    hide = hide
  )
}

predator <- function(name, weight, female, seek) {
  if (missing(seek)) {
    seek <- runif(1)
  }
  new("predator",
    animal(name, weight, female),
    seek = seek
  )
}

# function that generates a constructor function for animals with parent class
# prey or predator (type argument) just insert the lower and upper bounds of
# the intervals from which the two arguments weight and seek/hide (behavior)
# should be uniformly sampled of.
build_constructor <- function(animal,
                              type = c("prey", "predator"),
                              weight_lower,
                              weight_upper,
                              behavior_lower,
                              behavior_upper) {
  type <- match.arg(type)
  if (type == "prey") {
    function(name, weight, female, hide) {
      if (missing(weight)) {
        weight <- runif(1, weight_lower, weight_upper)
      }
      if (missing(hide)) {
        hide <- runif(1, behavior_lower, behavior_upper)
      }
      new(
        animal,
        prey(name, weight, female, hide)
      )
    }
  } else {
    function(name, weight, female, seek) {
      if (missing(weight)) {
        weight <- runif(1, weight_lower, weight_upper)
      }
      if (missing(seek)) {
        seek <- runif(1, behavior_lower, behavior_upper)
      }
      new(
        animal,
        predator(name, weight, female, seek)
      )
    }
  }
}

# build the constructors for the lowest class
mouse <- build_constructor("mouse",
  weight_lower = 0.5,
  weight_upper = 1,
  type = "prey",
  behavior_lower = 0.6,
  behavior_upper = 1
)

rabbit <- build_constructor("rabbit",
  weight_lower = 1,
  weight_upper = 5,
  type = "prey",
  behavior_lower = 0.3,
  behavior_upper = 0.8
)

deer <- build_constructor("deer",
  weight_lower = 15,
  weight_upper = 30,
  type = "prey",
  behavior_lower = 0.2,
  behavior_upper = 0.7
)

hawk <- build_constructor("hawk",
  weight_lower = 3,
  weight_upper = 8,
  type = "predator",
  behavior_lower = 0.6,
  behavior_upper = 1
)

lynx <- build_constructor("lynx",
  weight_lower = 20,
  weight_upper = 60,
  type = "predator",
  behavior_lower = 0.5,
  behavior_upper = 0.9
)
