# initiate generic function 'meet'
setGeneric(
  "meet",
  function(animal1, animal2) standardGeneric("meet")
)
# implement the most general meet method (animal, animal)
setMethod("meet",
  signature = c(animal1 = "animal", animal2 = "animal"),
  function(animal1, animal2) {
    sample(c("beschnuppern sich", "ignorieren sich"), 1)
  }
)
# for all in prey create method that implements meet of the same prey species
for (prey_animal in c("mouse", "rabbit", "deer")) {
  setMethod("meet",
    signature = c(animal1 = prey_animal, animal2 = prey_animal),
    function(animal1, animal2) {
      if (animal1@female != animal2@female) {
        return(sample(c(
          "beschnuppern sich", "ignorieren sich",
          "paaren sich"
        ), 1, prob = c(0.25, 0.25, 0.5)))
      }
      callNextMethod()
    }
  )
}

# method for the general meeting of two predators
setMethod("meet",
  signature = c(animal1 = "predator", animal2 = "predator"),
  function(animal1, animal2) {
    sample(c("beschnuppern sich", "ignorieren sich", "bekämpfen sich"), 1)
  }
)

# for each predator create a method when two of the same species meet
for (predator_animal in c("hawk", "lynx")) {
  setMethod("meet",
    signature = c(animal1 = predator_animal, animal2 = predator_animal),
    function(animal1, animal2) {
      if (animal1@female != animal2@female) {
        return(sample(c(
          "bekämpfen sich",
          "paaren sich"
        ), 1))
      }
      callNextMethod()
    }
  )
}

# implement a method for the case when a predator meets a prey
setMethod("meet",
  signature = c(animal1 = "predator", animal2 = "prey"),
  function(animal1, animal2) {
    if (animal2@weight >= 0.05 * animal1@weight &
      animal2@weight <= 0.7 * animal1@weight) {
      prob_death <- min(1, max(0, 0.6 + animal1@seek - animal2@hide))
      return(sample(c(
        "Raubtier tötet und frisst Beute",
        "Beute entkommt"
      ), 1,
      prob = c(
        prob_death,
        1 - prob_death
      )
      ))
    }
    callNextMethod()
  }
)
