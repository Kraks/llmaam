package llmaam.frontend.counter

private var counter = 0

def reset: Unit = counter = 0

def fresh(prefix: String = "#:x"): String =
  // The #:x namespace is reserved for this transformation.
  val name = s"$prefix$counter"
  counter += 1
  name
