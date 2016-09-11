val lines = {
  for {
    path <- args
    file <- scala.io.Source.fromFile(path)
  } yield path -> file.getLines()
}.toMap

val values = lines.mapValues{ lines =>
  {
    for {
      line <- lines
      (user :: item :: rating :: _) <- line.split(" ")
    } yield (user, item) -> line
  }.toMap
}

val inter = values.view.map(_._2.keySet).foldLeft(_ & _)

// vim: set ts=2 sw=2 et:
