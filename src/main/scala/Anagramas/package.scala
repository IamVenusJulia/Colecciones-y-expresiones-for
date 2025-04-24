package object Anagramas {
  type Palabra = String
  type Frase = List[Palabra]
  type Occurrencias = List[(Char, Int)]

  // Diccionario de prueba
  val diccionario: List[Palabra] = List(
    "mente", "gustaron","res","los","es","se","dana","oso","esa","al","nana","ron","aron","gatuno"
  )

  def lOcPal(p: Palabra): Occurrencias = {
    p.groupBy(c => c)
      .map { case (c, cs) => (c, cs.length) }
      .toList
      .sortBy(_._1)
  }

  def lOcFrase(f: Frase): Occurrencias =
    lOcPal(f.mkString)

  lazy val diccionarioPorOcurrencias: Map[Occurrencias, List[Palabra]] =
    diccionario.groupBy(lOcPal)

  def anagramasDePalabra(palabra: Palabra): List[Palabra] =
    diccionarioPorOcurrencias.getOrElse(lOcPal(palabra), Nil)

  def combinaciones(occ: Occurrencias): List[Occurrencias] = occ match {
    case Nil => List(Nil)
    case (char, n) :: rest =>
      val restComb = combinaciones(rest)
      val withChar = for {
        comb <- restComb
        i <- 1 to n
      } yield (char, i) :: comb
      restComb ++ withChar
  }
  def complemento(lOc: Occurrencias, slOc: Occurrencias): Occurrencias = {
    val slMap = slOc.toMap
    lOc.map { case (c, count) => (c, count - slMap.getOrElse(c, 0)) }
      .filter(_._2 > 0)
  }

  def anagramasDeFrase(frase: Frase): List[Frase] = {
    val occ = lOcFrase(frase)

    def iter(occ: Occurrencias): List[Frase] = {
      val resultados =
        for {
          combo <- combinaciones(occ)
          if combo.nonEmpty && diccionarioPorOcurrencias.contains(combo)
          palabra <- diccionarioPorOcurrencias(combo)
          resto <- iter(complemento(occ, combo))
        } yield palabra :: resto

      // incluir también frases parciales
      if (resultados.isEmpty) List(Nil)
      else resultados
    }

    iter(occ).filter(_.nonEmpty) // opcional si querés evitar listas vacías
  }
}
