package object Anagramas {
  type Palabra = String
  type Frase = List[Palabra]
  type Occurrencias = List[(Char, Int)]

  // Diccionario de prueba
  val diccionario: List[Palabra] = List(
    "cosas", "como", "yo", "y", "ocasos", "cayo", "mocosos", "roca", "moco", "sos"
  )

  /** Calcula la lista de ocurrencias de una palabra: lista de (char, frecuencia), ordenada por char */
  def lOcPal(p: Palabra): Occurrencias = {
    p.groupBy(c => c)
      .map { case (c, cs) => (c, cs.length) }
      .toList
      .sortBy(_._1)
  }

  /** Calcula la lista de ocurrencias de una frase concatenando sus palabras */
  def lOcFrase(f: Frase): Occurrencias =
    lOcPal(f.mkString)

  /** Mapea cada lista de ocurrencias (key) a las palabras del diccionario que la comparten */
  lazy val diccionarioPorOcurrencias: Map[Occurrencias, List[Palabra]] =
    diccionario.groupBy(lOcPal)

  /** Devuelve los anagramas de una palabra según el diccionario */
  def anagramasDePalabra(palabra: Palabra): List[Palabra] =
    diccionarioPorOcurrencias.getOrElse(lOcPal(palabra), Nil)

  /**
   * Genera todas las posibles sublistas de ocurrencias (incluyendo la lista vacía).
   * Cada sublista conserva el orden de caracteres.
   */
  def combinaciones(lOcurrencias: Occurrencias): List[Occurrencias] = lOcurrencias match {
    case Nil => List(Nil)
    case (c, n) :: rest =>
      val restCombs = combinaciones(rest)
      // Para cada sublista de rest, generar versiones con 1..n de (c)
      val withC = for {
        comb <- restCombs
        i <- 1 to n
      } yield (c, i) :: comb
      restCombs ++ withC
  }

  /**
   * Resta las ocurrencias slOc de lOc, asumiendo slOc <= lOc por cada caracter.
   * Devuelve sólo los caracteres con frecuencia > 0, en orden.
   */
  def complemento(lOc: Occurrencias, slOc: Occurrencias): Occurrencias = {
    val slMap = slOc.toMap
    lOc.map { case (c, count) => (c, count - slMap.getOrElse(c, 0)) }
      .filter(_._2 > 0)
  }

  /**
   * Genera todos los anagramas de una frase: listas de palabras que usan exactamente
   * las mismas ocurrencias de caracteres, basadas en el diccionario.
   */
  def anagramasDeFrase(frase: Frase): List[Frase] = {
    // Recursión que genera frases para las ocurrencias dadas
    def iter(occ: Occurrencias): List[Frase] =
      if (occ.isEmpty) List(Nil)
      else for {
        combo <- combinaciones(occ) if combo.nonEmpty
        palabra <- diccionarioPorOcurrencias.getOrElse(combo, Nil)
        resto <- iter(complemento(occ, combo))
      } yield palabra :: resto

    iter(lOcFrase(frase))
  }
}
