import Anagramas ._

lOcPal ( "casa" )
combinaciones (lOcPal ("casa"))
val frase = List ( "cosas" ,"como","yo")
combinaciones ( lOcFrase ( frase))
anagramasDeFrase(frase)

println("Pruebas para lOcPal")

lOcPal("barco")
lOcPal("banana")
lOcPal("zzz")
lOcPal("")
lOcPal("scala")

combinaciones(lOcPal("barco"))
combinaciones(lOcPal("banana"))
combinaciones(lOcPal("zzz"))
combinaciones(lOcPal(""))
combinaciones(lOcPal("scala"))

val frase1 = List ("me","gustaron")
val frase2 = List ("la","banana")
val frase3 = List ("sabes","ser")
val frase4 = List ("solo","se","que")
val frase5 = List ("nada","se")
val frase6 = List ("gusta","mente")

combinaciones (lOcFrase(frase1))
combinaciones (lOcFrase(frase2))
combinaciones (lOcFrase(frase3))
combinaciones (lOcFrase(frase4))
combinaciones (lOcFrase(frase5))

anagramasDeFrase(frase1)
anagramasDeFrase(frase2)
anagramasDeFrase(frase3)
anagramasDeFrase(frase4)
anagramasDeFrase(frase5)

