module Lib where
import Text.Show.Functions

laVerdad = True

{-De cada festival se sabe el lugar donde se realiza, la cantidad y estado de ánimo inicial del público y 
las bandas que tocan en él, ordenadas cronológicamente.
Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, tiene un 
público de 20000 personas con ánimo inicial “indiferente”.
-}

data Festival = UnFestival { lugar :: String,
                            cantidadPublico :: Float,
                            animoPublico :: String,
                            bandas :: [Banda]} deriving Show

{-Los géneros
Las bandas, cada vez que tocan, movilizan al público del festival de acuerdo al género al que pertenezcan. Por ejemplo:
rock nacional: hace que el público aumente en 100 personas

pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", duplican la cantidad 
y dejan el público "eufórico". 

Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan 
subgéneros
Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo. 
Existen otros subgéneros del metal que también alteran al público de igual manera, pero agregando otros calificativos 
al estado de ánimo. 
-}


type Genero = Festival -> Festival

rockNacional :: Genero
rockNacional festival = cambiarPublico (+100) festival

cambiarPublico :: (Float-> Float) -> Genero
cambiarPublico funcion festival = festival {cantidadPublico = (funcion.cantidadPublico) festival}

pop :: Genero
pop festival 
    | animoPublico festival == "indiferente" = (cambiarPublico (*2) . cambiarAnimo ( \x-> "euforico")) festival
    | otherwise = festival

cambiarAnimo :: (String -> String) -> Genero 
cambiarAnimo funcion festival = festival {animoPublico = (funcion.animoPublico) festival}


heavyMetal :: Genero 
heavyMetal = cambiarPublico (*1.01). cambiarAnimo (++"pesado")

trashMetal :: Genero 
trashMetal  = cambiarPublico (*1.01) . cambiarAnimo (++"basura")

{-Las bandas tienen un conjunto de descripciones realizadas por los críticos y los decibeles a los que suelen tocar. 
Además, cada vez que tocan, las bandas movilizan al público del festival de acuerdo al género al que pertenezcan. 
Algunas bandas son:
Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional. 
Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.
Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e 
"inodora".
Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del 
heavy metal.
Como se observa con el rock nacional, puede haber muchas bandas de cualquiera de los géneros.
-}

data Banda = UnaBanda {descriciones :: [String],
                        decibeles :: Float,
                        genero :: Genero} deriving Show

--definir la función tocar, que hace que la banda toque y altere al público del festival de acuerdo a su género.

tocar :: Festival -> Banda ->  Festival
tocar festival banda  = (genero banda) festival

{-Definir la función suceder, que hace que suceda un festival. El resultado debe ser el mismo festival pero 
con el público en su situación final, luego de haber tocado todas las bandas. -}

suceder :: Festival ->  Festival
suceder festival = foldl tocar festival (bandas festival)

{-Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo:
Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”. 
Acústica: Es la que toca a más de 55 decibeles. 
Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.
Definir las funciones que permitan clasificar a las bandas. Una banda puede clasificarse de 
más de una manera a la vez o ninguna.
-}

-------OPCION 1 CON GUARDAS ( sólo devuelve una clasificación)
{-clasificarBanda :: Banda -> String
clasificarBanda banda 
  | (length.descriciones) banda >= 3 || elem "vendida" (descriciones banda) = "vendida"
  | decibeles banda > 55 = "acustica"
  | decibeles banda > 40 && elem "legendaria" (descriciones banda) = "legendaria"-}
  
type Clasificacion = Banda -> String
vendida :: Clasificacion
vendida banda 
    |(length.descriciones) banda >= 3 || elem "vendida" (descriciones banda) = "vendida"
    |otherwise = ""

acustica :: Clasificacion
acustica banda
    | decibeles banda > 55 = "acustica" 
    | otherwise = " "

legendaria :: Clasificacion
legendaria banda
    |decibeles banda > 40 && elem "legendaria" (descriciones banda) = "legendaria"
    |otherwise = " "


----Clasificar banda con Fold
--clasificarBanda :: Banda -> [Clasificacion]-> [String]
--clasificarBanda banda clasificaciones = foldl1 (++).(clasificar banda) clasificaciones 
    --where clasificaciones = [vendida, acustica, legendaria]

--Clasificar banda con map
clasificarBanda :: Banda -> [Clasificacion] -> [String]
clasificarBanda banda clasificaciones = map (clasificar banda) clasificaciones

clasificar :: Banda -> Clasificacion -> String
clasificar banda clasificacion = clasificacion banda
 
 {-Definir la función popularidad, que, dada una lista de clasificaciones, permite conocer la popularidad de una banda. 
 La popularidad se calcula así: 100 puntos por cada clasificación a la que la banda aplique.-} 

popularidad :: Banda -> [Clasificacion] -> Float
popularidad banda  = (*100).fromIntegral.length.(clasificarBanda banda)

{-Definir la función buenFest, que dado un festival y un conjunto de clasificaciones posibles dice si es un buen fest. 
Esto sucede cuando cronológicamente cada banda es más popular que la anterior, 
y además la popularidad total (la popularidad acumulada de sus bandas) supera los 1000 puntos-}


buenFest :: Festival -> [Clasificacion] -> Bool
buenFest festival clasificaciones = sum (popularidades festival clasificaciones) > 1000 && crecePupularidad (popularidades festival clasificaciones)

popularidades :: Festival -> [Clasificacion] -> [Float]
popularidades festival clasificaciones = map (flip popularidad clasificaciones) (bandas festival)

crecePupularidad :: [Float] -> Bool
crecePupularidad [_] = True
crecePupularidad (banda1: banda2:restoBandas)
    | banda1 >= banda2 = False
    |otherwise = crecePupularidad (banda2:restoBandas)

{-¿Fueron de utilidad los conceptos de aplicación parcial y composición? Indicar donde se los utilizó y 
justificar su impacto en la solución.

¿Sería posible que alguna de las listas fuera infinita y de 
todas maneras las funciones sigan funcionando correctamente? Justificar y ejemplificar.
La funcion popularidad, por ejemplo, dejaría de funcionar ya que para arrojar un resultado necesita calcular primero el largo de 
toda la lista. Habría que agregarle que calcule el largo de los primeros n elementos y en ese caso sí funcionaría (gracias a Lazy evaluation)
Otra opcion sería sacar el largo de los que cumplan cierta condicion de esa lista infinita (Pero esto sólo funcionaría si 
existe al menos 1 elemnto que cumpla tal condición, sino seguirá evaluando infinitamente.
en CrecePopularidad ocurre lo mismo, evalua infinitamente ya que su caso base es una lista de 1 solo elemnto. Al igual 
ocurre con legendaria y vendida, que utilizan la funcion elem, si algún elemtno cumple la condicion me devuelve algo, sino, evalua infinitamente,
en todos casos la sugerencia de solución es la misma: Modificar las funciones y agregar funciones como take o filter que trabajen como lazy evaluation-}


