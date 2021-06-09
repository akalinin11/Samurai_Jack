module Library where
import PdePreludat

data Elemento = UnElemento { 
 tipo :: String,
 ataque :: (Personaje-> Personaje),
 defensa :: (Personaje-> Personaje)
  } deriving (Show)


data Personaje = UnPersonaje {
 nombre :: String,
 salud :: Number,
  elementos :: [Elemento],
  anioPresente :: Number 
} deriving (Show)

samuraiLeandro = UnPersonaje {
 nombre = "Lean Lianerd"
 ,salud = 70
  ,elementos = [elementoLean]
  ,anioPresente = 1000
} 

elementoLean= UnElemento{
    tipo = "malvado"
,    ataque = superPunch
,    defensa = funcionIgual  
}


enemigoDeLean = UnPersonaje {
 nombre ="Augusto"
 , salud = 60
 , elementos = [elementoLean]
 , anioPresente = 50
}

-- Punto 1)
--a)
mandarAlAnio :: Number->Personaje->Personaje
mandarAlAnio anio  = modificarAnio anio 

modificarAnio :: Number->Personaje->Personaje
modificarAnio n aPersonaje = aPersonaje {anioPresente= n}

--b)

meditar :: Number->Personaje->Personaje
meditar valor  = modificarSalud (valor/2) 

modificarSalud :: Number->Personaje->Personaje
modificarSalud valor aPersonaje = aPersonaje{salud= salud aPersonaje + valor}

--c)

causarDanio :: Number->Personaje->Personaje
causarDanio saludDada  = modificarSalud' (-saludDada) 

modificarSalud' :: Number->Personaje->Personaje
modificarSalud' saludDada aPersonaje = aPersonaje{salud= max (salud aPersonaje + saludDada) 0 }


--Punto 2

--a)

esMalvado :: Personaje->Bool
esMalvado  aPersonaje = elementoIgualA  (elementos aPersonaje)

elementoIgualA :: [Elemento]->Bool
elementoIgualA   = any (igualaPalabra) 

igualaPalabra :: Elemento->Bool
igualaPalabra = ("malvado"==) . tipo


--b)
--ataque y defensa 

funcionIgual :: Personaje->Personaje
funcionIgual aPersonaje = aPersonaje

superPunch :: Personaje->Personaje
superPunch  = modificarSalud' (-80) 
--------------------------------------------

type Ataque = Personaje->Personaje

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce aPersonaje elemento = diferencia  (saludLuegoDeAtaque elemento aPersonaje) (salud aPersonaje)

saludLuegoDeAtaque :: Elemento ->Personaje ->Number
saludLuegoDeAtaque aElemento  =  salud . (ataque aElemento)


diferencia :: Number->Number->Number
diferencia  saludFInal  = (saludFInal -)


--c)

enemigosMortales :: Personaje->[Personaje]->[Personaje]
enemigosMortales aPersonaje listasDeRivales = filter (puedeMatar aPersonaje) listasDeRivales


puedeMatar:: Personaje->Personaje->Bool
puedeMatar aPersonaje aRival = any (saludIgualaCero aPersonaje)  (elementos aRival)


saludIgualaCero :: Personaje->Elemento->Bool
saludIgualaCero aPersonaje  = (0==). salud  . flip  modificarSalud' aPersonaje . danioQueProduce aPersonaje
