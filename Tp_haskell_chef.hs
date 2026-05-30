import Data.List

data Participante=UnParticipante{
    nombre::String,
    trucosCocina::[Truco],
    platoEspecialidad::Platos
}

type Truco=Platos->Platos

data Platos=UnPlato{
    nombrePlato::String,
    ingredientes::[Ingredientes],
    dificultad::Int
}deriving (Eq,Show)

sopa = UnPlato{
    nombrePlato = "sopa",
    ingredientes=[harina, harina, harina],
    dificultad=8
}

ensalada=UnPlato{
    nombrePlato="ensalada",
    ingredientes=[cafe, cafe],
    dificultad=9
}


data Ingredientes=UnIngrediente{
    nombreIngrediente::String,
    pesoIngrediente::Int
}deriving (Eq,Show)

cafe=UnIngrediente{
    nombreIngrediente="cafe",
    pesoIngrediente=2
}

harina=UnIngrediente{
    nombreIngrediente="harina",
    pesoIngrediente=6
}

--funciones Parte A
    --trucos

crearIngrediente::String->Int->Ingredientes
crearIngrediente unNombre unPeso= UnIngrediente unNombre unPeso

agregarIngrediente::Ingredientes->Platos->Platos
agregarIngrediente unIngrediente unPlato= unPlato{ingredientes=unIngrediente : ingredientes unPlato}

endulzar::Int->Platos->Platos
endulzar unPeso unPlato =agregarIngrediente (crearIngrediente "azucar" unPeso) unPlato

salar::Int->Platos->Platos
salar unPeso unPlato = agregarIngrediente(crearIngrediente "sal" unPeso) unPlato

darSabor::Int->Int->Platos->Platos
darSabor cantSal cantAzucar unPlato = salar cantSal .endulzar cantAzucar $ unPlato

modificarPesoIngrediente::(Int->Int)->Ingredientes->Ingredientes
modificarPesoIngrediente unaOperacion unIngrediente= unIngrediente{pesoIngrediente=unaOperacion(pesoIngrediente unIngrediente)}

modificarIngredientes::(Int->Int)->Platos->Platos
modificarIngredientes unaOperacion unPlato = unPlato{ingredientes=map(modificarPesoIngrediente unaOperacion)(ingredientes unPlato)}

duplicarPorcion::Platos->Platos
duplicarPorcion unPlato=modificarIngredientes (*2) unPlato

simplificar::Platos->Platos
simplificar unPlato
    |esComplejo unPlato = unPlato {dificultad = 5, ingredientes = filter ingredientesConMasDe10Gramos (ingredientes unPlato)}
    |otherwise = unPlato

ingredientesConMasDe10Gramos::Ingredientes->Bool
ingredientesConMasDe10Gramos unIngrediente = pesoIngrediente unIngrediente >= 10


--platos
nombresIngredientes::[Ingredientes]->[String]
nombresIngredientes=map nombreIngrediente

buscaNombreEnLista::Platos->String->Bool
buscaNombreEnLista unPlato unNombre = any (==unNombre) (nombresIngredientes(ingredientes unPlato))

esVegano::Platos->Bool
esVegano unPlato= not (buscaNombreEnLista unPlato "carne" || buscaNombreEnLista unPlato "huevos" || buscaNombreEnLista unPlato "lacteos")

esSinTacc::Platos->Bool
esSinTacc unPlato= not (buscaNombreEnLista unPlato "harina")

esComplejo::Platos->Bool
esComplejo unPlato= (length(ingredientes unPlato) > 5) && (dificultad unPlato > 7)

noAptoHipertension::Platos->Bool
noAptoHipertension unPlato=(pesoIngrediente (head (filter (\ingrediente -> nombreIngrediente ingrediente == "sal") (ingredientes unPlato))) > 2)

--parte B

pepe=UnParticipante{
    nombre= "Pepe Ronccino",
    trucosCocina=[darSabor 2 5, simplificar, duplicarPorcion],
    platoEspecialidad=sopa
}

lucas=UnParticipante{
    nombre= "Lucas Calavera",
    trucosCocina=[darSabor 2 5, simplificar, duplicarPorcion],
    platoEspecialidad=ensalada
}

--parte C

aplicarTrucoCocina::Platos->Truco->Platos
aplicarTrucoCocina unPlato unTruco=unTruco unPlato

cocinar::Participante->Platos
cocinar unParticipante= foldl (aplicarTrucoCocina) (platoEspecialidad unParticipante) (trucosCocina unParticipante)

calculoPesosIngredientes::[Ingredientes]->[Int]
calculoPesosIngredientes=map pesoIngrediente

esMasComplejoQue::Platos->Platos->Bool
esMasComplejoQue unPlatoA unPlatoB=(dificultad unPlatoA) > (dificultad unPlatoB)

sumaPesos::[Ingredientes]->Int
sumaPesos listaIngredientes = foldr (+) 0 (calculoPesosIngredientes listaIngredientes) 

pesaMenosQue::Platos->Platos->Bool
pesaMenosQue unPlatoA unPlatoB = sumaPesos (ingredientes unPlatoA) < sumaPesos (ingredientes unPlatoB)

esMejorQue::Platos->Platos->Bool
esMejorQue unPlatoA unPlatoB = esMasComplejoQue unPlatoA unPlatoB && pesaMenosQue unPlatoA unPlatoB

devuelveMejoreParticipante::Participante->Participante->Participante
devuelveMejoreParticipante unParticipanteA unParticipanteB
    |esMejorQue (cocinar unParticipanteA) (cocinar unParticipanteB)=unParticipanteA
    |otherwise = unParticipanteB

participanteEstrella::[Participante]->Participante
participanteEstrella listaParticipantes=foldr1 (devuelveMejoreParticipante) (listaParticipantes)
