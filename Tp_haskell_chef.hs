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

pizza = UnPlato{
    nombrePlato = "pizza",
    ingredientes=[harina, salsaTomate, queso, jamon, aceituna, queso],
    dificultad=8
}

harina=UnIngrediente{
    nombreIngrediente="harina",
    pesoIngrediente=100
}

salsaTomate=UnIngrediente{
    nombreIngrediente="salsa de tomate",
    pesoIngrediente=50
}

queso=UnIngrediente{
    nombreIngrediente="queso",
    pesoIngrediente=100
}

jamon=UnIngrediente{
    nombreIngrediente="jamon",
    pesoIngrediente=50
}

aceituna=UnIngrediente{
    nombreIngrediente="aceituna",
    pesoIngrediente=20
}

ensalada=UnPlato{
    nombrePlato="ensalada",
    ingredientes=[lechuga, tomate, zanahoria],
    dificultad=2
}

lechuga=UnIngrediente{
    nombreIngrediente="lechuga",
    pesoIngrediente=50
}

tomate=UnIngrediente{
    nombreIngrediente="tomate",
    pesoIngrediente=50
}

zanahoria=UnIngrediente{
    nombreIngrediente="zanahoria",
    pesoIngrediente=50
}

data Ingredientes=UnIngrediente{
    nombreIngrediente::String,
    pesoIngrediente::Int
}deriving (Eq,Show)



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

modificarPesoCadaIngrediente::(Int->Int)->Platos->Platos
modificarPesoCadaIngrediente unaOperacion unPlato = unPlato{ingredientes=map(modificarPesoIngrediente unaOperacion)(ingredientes unPlato)}

duplicarPorcion::Platos->Platos
duplicarPorcion unPlato=modificarPesoCadaIngrediente (*2) unPlato

simplificar::Platos->Platos
simplificar unPlato
    |esComplejo unPlato = unPlato {dificultad = 5, ingredientes = filter ingredientesConMasDe10Gramos (ingredientes unPlato)}
    |otherwise = unPlato

ingredientesConMasDe10Gramos::Ingredientes->Bool
ingredientesConMasDe10Gramos unIngrediente = pesoIngrediente unIngrediente >= 10


--platos
nombresIngredientes::[Ingredientes]->[String]
nombresIngredientes=map nombreIngrediente

contieneIngrediente::Platos->String->Bool
contieneIngrediente unPlato unNombre = any (==unNombre) (nombresIngredientes(ingredientes unPlato))

esVegano::Platos->Bool
esVegano unPlato= not (contieneIngrediente unPlato "carne" || contieneIngrediente unPlato "huevos" || contieneIngrediente unPlato "lacteos")

esSinTacc::Platos->Bool
esSinTacc unPlato= not (contieneIngrediente unPlato "harina")

esComplejo::Platos->Bool
esComplejo unPlato= (length(ingredientes unPlato) > 5) && (dificultad unPlato > 7)

ingredientesConNombre::String->Platos->[Ingredientes]
ingredientesConNombre unNombre=filter((== unNombre).nombreIngrediente).ingredientes

noAptoHipertension::Platos->Bool
noAptoHipertension unPlato=any((>2).pesoIngrediente)(ingredientesConNombre "sal" unPlato)

--parte B

pepe=UnParticipante{
    nombre= "Pepe Ronccino",
    trucosCocina=[darSabor 2 5, simplificar, duplicarPorcion],
    platoEspecialidad=pizza
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
