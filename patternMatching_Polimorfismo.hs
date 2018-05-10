data Animal = Perro {
	nombre:: String,
	peso :: Float
}

data Animal = Gato {
	nombre :: String,
	peso :: Float
}

come unAlimento pero@(Perro _ _ ) = perro { peso = peso Perro + unAlimento}

come unAlimento gato@(Gato _ _ ) = gato {peso = peso Gato + unAlimento}