library(dagitty)

g1 <- dagitty( "dag {
	W1 -> Z1 -> X -> Y
	Z1 <- V -> Z2
	W2 -> Z2 -> Y
	X <-> W1 <-> W2 <-> Y
}")

g2 <- dagitty( "dag {
	Y <- X <- Z1 <- V -> Z2 -> Y
	Z1 <- W1 <-> W2 -> Z2
	X <- W1 -> Y
	X <- W2 -> Y
}")

plot(graphLayout(g1))
plot(graphLayout(g2))


print( impliedConditionalIndependencies( g1 ) )


lsd_dag <- dagitty(" dag {
  age -> covid_sev
  age -> dementia -> covid_sev
  age -> cci <-> dementia
  lsd -> cci -> covid_sev
  race -> lsd -> dementia
  race -> cci
  age -> lifestyle -> dementia
  race -> lifestyle -> covid_sev
  sex -> covid_sev
  sex -> cci
  sex -> lsd
  sex -> dementia
  sex -> lifestyle
  race -> covid_sev
  cci <-> lifestyle
  age -> lsd
  lsd -> covid_sev
}")

plot(graphLayout(lsd_dag))
