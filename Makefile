flower: flower.hs
	ghc --make flower.hs -o bin/flower
	bin/flower -p -o flower.svg -w 600
	open flower.svg

planet: planet.hs
	ghc --make planet.hs -o bin/planet
	bin/planet -p -o planet.svg -w 600
	open planet.svg
