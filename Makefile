flower: flower.hs
	ghc --make flower.hs -o bin/flower
	bin/flower -p -o flower.svg -w 600
	open flower.svg
