all:  docs install

docs:
	R -e "devtools::document()"
vig:
	R -e "devtools::build_vignettes()"

build:
	(cd ..; R CMD build --no-build-vignettes trenaGeneData)

install:
	(cd ..; R CMD INSTALL trenaGeneData)

check:
	(cd ..; R CMD check `ls -t trenaGeneData_* | head -1`)

biocCheck:
	(cd ..; R CMD BiocCheck `ls -t trenaGeneData_* | head -1`)
