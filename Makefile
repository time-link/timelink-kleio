bn=$$(./build-number)
vn=$$(cat version.number.kleio)
cdate=$$(date "+%Y-%m-%d %H:%M:%S" )

.PHONY:

all:
	@echo "usage:"
	@echo "  make image                build docker image and tag with new build number"
	@echo "  make latest               tag last image as 'latest"
	@echo "  make unique               tag last image as 'unique"
	@echo "  make stable               tag last image as 'stable"
	@echo "  make push tag=TAG         push image with tqag=latest | unique | stable
	@echo "  make number               return the current build number"
	@echo "  make version              return the current version string"
	@echo "  make inc                  increment build number"

clean:
	rm -rf .build

prepare: clean
	mkdir -p .build
	cp -r ./src .build
	cp Dockerfile .build/
	mv .build/src/gacto2.str .build/src/gacto2.str.bak
	mv .build/src/topLevel.pl .build/src/topLevel.pl.bak
	@sed -e "s/@@VERSION@@/${vn}/" \
	    -e "s/@@BUILD@@/${bn}/" \
	    -e "s/@@DATE@@/${cdate}/"\
		.build/src/gacto2.str.bak\
		> .build/src/gacto2.str
	@sed -e "s/@@VERSION@@/${vn}/" \
	    -e "s/@@BUILD@@/${bn}/" \
	    -e "s/@@DATE@@/${cdate}/"\
		.build/src/topLevel.pl.bak\
		> .build/src/topLevel.pl
	rm .build/src/*.bak
	@ echo "Prepared for build ${bn} version ${vn}, ${cdate}"


inc: .PHONY
	@./build-number inc

number: .PHONY
	@echo ${bn}

version: .PHONY
	@echo ${vn}

image: inc
	@docker build \
	 	-t "kleio-server:${bn}" \
		-t "joaquimrcarvalho/kleio-server:${bn}" \
		-t kleio-server \
		-t joaquimrcarvalho/kleio-server\
		.build
	@echo "docker new kleio-server image build ${bn}""

latest:
	@docker tag kleio-server joaquimrcarvalho/kleio-server:latest
	@echo "'latest' tag added to kleio-server  image build ${bn}"

unique:
	@docker tag kleio-server joaquimrcarvalho/kleio-server:${bn}
	@docker tag kleio-server kleio-server:${bn}
	@echo "'unique' tag added to kleio-server image build ${bn}"

stable:
	@docker tag kleio-server joaquimrcarvalho/kleio-server:${vn}
	@docker tag kleio-server kleio-server:${vn}
	@echo "'stable' and '${vn}' tags added to kleio-server image build ${bn}"

push-latest:
	@docker push joaquimrcarvalho/kleio-server:latest

push-unique:
	@docker push joaquimrcarvalho/kleio-server:${bn}

push-stable:
	@docker push joaquimrcarvalho/kleio-server:${vn}
	@docker push joaquimrcarvalho/kleio-server:stable
