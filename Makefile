bn="k$$(./build-number)"
major="$$(./build-number -f version.number.kleio)"
minor="$$(./build-number -f patch.number.kleio)"
vn="$(major).$(minor)"
cdate=$$(date "+%Y-%m-%d %H:%M:%S" )

.PHONY:

all:
	@echo "usage:"
	@echo "  make image                build docker image and tag with new build number"
	@echo "  make latest               tag last image as 'latest"
	@echo "  make unique               tag last image as 'unique'"
	@echo "  make stable               tag last image as 'stable'"
	@echo "  make push-TAG             push image with TAG in latest | unique | stable"
	@echo "  make build                return the current build number"
	@echo "  make version              return the current version string (version.patch)"
	@echo "  make current              return the current version, build number"
	@echo "  make last                 return the last image build date, version, build number"
	@echo "  make inc-NUMBER           increment number with NUMBER in build | major | minor"

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

build: .PHONY
	@echo ${bn}

inc-build: .PHONY
	@./build-number inc -f build.number.kleio
inc-major: .PHONY
	@./build-number inc -f version.number.kleio
inc-minor: .PHONY
	@./build-number inc -f patch.number.kleio

version: .PHONY
	@echo ${vn}

current: .PHONY
	@echo ${vn}.${bn}

last: .PHONY
	@if ! test -f "build.date.kleio"; then echo "No previous image build"; else echo "$$(cat build.date.kleio) version ${vn} build ${bn}";  fi

image: prepare inc-build
	@docker build \
	 	-t "kleio-server:${bn}" \
		-t "joaquimrcarvalho/kleio-server:${bn}" \
		-t kleio-server \
		-t joaquimrcarvalho/kleio-server\
		.build
	@echo "$(cdate)" > "build.date.kleio"
	@echo "docker new kleio-server image build ${bn}"

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
