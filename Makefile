KLEIO_VERSION_NUMBER_FILE="kleio.version.number"
KLEIO_PATCH_NUMBER_FILE="kleio.patch.number"
KLEIO_BUILD_NUMBER_FILE="kleio.build.number"
KLEIO_BUILD_DATE_FILE="kleio.build.date"

bn="k$$(./build-number -f ${KLEIO_BUILD_NUMBER_FILE})"
major="$$(./build-number -f ${KLEIO_VERSION_NUMBER_FILE})"
minor="$$(./build-number -f ${KLEIO_PATCH_NUMBER_FILE})"
vn="$(major).$(minor)"
cdate=$$(date "+%Y-%m-%d %H:%M:%S" )

.PHONY:

all:
	@echo "usage:"
	@echo "  make image                build docker image and tag with new build number"
	@echo "  make tag-TAG              tag last image with TAG in latest | unique | stable"
	@echo "  make push-TAG             push image with TAG in latest | unique | stable"
	@echo "  make build                return the current build number"
	@echo "  make version              return the current version string (major.minor)"
	@echo "  make current              return the current version, build number"
	@echo "  make last                 return the last image build date, version, build number"
	@echo "  make inc-NUMBER           increment version with NUMBER in major | minor"
	@echo "  make token                generate token for KLEIO_ADMIN_TOKEN"
	@echo "  make start                start Kleio server on docker (requires image)"
	@echo "  make stop                 stop Kleio server"
	@echo "  make docs                 generate api docs (requires postman_doc_gen and api files)"


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
	@./build-number inc -f ${KLEIO_BUILD_NUMBER_FILE}
inc-major: .PHONY
	@./build-number inc -f ${KLEIO_VERSION_NUMBER_FILE}
inc-minor: .PHONY
	@./build-number inc -f ${KLEIO_PATCH_NUMBER_FILE}

version: .PHONY
	@echo ${vn}

current: .PHONY
	@echo ${vn}.${bn}

last: .PHONY
	@if ! test -f ${KLEIO_BUILD_DATE_FILE}; then echo "No previous image build"; else echo "$$(cat ${KLEIO_BUILD_DATE_FILE}) version ${vn} build ${bn}";  fi

image: prepare inc-build
	@docker build \
	 	-t "kleio-server:${bn}" \
		-t "joaquimrcarvalho/kleio-server:${bn}" \
		-t kleio-server \
		-t joaquimrcarvalho/kleio-server\
		.build
	@echo "$(cdate)" > "${KLEIO_BUILD_DATE_FILE}"
	@echo "docker new kleio-server image build ${bn}"

tag-latest:
	@docker tag kleio-server joaquimrcarvalho/kleio-server:latest
	@echo "'latest' tag added to kleio-server  image build ${bn}"

tag-unique:
	@docker tag kleio-server joaquimrcarvalho/kleio-server:${bn}
	@docker tag kleio-server kleio-server:${bn}
	@echo "'unique' tag added to kleio-server image build ${bn}"

tag-stable:
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

token:
	@echo "KLEIO_ADMIN_TOKEN=$$(openssl rand -hex 20)"

kleio-run:
	@export KLEIO_ADMIN_TOKEN=$$(openssl rand -hex 20);\
	echo;\
	if [ -e "$$PWD/tests/kleio-home" ]; then KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME" ]; then export KLEIO_HOME="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8089"; fi;\
	echo "starting kleio-server on port $$KLEIO_SERVER_PORT";\
	docker run -e KLEIO_ADMIN_TOKEN -e KLEIO_DEBUG -e KLEIO_SERVER_PORT -v $$KLEIO_HOME:/kleio-home:cached -p "$$KLEIO_SERVER_PORT:$$KLEIO_SERVER_PORT" --rm --name kleio -d kleio-server ;\
	echo " ";\
	echo "KLEIO_HOME=$$KLEIO_HOME";\
	echo "KLEIO_ADMIN_TOKEN=$$KLEIO_ADMIN_TOKEN";\
	echo " "

kleio-start: kleio-run

kleio-stop:
	docker stop kleio

start: kleio-run

stop: kleio-stop

docs: .PHONY
	@echo "Requires postman_doc_gen https://github.com/karthiks3000/postman-doc-gen"
	@echo "Requires Postman API export at ./api/api.json"
	@echo "Requires Postman environment export at ./api/environment.json"
	@echo "Generating doc..."
	@postman_doc_gen api/postman/api.json -o docs/api -e api/postman/environment.json