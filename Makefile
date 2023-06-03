SHELL := /bin/bash
DOCKER_REPOSITORY=timelinkserver
KLEIO_VERSION_NUMBER_FILE="kleio.version.number"
KLEIO_PATCH_NUMBER_FILE="kleio.patch.number"
KLEIO_BUILD_NUMBER_FILE="kleio.build.number"
KLEIO_BUILD_DATE_FILE="kleio.build.date"

bn="$$(./build-number -f ${KLEIO_BUILD_NUMBER_FILE})"
major="$$(./build-number -f ${KLEIO_VERSION_NUMBER_FILE})"
minor="$$(./build-number -f ${KLEIO_PATCH_NUMBER_FILE})"
vn="$(major).$(minor)"
patch="$(major).$(minor).$(bn)"
cdate=$$(date "+%Y-%m-%d %H:%M:%S" )

.PHONY:

all: help

help: .PHONY
	@echo "usage:"
	@echo "  make build-local          build a local docker image and tag with new build number"
	@echo "  make tag-local-TAG        tag last local image with TAG in latest | stable"
	@echo "  make build-multi          build local mage and push multi platform docker images tagged with new build number"
	@echo "                              (requires a previous 'docker login')"
	@echo "  make tag-multi-TAG        tag last multi platform image with TAG in latest | stable"
	@echo "  make show-build           return the current build number"
	@echo "  make show-version         return the current version string (major.minor)"
	@echo "  make show-current         return the current version, build number"
	@echo "  make show-last            return the last image build date, version, build number"
	@echo "  make show-env             show KLEIO env variables currently defined"
	@echo "  make inc-NUMBER           increment version with NUMBER in major | minor"
	@echo "  make gen-token            generate a string suitable for KLEIO_ADMIN_TOKEN for .env file"
	@echo "  make bootstrap-token      generate and register a token for 'admin' during bootstrap"
	@echo "                                (only if no tokens exist and server running < 5 minutes)"
	@echo "  make docs                 generate api docs (requires postman_doc_gen and api files)"
	@echo "  make pull-tag tag=x.y.z    pull image with tag x.y.z from docker hub"

	@echo "  make kleio-run-latest    start server with latest multi platform image, .env config and tests/docker_compose.yaml"
	@echo "  make kleio-run-current    start server with most recent build, .env config and tests/docker_compose.yaml"
	@echo "  make kleio-run-tag tag=x.y.z    start server with image with tag x.y.z, .env config and tests/docker_compose.yaml"
	@echo "  make kleio-stop | stop    stop running server"
	@echo "  make test-semantics       run semantic tests"
	@echo "  make test-api             run api tests (requires newman (npm install newman))"



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

show-build: .PHONY
	@echo ${bn}

inc-build: .PHONY
	@./build-number inc -f ${KLEIO_BUILD_NUMBER_FILE}
inc-major: .PHONY
	@./build-number inc -f ${KLEIO_VERSION_NUMBER_FILE}
	@echo "0" > ${KLEIO_PATCH_NUMBER_FILE}
inc-minor: .PHONY
	@./build-number inc -f ${KLEIO_PATCH_NUMBER_FILE}

show-version: .PHONY
	@echo ${vn}

show-current: .PHONY
	@echo ${patch}

show-last: .PHONY
	@if ! test -f ${KLEIO_BUILD_DATE_FILE}; then echo "No previous image build"; else echo "$$(cat ${KLEIO_BUILD_DATE_FILE}) version ${patch}";  fi

check-env:
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; \
	exit 1; else exit 0; fi
	
	

build-local: inc-build prepare
	@docker build \
	 	-t "kleio-server" \
		-t "kleio-server:$(patch)" \
		-t "kleio-server:latest" \
		.build
	@echo "$(cdate)" > "${KLEIO_BUILD_DATE_FILE}"
	@echo "docker new kleio-server image build ${patch}"

# do multi architecture docker build, for arm64 and amd64
# requires docker buildx
# see https://www.docker.com/blog/multi-arch-build-and-images-the-simple-way/
build-multi: build-local
	-@docker buildx rm multibuilder
	@docker buildx create --platform linux/arm64,linux/amd64 \
	    --name multibuilder \
		--driver docker-container \
		--use
	@docker buildx build \
		--platform linux/arm64,linux/amd64 \
		--push \
		-t "${DOCKER_REPOSITORY}/kleio-server:$(patch)" \
		.build
	@echo "$(cdate)" > "${KLEIO_BUILD_DATE_FILE}"
	@echo "docker new kleio-server image build ${patch}"

tag-multi-latest: .PHONY
	docker buildx imagetools create timelinkserver/kleio-server:${patch} --tag timelinkserver/kleio-server:latest
	@echo "timelinkserver/kleio-server:${patch} multi arch image tagged as latest"
	
tag-multi-stable: 
	docker buildx imagetools create timelinkserver/kleio-server:${patch} \
		--tag timelinkserver/kleio-server:${vn}
	docker buildx imagetools create timelinkserver/kleio-server:${patch} \
		--tag timelinkserver/kleio-server:${major}
	@git tag ${patch}
	@echo "timelinkserver/kleio-server:{patch} multi arch image tagged as stable for ${major}} and ${vn}"

tag-local-latest:
	@docker tag kleio-server kleio-server:latest
	@echo "'latest' tag added to kleio-server latest image"

tag-local-stable:
	@docker tag kleio-server kleio-server:${vn}
	@docker tag kleio-server kleio-server:${major}
	@echo "${major} and '${vn}' tags added to kleio-server latest image "


gen-token:
	@echo "KLEIO_ADMIN_TOKEN=$$(openssl rand -hex 20)"


pull-tag:
	if [ -z "${tag}" ] ; then echo "ERROR: no tag specified. Use 'make pull-tag tag=1.2.3'"; exit 1; fi
	@docker pull ${DOCKER_REPOSITORY}/kleio-server:${tag}
	@docker tag ${DOCKER_REPOSITORY}/kleio-server:${tag} kleio-server:${tag}
	@echo "pulled latest image from ${DOCKER_REPOSITORY}/kleio-server:${tag}"

kleio-run-latest:  check-env
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	echo "Starting kleio-server with timelinkserver/kleio-server:latest port $$KLEIO_EXTERNAL_PORT";\
	declare | grep "^KLEIO"; \
	docker pull timelinkserver/kleio-server:latest;\
	docker compose up -d

kleio-run-tag: check-env kleio-stop
	if [ -z "${tag}" ] ; then echo "ERROR: no tag specified. Use 'make run-tag tag=1.2.3'"; exit 1; fi
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	export KLEIO_SERVER_IMAGE="kleio-server:${tag}"; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	declare | grep "^KLEIO"; \
	echo "Starting kleio-server with kleio-server:${tag}";\
	docker compose up -d

kleio-run-current: kleio-stop
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	export KLEIO_SERVER_IMAGE="kleio-server:${patch}"; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	declare | grep "^KLEIO"; \
	echo "Starting kleio-server with kleio-server:${patch} port $$KLEIO_EXTERNAL_PORT in dir $${KLEIO_HOME_DIR}";\
	docker compose up -d

show-env:
	@source .env; declare | grep "^KLEIO"; 

kleio-stop:
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	echo "KLEIO_HOME_DIR=$$KLEIO_HOME_DIR";\
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	echo "KHOME=$$KHOME";\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	echo "Stopping server at dir ${KLEIO_HOME_DIR}	";\
	docker compose config;\
	docker compose stop
	

kleio-start: kleio-run-current

start: kleio-start

stop: kleio-stop

bootstrap-token:
	@source .env; export BTOKEN=$$(cat "$$KLEIO_HOME_DIR/system/conf/kleio/.bootstrap_token");\
	echo "bootstrap token: $$BTOKEN";\
	curl -X POST \
     -H 'Content-Type: application/json' \
     -d '{"jsonrpc": "2.0","method": "users_invalidate","id":"make","params":{ "user": "admin","token":"'"$$BTOKEN"'"}}'\
	   http://localhost:$$KLEIO_EXTERNAL_PORT/json/ ;\
	 curl -X POST \
     -H 'Content-Type: application/json' \
     -d '{"jsonrpc": "2.0","method": "tokens_generate","id":"make",\
	 "params":{ "user": "admin","info":{"api":[\
		"sources",\
		"files",\
		"structures",\
		"translations",\
		"upload",\
		"delete",\
		"mkdir",\
		"rmdir",\
		"generate_token",\
		"invalidate_token",\
		"invalidate_user"\
	 ],"life_span":300},"token":"'"$$BTOKEN"'"}}'\
	   http://localhost:$$KLEIO_EXTERNAL_PORT/json/

compose-up:
    
test-semantics: .PHONY
	@cd tests; ./scripts/run_tests.sh

test-api: kleio-run-current
	@echo To run api tests install newman 
	@echo https://learning.postman.com/docs/running-collections/using-newman-cli/command-line-integration-with-newman/
	@source .env; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
    newman run api/postman/tests.json -e api/postman/tests.postman_environment.json --env-var "testadmintoken=$$KLEIO_ADMIN_TOKEN"  --env-var "KLEIO_HOME_DIR=$$KLEIO_HOME_DIR" -n 10 || true
	@echo 
	@echo See tests/README.md for tips on testing

docs: .PHONY
	@echo "Requires postman_doc_gen https://github.com/karthiks3000/postman-doc-gen"
	@echo "Requires Postman API export at ./api/api.json"
	@echo "Requires Postman environment export at ./api/environment.json"
	@echo "Generating doc..."
	@postman_doc_gen api/postman/api.json -o docs/api -e api/postman/environment.json

test-vargs:
	@echo $variable-args

test-vargs-unique:
