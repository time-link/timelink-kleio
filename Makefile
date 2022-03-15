DOCKER_REPOSITORY=timelinkserver
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

all: help

help: .PHONY
	@echo "usage:"
	@echo "  make image                build docker image and tag with new build number"
	@echo "  make build                return the current build number"
	@echo "  make version              return the current version string (major.minor)"
	@echo "  make current              return the current version, build number"
	@echo "  make last                 return the last image build date, version, build number"
	@echo "  make inc-NUMBER           increment version with NUMBER in major | minor"
	@echo "  make token                generate a string for KLEIO_ADMIN_TOKEN for .env file"
	@echo "  make bootstrap-token      generate and register a token for 'admin' during bootstrap"
	@echo "                                (only if no tokens exist and server running < 5 minutes)"
	@echo "  make show-env             show KLEIO env variables currently defined"
	@echo "  make start                start Kleio server on docker (requires image)"
	@echo "  make stop                 stop Kleio server"
	@echo "  make docs                 generate api docs (requires postman_doc_gen and api files)"
	@echo "  make tag-TAG              tag last image with TAG in latest | unique | stable"
	@echo "  make push-TAG             push image with TAG in latest | unique | stable"
	@echo "  make kleio-run            start server with .env config and tests/docker_compose.yaml"
	@echo "  make kleio-stop           stop running server"
	@echo "  make test-semantics       run semantic tests"
	@echo "  make test-api             run api tests"



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

image: inc-build prepare
	@docker build \
	 	-t "kleio-server" \
		.build
	@echo "$(cdate)" > "${KLEIO_BUILD_DATE_FILE}"
	@echo "docker new kleio-server image build ${bn}"

tag-jrc-latest:
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:latest
	@docker tag kleio-server kleio-server:latest
	@echo "'latest' tag added to kleio-server  latest"

tag-jrc-unique:
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:${bn}
	@docker tag kleio-server kleio-server:${bn}
	@echo "${bn} tag added to kleio-server latest"

tag-jrc-stable:
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:${vn}
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:stable
	@docker tag kleio-server kleio-server:${vn}
	@docker tag kleio-server kleio-server:stable
	@echo "'stable' and '${vn}' tags added to kleio-server latest image"

push-latest-jrc:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:latest

push-unique-jrc:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:${bn}

push-stable-jrc:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:${vn}
	@docker push ${DOCKER_REPOSITORY}/kleio-server:stable

tag-latest:
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:latest
	@docker tag kleio-server kleio-server:latest
	@echo "'latest' tag added to kleio-server latest image"

tag-unique:
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:${bn}
	@docker tag kleio-server kleio-server:${bn}
	@echo "${bn} tag added to kleio-server latest image"

tag-stable:
	@docker tag kleio-server kleio-server:${vn}
	@docker tag kleio-server kleio-server:stable
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:${vn}
	@docker tag kleio-server ${DOCKER_REPOSITORY}/kleio-server:stable
	@echo "'stable' and '${vn}' tags added to kleio-server latest image "

push-latest:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:latest

push-unique:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:${bn}

push-stable:
	@docker push ${DOCKER_REPOSITORY}/kleio-server:${vn}
	@docker push ${DOCKER_REPOSITORY}/kleio-server:stable

token:
	@echo "KLEIO_ADMIN_TOKEN=$$(openssl rand -hex 20)"

kleio-run: 
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	echo "Starting kleio-server";\
	declare | grep "^KLEIO"; \
	docker compose up -d

show-env:
	@source .env;\
	declare | grep "^KLEIO"; 

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

kleio-stop:
	@if [ ! -f ".env" ]; then echo "ERROR: no .env file in current directory. Copy .env-sample and change as needed." ; fi
	@source .env; \
	if [ -e "$$PWD/tests/kleio-home" ]; then export KHOME="$$PWD/tests/kleio-home" ;  fi;\
	if [ -z "$$KLEIO_HOME_DIR" ]; then export KLEIO_HOME_DIR="$$KHOME"; fi;\
	if [ -z "$$KLEIO_SERVER_PORT" ]; then export KLEIO_SERVER_PORT="8088"; fi;\
	if [ -z "$$KLEIO_EXTERNAL_PORT" ]; then export KLEIO_EXTERNAL_PORT="8089"; fi;\
	export KLEIO_USER=$$(id -u):$$(id -g);\
	docker compose stop
	

kleio-start: kleio-run

start: kleio-run

stop: kleio-stop

compose-up:
    
test-semantics: .PHONY
	@cd tests; ./scripts/run_tests.sh

test-api: .PHONY
	@export KLEIO_ADMIN_TOKEN=mytoken;\
      newman run api/postman/tests.json -e api/postman/tests.postman_environment.json --env-var "testadmintoken=$$KLEIO_ADMIN_TOKEN"
	@echo To run api tests install newman 
	@echo https://learning.postman.com/docs/running-collections/using-newman-cli/command-line-integration-with-newman/
	@echo Kleio server must be running. 
	@echo See tests/README.md for tips

docs: .PHONY
	@echo "Requires postman_doc_gen https://github.com/karthiks3000/postman-doc-gen"
	@echo "Requires Postman API export at ./api/api.json"
	@echo "Requires Postman environment export at ./api/environment.json"
	@echo "Generating doc..."
	@postman_doc_gen api/postman/api.json -o docs/api -e api/postman/environment.json
