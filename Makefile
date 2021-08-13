
PROJECT_NAME            := amazonka-contrib-rds-utils

# https://www.gnu.org/software/make/manual/html_node/Special-Variables.html
# https://ftp.gnu.org/old-gnu/Manuals/make-3.80/html_node/make_17.html
PROJECT_MKFILE_PATH     := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
PROJECT_MKFILE_DIR      := $(shell cd $(shell dirname $(PROJECT_MKFILE_PATH)); pwd)

PROJECT_ROOT            := $(PROJECT_MKFILE_DIR)
DISTRIBUTIONS           := $(PROJECT_MKFILE_DIR)/dist-newstyle/sdist


build: $(PROJECT_ROOT)/src $(PROJECT_ROOT)/$(PROJECT_NAME).cabal
	cabal v2-build

.PHONY: run
run:
	cabal v2-run generate-db-auth-token -- --help

.PHONY: run-example
run-example:
	AWS_ACCESS_KEY_ID=111 AWS_SECRET_ACCESS_KEY=222 \
	cabal v2-run \
		generate-db-auth-token --       \
			--hostname example-host.com \
			--port     6543 		    \
			--username example_user		\
			--region   eu-west-2

.PHONY: distribute
distribute:
	cabal v2-sdist

.PHONY: publish
publish: | cleanall distribute
	cabal upload $(DISTRIBUTIONS)/$(PROJECT_NAME)-*.tar.gz

.PHONY: release
release:
	cabal upload $(DISTRIBUTIONS)/$(PROJECT_NAME)-*.tar.gz --publish

.PHONY: cleanall
cleanall:
	rm -r $(DISTRIBUTIONS)
