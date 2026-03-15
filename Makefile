# Copyright 2019-present Cornell University
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

NAME=petr4
WEB_EXAMPLES=examples/checker_tests/good/table-entries-lpm-bmv2.p4
WEB_EXAMPLES+=examples/checker_tests/good/switch_ebpf.p4
WEB_EXAMPLES+=examples/checker_tests/good/union-valid-bmv2.p4
WEB_EXAMPLES+=stf-test/custom-stf-tests/register.p4

.PHONY: all build claims clean test test-stf web ci-test

EXCLUDES_STATIC := $(shell find testdata/excludes/static -name '*.exclude' ! -path 'testdata/excludes/static/bug/*' -printf '-e %p ')
EXCLUDES := $(shell find testdata/excludes -name '*.exclude' ! -path 'testdata/excludes/static/bug/*' -printf '-e %p ')

all: build

default: build

build:
	dune build @install && echo

doc:
	dune build @doc

run:
	dune exec -- $(NAME)

install:
	dune install

claims:
	@test/claims.py

ci-test:
	dune exec -- bin/test.exe
	cd test && dune exec -- ./test.exe test -q

test-stf:
	dune exec -- bin/test.exe

test:
	cd test && dune exec -- ./test.exe

sim-p4c-v1model:
	mkdir -p /evaluation/p4c/petr4/dynamic
	ALCOTEST_COLOR=never opam exec -- \
	dune exec bin/test.exe -- -e petr4.exclude $(EXCLUDES) testdata/p4c/v1model/ | tee /evaluation/p4c/petr4/dynamic/v1model.log
	@echo "Test log recorded in /evaluation/p4c/petr4/dynamic/v1model.log"

sim-p4c-ebpf:
	mkdir -p /evaluation/p4c/petr4/dynamic
	ALCOTEST_COLOR=never opam exec -- \
	dune exec bin/test.exe -- -e petr4.exclude $(EXCLUDES) testdata/p4c/ebpf/ | tee /evaluation/p4c/petr4/dynamic/ebpf.log
	@echo "Test log recorded in /evaluation/p4c/petr4/dynamic/ebpf.log"

sim-p4testgen-v1model:
	mkdir -p /evaluation/p4testgen/petr4/dynamic
	ALCOTEST_COLOR=never opam exec -- \
	dune exec bin/test.exe -- -e petr4.exclude $(EXCLUDES) testdata/p4testgen/v1model/ | tee /evaluation/p4testgen/petr4/dynamic/v1model.log
	@echo "Test log recorded in /evaluation/p4testgen/petr4/dynamic/v1model.log"

sim-p4testgen-ebpf:
	mkdir -p /evaluation/p4testgen/petr4/dynamic
	ALCOTEST_COLOR=never opam exec -- \
	dune exec bin/test.exe -- -e petr4.exclude $(EXCLUDES) testdata/p4testgen/ebpf/ | tee /evaluation/p4testgen/petr4/dynamic/ebpf.log
	@echo "Test log recorded in /evaluation/p4testgen/petr4/dynamic/ebpf.log"

pos:
	mkdir -p /evaluation/p4c/petr4/static
	ALCOTEST_COLOR=never opam exec -- \
	dune exec test/test.exe -- -e petr4.exclude $(EXCLUDES_STATIC) -pos | tee /evaluation/p4c/petr4/static/pos.log

neg:
	mkdir -p /evaluation/p4c/petr4/static
	ALCOTEST_COLOR=never opam exec -- \
	dune exec test/test.exe -- -e petr4.exclude $(EXCLUDES_STATIC) -neg | tee /evaluation/p4c/petr4/static/neg.log

clean:
	dune clean

web:
	dune build _build/default/web/web.bc.js --profile release && cp _build/default/web/web.bc.js web/html_build/
