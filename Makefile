PROJECT=eventi

DEPS = eleveldb ranch

dep_eleveldb = https://github.com/basho/eleveldb master
dep_ranch = pkg://ranch master

PLT_APPS=kernel stdlib

include erlang.mk