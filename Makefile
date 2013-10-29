PROJECT=eventi

ERLC_OPTS = -Werror +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard '+{parse_transform, lager_transform}'

DEPS = eleveldb ranch lager

dep_lager = https://github.com/basho/lager master
dep_eleveldb = https://github.com/basho/eleveldb master
dep_ranch = pkg://ranch master

PLT_APPS=kernel stdlib crypto

include erlang.mk

console:
	./_rel/bin/eventi console -pa ../ebin
