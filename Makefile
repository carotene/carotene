PROJECT = carotene

DEPS = cowboy amqp_client eredis_smart_sub jsx getopt
dep_getopt = git https://github.com/jcomellas/getopt v0.8.2
dep_cowboy = git https://github.com/extend/cowboy.git 2.0.0-pre.1
dep_amqp_client = git git://github.com/jbrisbin/amqp_client.git rabbitmq-3.4.0-community
dep_eredis_smart_sub = git git://github.com/nacmartin/eredis_smart_sub.git master
#dep_meck = git git://github.com/eproxus/meck.git 0.8.2
dep_jsx = git git://github.com/talentdeficit/jsx.git v2.4.0

.PHONY: release clean-release

#release: clean-release all
#	relx -o rel/$(PROJECT)
#
#clean-release:
#	rm -rf rel/$(PROJECT)
#
include erlang.mk

ERLC_OPTS= $(ERLC_COMPILE_OPTS) +debug_info

