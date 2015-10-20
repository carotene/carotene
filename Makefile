PROJECT = carotene

DEPS = bullet amqp_client eredis_smart_sub jsx getopt uuid
dep_getopt = git https://github.com/jcomellas/getopt v0.8.2
dep_bullet = git https://github.com/extend/bullet.git master
dep_amqp_client = git git://github.com/jbrisbin/amqp_client.git rabbitmq-3.5.0
dep_eredis_smart_sub = git git://github.com/nacmartin/eredis_smart_sub.git master
dep_jsx = git git://github.com/talentdeficit/jsx.git v2.4.0
dep_uuid = git git://github.com/avtobiff/erlang-uuid.git v0.4.7

TEST_DEPS = meck
dep_meck = git git://github.com/eproxus/meck.git 0.8.2

.PHONY: release clean-release

#release: clean-release all
#	relx -o rel/$(PROJECT)
#
#clean-release:
#	rm -rf rel/$(PROJECT)
#
include erlang.mk

#ERLC_OPTS= $(ERLC_COMPILE_OPTS) +debug_info
