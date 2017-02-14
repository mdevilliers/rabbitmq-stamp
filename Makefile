PROJECT = rabbitmq_stamp
PROJECT_DESCRIPTION = RabbitMQ Exchange to provide unique incrementing identifiers for messages sent to it.
PROJECT_MOD = rabbit_stamp

define PROJECT_ENV
[
	    {exchange, <<"rabbitmq_stamp">>}
	  ]
endef

DEPS = rabbit_common rabbit amqp_client
dep_rabbit_common_commit = rabbitmq_v3_6_6
dep_rabbit_commit = rabbitmq_v3_6_6
dep_amqp_client_commit = rabbitmq_v3_6_6

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
