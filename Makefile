PROJECT = rabbitmq_stamp
PROJECT_DESCRIPTION = RabbitMQ Exchange to provide unique incrementing identifiers for messages sent to it.
PROJECT_MOD = rabbit_stamp

define PROJECT_ENV
[
	    {exchange, <<"rabbitmq_stamp">>}
	  ]
endef

# sets the current version of rabbit rather than working of master
current_rmq_ref = v3.7.4

DEPS = rabbit_common rabbit amqp_client

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers ranch

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk

include erlang.mk
