RabbitMq-Stamp
--------------

A custom RabbitMQ Exchange to provide unique incrementing identifiers for messages sent to it.

The exchange "stamps" the message header with the identifier then forward the message to a destination exchange.

The exchange type is "x-stamp".

Supplied message should contain the message header - forward-exchange e.g.

```
forward-exchange {your exchange name}
```

The forwarded message will contain the header

```
stamp {some number}
```

Each exchange of type "x-stamp" will maintain their own incrementing identifier.


Build
-----

Follow the instructions at http://www.rabbitmq.com/plugin-development.html to get a working RabbitMQ build environment.

My project https://github.com/mdevilliers/vagrant-rabbitmq-development-environment provides a vagrant script for automating the provisioning of a development environment.

Checkout this project to a folder under the rabbitmq-public-umbrella project

Run the tests

```
cd rabbitmq-stamp
make test
```

Make the project

```
cd rabbitmq-stamp
make
```

Deploy
------

Copy to your plugins directory in Rabbit
Restart Rabbit




