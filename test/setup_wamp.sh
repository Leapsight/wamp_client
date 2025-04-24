#!/bin/bash

# set -e

## Create Realm
curl -X "POST" "http://localhost:18081/realms/" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d $'{
     "uri": "com.wamp_client.public",
     "description": "Wamp Client Test Realm",
     "authmethods": ["anonymous", "password", "wampcra", "cryptosign"]
}'

## Add Group to the Realm
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/groups' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "groups": [],
    "meta": {
        "description": "Wamp Client Test Group"
    },
    "name": "wamp_client_group"
}'

## Add Grants to the Realm
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/grants' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "permissions": [
        "wamp.register",
        "wamp.unregister",
        "wamp.subscribe",
        "wamp.unsubscribe",
        "wamp.call",
        "wamp.cancel",
        "wamp.publish"
    ],
    "resources": [
          {
               "uri": "com.example.",
               "match": "prefix"
          }
    ],
    "roles": ["wamp_client_group"]
}'
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/grants' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "permissions": [
        "wamp.register",
        "wamp.unregister",
        "wamp.subscribe",
        "wamp.unsubscribe",
        "wamp.call",
        "wamp.cancel",
        "wamp.publish"
    ],
    "resources": [
          {
               "uri": "com.example.",
               "match": "prefix"
          }
    ],
    "roles": ["anonymous"]
}'

## Add Sources to the Realm
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/sources' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "usernames": ["anonymous"],
    "authmethod": "anonymous",
    "cidr" : "0.0.0.0/0"
}'
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/sources' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "usernames": "all",
    "authmethod": "wampcra",
    "cidr" : "0.0.0.0/0"
}'
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/sources' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "usernames": "all",
    "authmethod": "password",
    "cidr" : "0.0.0.0/0"
}'
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/sources' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
    "usernames": "all",
    "authmethod": "cryptosign",
    "cidr" : "0.0.0.0/0"
}'

# curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/sources' \
#      -H 'Content-Type: application/json; charset=utf-8' \
#      -H 'Accept: application/json; charset=utf-8' \
#      -d '{
#     "usernames": "john.doe",
#     "authmethod": "cryptosign",
#     "cidr" : "0.0.0.0/0"
# }'

## Add User to the Realm
curl -X "POST" 'http://localhost:18081/realms/com.wamp_client.public/users' \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d '{
     "username": "john.doe",
     "password": "123456",
     "authorized_keys": ["1766c9e6ec7d7b354fd7a2e4542753a23cae0b901228305621e5b8713299ccdd"],
     "groups": ["wamp_client_group"]
}'

## Disable Security
# curl -X "DELETE" "http://localhost:18081/realms/com.wamp_client.public/security_enabled" \
#      -H 'Accept: application/json; charset=utf-8' \
#      -u 'admin:bondy'

