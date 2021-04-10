#!/bin/bash

# set -e

# Create Realm
## Create Realm
curl -X "POST" "http://localhost:18081/realms/" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -H 'Accept: application/json; charset=utf-8' \
     -d $'{
  "uri": "com.magenta.test",
  "description": "Test"
}'

## Disable Security
curl -X "DELETE" "http://localhost:18081/realms/com.magenta.test/security_enabled" \
     -H 'Accept: application/json; charset=utf-8' \
     -u 'admin:bondy'

