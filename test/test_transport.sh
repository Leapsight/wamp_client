#!/bin/bash

# Transport Test Runner Script
# This script runs the transport-specific tests for TCP and TLS
# Aligned with setup_wamp.sh configuration

set -e

echo "Running Transport Tests for WAMP Client"
echo "========================================"
echo "Tests aligned with setup_wamp.sh configuration:"
echo "- Realm: com.wamp_client.public"
echo "- Auth methods: anonymous, password, wampcra, cryptosign"  
echo "- Test user: john.doe (password: 123456)"
echo ""

# Change to project root directory (parent of test directory)
cd "$(dirname "$0")/.."

# Compile the transport module first
echo "Compiling transport modules..."
if command -v rebar3 &> /dev/null; then
    echo "Using rebar3 to compile..."
    rebar3 compile
else
    echo "Using erlc to compile transport module..."
    erlc -I src -o src src/awre_trans_tcp.erl
    erlc -I src -o src src/awre_transport.erl 2>/dev/null || echo "awre_transport.erl not found, skipping..."
    erlc -I src -o src src/awre_con.erl 2>/dev/null || echo "awre_con.erl not found, skipping..."
fi

if [ $? -eq 0 ]; then
    echo "✅ Transport modules compiled successfully"
else
    echo "❌ Transport module compilation failed"
    exit 1
fi

# Run transport-specific tests
echo ""
echo "Running transport tests..."

# Test with rebar3 if available
if command -v rebar3 &> /dev/null; then
    echo "Using rebar3 to run tests..."
    
    # Run specific test suites with groups
    echo ""
    echo "Running TCP transport tests..."
    rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=tcp_tests
    
    echo ""
    echo "Running TLS transport tests..."
    rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=tls_tests
    
    echo ""
    echo "Running authentication tests (aligned with setup_wamp.sh)..."
    rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=auth_tests
    
    echo ""
    echo "Running transport abstraction tests..."
    rebar3 ct --suite=test/awre_trans_tcp_SUITE --case=transport_abstraction_test,encoding_details_test,challenge_handling_test
    
    echo ""
    echo "Running integration tests..."
    rebar3 ct --suite=test/transport_integration_SUITE
    
else
    echo "rebar3 not found, running tests with direct Common Test..."
    
    # Run tests directly with ct_run
    ct_run -dir test -suite awre_trans_tcp_SUITE transport_integration_SUITE -pa src
fi

echo ""
echo "Transport tests completed!"
echo ""
echo "Test Coverage Summary:"
echo "✅ TCP Tests:"
echo "  - TCP connection establishment"
echo "  - TCP handshake protocol"
echo "  - TCP ping/pong messaging"
echo "  - TCP authentication (anonymous, password)"
echo "  - TCP error handling"
echo ""
echo "✅ TLS Tests:"
echo "  - TLS connection establishment"  
echo "  - TLS handshake protocol"
echo "  - TLS ping/pong messaging"
echo "  - TLS authentication (anonymous)"
echo "  - Custom SSL options"
echo "  - TLS error handling"
echo ""
echo "✅ Authentication Tests (setup_wamp.sh aligned):"
echo "  - Anonymous authentication"
echo "  - Password authentication (john.doe/123456)"
echo "  - WAMP-CRA authentication"
echo "  - Cryptosign authentication (with authorized keys)"
echo ""
echo "✅ Integration Tests:"
echo "  - Transport selection (TCP/TLS)"
echo "  - End-to-end workflows"
echo "  - WAMP client integration with setup_wamp.sh config"
echo "  - Error handling scenarios"
echo ""
echo "✅ Transport Abstraction:"
echo "  - Unified TCP/TLS interface"
echo "  - Message encoding/serialization"
echo "  - Challenge handling"