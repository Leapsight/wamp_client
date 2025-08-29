# Transport Tests for WAMP Client

This directory contains comprehensive test suites for testing both TCP and TLS transport functionality in the WAMP client.

## Test Suites

### 1. `awre_trans_tcp_SUITE.erl`
Main test suite for the unified TCP/TLS transport module.

**TCP Tests:**
- `tcp_connection_test` - Basic TCP connection establishment
- `tcp_handshake_test` - TCP handshake protocol validation
- `tcp_ping_pong_test` - Ping/pong frame handling over TCP
- `tcp_auth_anonymous_test` - Anonymous authentication over TCP
- `tcp_auth_password_test` - Password authentication over TCP
- `tcp_connection_error_test` - TCP connection error handling

**TLS Tests:**
- `tls_connection_test` - Basic TLS connection establishment
- `tls_handshake_test` - TLS handshake protocol validation  
- `tls_ping_pong_test` - Ping/pong frame handling over TLS
- `tls_auth_anonymous_test` - Anonymous authentication over TLS
- `tls_ssl_options_test` - Custom SSL options handling
- `tls_connection_error_test` - TLS connection error handling

**Abstraction Tests:**
- `transport_abstraction_test` - Transport layer abstraction validation
- `encoding_details_test` - Message encoding/serialization tests
- `challenge_handling_test` - Authentication challenge processing

**Authentication Tests (aligned with setup_wamp.sh):**
- `anonymous_auth_test` - Anonymous authentication as configured in setup script
- `password_auth_test` - Password authentication using john.doe credentials 
- `wampcra_auth_test` - WAMP-CRA authentication with configured user
- `cryptosign_auth_test` - Cryptosign authentication with authorized keys

### 2. `transport_integration_SUITE.erl`
Integration tests that validate the complete transport selection and functionality.

**Integration Tests:**
- `tcp_transport_selection_test` - Verify TCP transport selection
- `tls_transport_selection_test` - Verify TLS transport selection  
- `transport_switch_test` - Test switching between TCP/TLS
- `end_to_end_tcp_test` - Complete TCP workflow validation
- `end_to_end_tls_test` - Complete TLS workflow validation
- `error_handling_test` - Comprehensive error scenario testing
- `wamp_client_integration_anonymous_test` - Integration test with anonymous auth from setup_wamp.sh
- `wamp_client_integration_password_test` - Integration test with john.doe credentials from setup_wamp.sh

## Running the Tests

### Option 1: Using the Test Script
```bash
# From project root:
./test/test_transport.sh

# From test directory:
cd test && ./test_transport.sh
```

### Option 2: Using rebar3
```bash
# Run all transport tests
rebar3 ct --suite=test/awre_trans_tcp_SUITE,test/transport_integration_SUITE

# Run specific test suites
rebar3 ct --suite=test/awre_trans_tcp_SUITE
rebar3 ct --suite=test/transport_integration_SUITE

# Run specific test groups
rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=tcp_tests
rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=tls_tests
rebar3 ct --suite=test/awre_trans_tcp_SUITE --group=auth_tests

# Run specific authentication tests aligned with setup_wamp.sh
rebar3 ct --suite=test/awre_trans_tcp_SUITE --case=anonymous_auth_test,password_auth_test,wampcra_auth_test,cryptosign_auth_test
```

### Option 3: Using Common Test directly
```bash
ct_run -dir test -suite awre_trans_tcp_SUITE transport_integration_SUITE -pa src
```

## Test Architecture

### Mock Servers
The tests include mock TCP and TLS servers that:
- Accept connections on random ports
- Handle basic handshake protocols
- Echo back data for testing
- Support both TCP and TLS protocols

### Transport Abstraction Testing
Tests validate that:
- The same module handles both TCP and TLS
- Transport selection works based on the `tls` flag
- SSL options are properly merged and applied
- Error conditions are handled gracefully

### Authentication Testing
Tests cover all supported authentication methods:
- Anonymous authentication
- Password-based authentication  
- WAMP-CRA challenge-response
- Cryptosign authentication

## Configuration

### SSL/TLS Testing
TLS tests use self-signed certificates generated at runtime. The tests:
- Create temporary certificate files
- Use `verify_none` for testing purposes
- Handle TLS server setup failures gracefully
- Skip TLS tests if SSL setup fails

### Test Data
Tests use data aligned with `setup_wamp.sh` configuration:
- Realm: `<<"com.wamp_client.public">>`
- Version: `<<"test-1.0">>`
- Host: `"localhost"`
- Ports: Randomly assigned by mock servers
- Test User: `john.doe` with password `123456`
- Anonymous User: `anonymous`
- Authorized Key: `1766c9e6ec7d7b354fd7a2e4542753a23cae0b901228305621e5b8713299ccdd`

## Test Coverage

The test suite provides comprehensive coverage for:

✅ **Connection Management**
- TCP socket creation and management
- TLS socket creation and SSL handshake
- Connection error handling
- Socket cleanup on shutdown

✅ **Protocol Handling**  
- WAMP handshake packet construction
- Ping/pong frame handling
- Message serialization/deserialization
- Protocol error responses

✅ **Authentication**
- Anonymous authentication flows
- Password authentication 
- Challenge-response mechanisms
- Authentication error scenarios

✅ **Transport Abstraction**
- Unified TCP/TLS interface
- Transport selection logic
- SSL option configuration
- Transport-specific message handling

✅ **Integration Scenarios**
- End-to-end connection workflows
- Transport switching capabilities
- Error recovery and handling
- Real-world usage patterns

## Troubleshooting

### Common Issues

**TLS Tests Failing:**
- Ensure OpenSSL is available
- Check that SSL application is started
- Verify certificate generation permissions

**Port Conflicts:**
- Tests use random ports to avoid conflicts
- If tests hang, check for zombie server processes

**Permission Errors:**
- Ensure write permissions in `/tmp/` for certificates
- Check that test files are executable

### Debug Mode
Enable verbose logging by setting:
```bash
export CT_VERBOSE=1
./test/test_transport.sh
```

## Future Enhancements

Potential improvements for the test suite:
- Add performance benchmarks for TCP vs TLS
- Include IPv6 connection testing
- Add concurrent connection stress tests
- Implement property-based testing scenarios
- Add WebSocket transport tests (if implemented)