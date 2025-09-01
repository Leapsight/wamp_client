# CHANGE LOG

All notable changes to this project will be documented in this file. This project adheres to [Semantic Versioning](https://semver.org/).

## [2.0.0] - 2025 August

### Major Changes
- **BREAKING CHANGE**: Upgraded to OTP27 minimum requirement
- **BREAKING CHANGE**: Embedded awre library directly into wamp_client to eliminate external dependency
- **BREAKING CHANGE**: Embedded wamper protocol libraries (converter, protocol, validator) to eliminate external dependencies
- **BREAKING CHANGE**: Removed app_config dependency - wamp_client now configured directly through supervisor
- **BREAKING CHANGE**: Replaced jsone with standard json library for JSON serialization

### Added
- New embedded modules:
  - `awre.erl` - WAMP router interface
  - `awre_con.erl` - WAMP connection management
  - `awre_sup.erl` - AWRE supervisor
  - `awre_trans_local.erl` - Local transport implementation
  - `awre_trans_tcp.erl` - Unified TCP/TLS transport implementation
  - `awre_transport.erl` - Transport abstraction layer
  - `wamper_converter.erl` - WAMP message conversion
  - `wamper_protocol.erl` - WAMP protocol implementation
  - `wamper_validator.erl` - WAMP message validation
- New header files:
  - `wamper_header_mapping.hrl` - Header mappings for WAMP protocol
  - `wamper_message_codes.hrl` - WAMP message type codes
  - `supervision_spec.hrl` - Supervision specifications
- **TLS Support Enhancements**:
  - Unified TCP/TLS transport module handling both protocols seamlessly
  - Configurable TLS options with SSL certificate validation
  - Transport abstraction that automatically selects TCP or TLS based on configuration
  - Comprehensive test suites for both TCP and TLS connections
- **Testing Infrastructure**:
  - `awre_trans_tcp_SUITE.erl` - Comprehensive TCP/TLS transport tests
  - `transport_integration_SUITE.erl` - End-to-end integration tests
  - `test_transport.sh` - Automated test runner with detailed reporting
  - Mock servers supporting both TCP and TLS protocols for testing
- **Security Enhancements**:
  - `wamp_client_sensitive.erl` - Comprehensive sensitive data protection module
  - Authentication details are now automatically wrapped to prevent exposure in logs, crash dumps, and debugging output
  - Sensitive data wrapping/unwrapping utilities with backward compatibility
  - Enhanced `format_status/1` callbacks to sanitize state data in status reports

### Changed
- Upgraded minimum OTP version to 27.0
- Simplified configuration system by removing app_config dependency
- Updated supervision tree structure for better application integration
- Improved process registration using proper atom naming conventions
- Enhanced error handling and authentication details in router spec
- **Transport Layer Improvements**:
  - Unified TCP and TLS handling in single module (`awre_trans_tcp`)
  - Enhanced transport abstraction with automatic protocol selection
  - Improved SSL options handling and certificate validation
  - Better error handling for connection failures and transport issues
- **Build and Development Tools**:
  - Added Elvis linting configuration with customized rules
  - Enhanced rebar3 configuration with linting support
  - Added formatting tools and code style enforcement
- Updated dependency versions:
  - `gproc` (now required in applications list)
  - `maps_utils` upgraded to version 1.1.0
  - `wamp` dependency updated to develop_otp27 branch
  - Added `jsone`, `msgpack`, and `backoff` dependencies
- Removed external dependencies:
  - `awre` (now embedded)
  - `app_config` (replaced with direct supervisor configuration)

### Fixed
- Fixed atom name generation for process registrations
- Improved error response consistency with `wamp_service`
- Enhanced transport layer reliability and connection handling
- **TLS Transport Fixes**:
  - Resolved SSL connection establishment issues
  - Fixed transport selection logic for TCP vs TLS
  - Improved certificate validation and SSL option handling
  - Enhanced error reporting for TLS connection failures
- **Security Fixes**:
  - Fixed double-wrapping of sensitive authentication data that caused function_clause errors
  - Resolved `wamp_client_sensitive:unwrap/1` handling of already-wrapped data
  - Fixed deprecated `format_status/2` callbacks to use modern `format_status/1` format
  - Corrected authentication flow to properly unwrap sensitive data before use
  - Enhanced sensitive data protection to prevent accidental exposure in error messages

### Migration Guide
This is a major version upgrade with breaking changes:
1. **OTP Version**: Upgrade to OTP 27 or later
2. **Configuration**: Replace app_config-based configuration with direct supervisor configuration
3. **Dependencies**: Remove `awre` and `app_config` from your dependencies - they are now embedded
4. **TLS Configuration**: Update TLS connection parameters to use new unified transport API:
   - Set `tls => true` in connection configuration for TLS connections
   - SSL options are now handled automatically by the transport layer
   - Verify TLS certificates are properly configured for production use
5. **Testing**: Use new comprehensive test suites for validation:
   - Run `./test/test_transport.sh` for complete transport testing
   - Validate both TCP and TLS functionality in your environment
6. **Application Structure**: Review supervision tree changes for application integration

## [1.4.3] - 2025 January
### Changed
- Updated error responses to inform the error data as `details` instead of include it in `args`. 
This change is needed for consistency with `wamp_service` the error is returned as `details` when an exception is processed


## [1.4.2] - 2024 October
### Changed
- Upgrade awre version to `2.0.1`: Normalizes an Ed25519 private key to ensure it is in the 32-byte format required for signing operations, accepting either a 32-byte or 64-byte binary key.

## [1.4.1] - 2024 October
### Changed
- Upgrade awre version to `2.0.0`

## [1.4.0] - 2024 August
### Changed
- Were removed the following dependency libraries:
    - **enacl**
    - **pbkdf2**
    It allows to avoid the NIF for **libsodium**! and we use the Erlang crypto library instead.

## [1.3.0] - 2024 July

### Added
- Support for authentication process with the following auth methods:
    - `password`
    - `wampcra`
    - `cryptosign`: It requires **libsodium**!

    Auth configuration in client for the router side:
    ```erlang
    auth => #{
        user => <<"my_authid">>,
        %% anonymous (default) | password | wampcra | cryptosign
        method => wampcra,
        %% for password & wampcra
        secret => <<"my_secret">>,
        %% for cryptosign
        pubkey => <<"my_pubkey">>,
        privkey => <<"my_privkey">>
    }
    ``` 
    A new version of `awre` dependency library was required and also `enacl` was added.

### Changed
- New version of the following dependency libraries:
    - **awre**
    - **pbkdf2**
    - **app_config**
    - **map_utils**
    - **backoff**

### TODO
- [x] Add support for `cryptosign` auth method.

## [1.2.4] - 2024 June

### Changed
- Minor changes in the log level

## [1.2.3] - 2023 November

### Added
- Initial releases tracking of the project.

### Changed
> Changes versus previous versions:
- New module `wamp_client_peer` to replace `wamp_client`
- Allows pools of WAMP Peers, each one with its own TCP connection to the router
- The Handler API has been completely redesigned
- The configuration has been completely redesigned

