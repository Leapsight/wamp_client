# CHANGE LOG

All notable changes to this project will be documented in this file. This project adheres to [Semantic Versioning](https://semver.org/).

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

