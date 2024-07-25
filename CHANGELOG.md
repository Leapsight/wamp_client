# CHANGE LOG

All notable changes to this project will be documented in this file. This project adheres to [Semantic Versioning](https://semver.org/).

## [1.3.0] - 2024 July

### Added
- Support for authentication process with the following auth methods:
    - `password`
    - `wampcra`
    Auth configuration in the router side:
    ```erlang
    auth => #{
        user => <<"my_authid">>,
        %% anonymous (default) | password | wampcra
        method => wampcra,
        secret => <<"my_secret">>
    }
    ``` 
    A new version of `awre` dependency library was required.

### Changed
- New version of the following dependency libraries:
    - **awre**
    - **pbkdf2**
    - **app_config**
    - **map_utils**
    - **backoff**

### TODO
- [] Add support for `cryptosign` auth method.

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

