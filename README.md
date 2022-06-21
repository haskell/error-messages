# Haskell Errors

So you would like to help improve the error messages we see in the Haskell toolchain and ecosystem?

Welcome! We're glad to have you, and we will do our best to ensure your efforts are worthwhile.

## Project Goals

* Serve the Haskell community, focusing on improvements to the error messages seen in the Haskell ecosystem.

* Lower Haskell's barrier-to-entry by increasing the general understanding of the error messages experienced during development.


## What and How?

This project is taking shape, so many details are still being worked out, however:

* Using the issue tracker, this repo will provide a space and workflow for the discussion and collaboration around improving error messages.
* We are interested in building a catalog of error messages for each tool in common use (GHC, Cabal, Stack, ghcup, HLS, etc).
* For each error message that is identified for potential improvement:
  * We would like to have a reasonable means of reproducing the error message under different circumstances
  * We should identify specific improvements to the messaging
  * We will reach out to and work collaboratively with tool developers
  * We should document and help improve understanding of the error messaging
* For tools that use error message codes:
  * We describe the messages and how to interpret them in the Haskell Message Index (see below)

As a result, this repo may include code that demonstrates the errors we wish to improve, as well as code and material to render documentation relevant to explaining each error and the possible situations the error relates to.


## Contributor Expectations

We welcome contributions that help to further progress the project towards its goals.

Contributions may come in the form of changes to the code base, as well as opening or commenting on issues and pull requests.

All contributors are expected to follow the [Haskell Foundation's Code of Conduct](https://haskell.foundation/guidelines-for-respectful-communication/).

## The Haskell Message Index

This repository previously hosted the Haskell Message Index, an effort to document the existing errors and warnings of Haskell tooling. It has been moved into [its own repository](https://github.com/haskellfoundation/error-message-index).
