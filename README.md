# Haskell Attestation Manager

## Overview
---

The Haskell Attestation Manager is a collection of Haskell libraries and executables that support the design and prototyping of layered attestation protocols.  It builds off of the [Copland effort](https://ku-sldg.github.io/copland/) to provide a concrete implementation of the Copland semantics and a testing ground for experimental extensions (to both the Copland language proper and its supporting envioronment).

We have ongoing work to implement similar Attestation Managers in other language environments(see:  [Copland Software](https://ku-sldg.github.io/copland/software.html)), and our [JSON message exchange format](https://ku-sldg.github.io/copland///resources/copland_core.pdf) is designed to facilitate communication amongst protocols whose executions span these diverse environments.  The ultimate goal of the Copland effort is to build formally verified attestation protocols and infrastructure.  This prototype serves as a testing ground towards that goal.

<!---
[Orchestrating Layered Attestations](https://ku-sldg.github.io/copland///resources/copland-post-2019.pdf)  --->

Please send questions/comments to Adam Petz(ampetz@ku.edu) or submit a [GitHub issue](https://github.com/ku-sldg/haskell-am/issues).

## Installation
---

### Dependencies
* Haskell Stack:  https://docs.haskellstack.org/en/stable/README/
* Haskell AM source: https://github.com/ku-sldg/haskell-am.git

### QuickStart Guide
1) Install Haskell Stack (via instructions at the link above)
1) Clone the haskell-am source repository (git clone https://github.com/ku-sldg/haskell-am.git)
1) In the top-level of that repository:  type `make`
    * NOTE:  this may take a while (~20-30 minutes) the first time due to the Haskell dependencies.
1) Type `make run`
1) Successful installation/execution will include output that ends with something like "Appraise Result:" followed by a pretty-printed evidence value (the result of executing a hard-coded Copland protocol).  See the Examples section below for a description of what `make run` does.

<!-- * This will use a default key for signing (located at `keys/key0.txt`) by _**temporarily**_ setting the `COPLAND_BUILD` environment variable.  See the [Advanced Configuration](#advanced-configuration) section for instructions on setting this variable (or the custom `COPLAND_KEY` variable) more permanently for development/deployment purpuses. -->

## Haskell AM Executables
---

The Haskell AM project is organized as three logically distinct executables:

1)  [Attestation Manager Client (AM Client)](#attestation-manager-client-appraiser-client)
1)  [Copland Virtual Machine Server (CVM Server)](#copland-interpreter-server-attestation-server)
1)  [Datatype/JSON Generator and Translator](#datatypejson-generator-and-translator)

These executables share common libraries(see [Source Files](#Source-Files) section below).  Their purpose and usage are described individually in the following sections.

---

### Attestation Manager Client (Appraiser Client)

The Appraiser Client can generate nonces, sequence exection of multiple Copland phrases, gather evidence results, provide evidence results as initial evidence to other Copland phrase executions, and perform appraisal over evidence.  It is responsible for sending the first request during an attestation protocol run, and also performs the final appraisal.

*  `-w` spawns attestation servers as separate background threads before performing the client actions.  This option is useful for testing purposes-  it alleviates the hassle of configuring and starting each Attestation Server as its own executable.  The appropriate number of servers (and at appropriate addresses) are spawned based on the protocol terms involved (and the custom name mapping if provided via the  `-n` option).
*  `-n FILENAME` allows specifying a mapping from Place to Address in the file FILENAME.  The format of the file must be newline-separated strings of the form:  \<Place\>:\<Address\>, where \<Place\> is a place identifier(a number) and \<Address\> is an address string (now just a port number, but may become more sophisticated).  An example mapping in a file looks like:
    ```
    0:3000
    1:3001
    2:3002
    ```

    The appraiser client will package and forward this mapping in its requests to the attestation servers so that they share a notion of the Place->Address association.   If `-n` is omitted, random available ports are assigned to each Place involved.

    NOTE:  You may only omit `-n` when `-w` is set.  This is because it only makes sense to randomly generate server ports when the appraiser client is also responsible for spawning the server threads.  When `-w` is NOT set the user is responsible for manually configuring and starting attestation servers at appropriate Addresses according to the name mapping provided via `-n`.

*  `-v` option specifies that spawned servers should run in simulation mode.  This option only has effect if `-w` is also set.
*  `-t FILENAME1` and `-e FILENAME2` are a convenient way to specify execution of a *single* Copland phrase and initial evidence.
*  If the `-t` option is empty or omitted, a hard-coded protocol written as a computation in the AM monad will run instead.
*  The main module for the Appraisal Client is [ClientMain.hs](https://github.com/ku-sldg/haskell-am/blob/master/copland-interp/app/ClientMain.hs)
*  Type `make helpclient` for a complete description of Appraiser Client command line options.

AM capabilities:
1) generating and managing nonces
1) sequencing execution of multiple Copland protocol phrases
1) performing appraisal on the resulting evidence bundles

---

### Copland Virtual Machine Server (CVM Server)
#### Documentation forthcoming...
---

<!-- 
### Copland Interpreter Server (Attestation Server)

An Attestation Server handles requests from clients that ask it to interpret a Copland phrase.

*  Note:  The Attestation Server may also act as an Attestation Client.  When it encounters an AT term in a Copland phrase it must itself send a request to the place specified.
*  `-r ADDRESS` specifies ADDRESS as the port where the server should listen for connections.  If ommited, a random available port is selected.
*  In the `RequestMessage` a client includes a mapping from Place to Address where Address is currently a port string.  This tells the server the intended Address of each Place it encounters in the Copland phrase.
*  A full description of the Request/Response Messages handled by the server and their JSON representations is included in this document:  [Copland terms and JSON](https://ku-sldg.github.io/copland///resources/copland_core.pdf).
*  The main module for the Attestation Server is [ServerMain.hs](https://github.com/ku-sldg/haskell-am/blob/master/copland-interp/app/ServerMain.hs)
*  Type `make helpserver` for a complete description of Attestation Server command line options.

--- -->

<!-- 
### ConnectionServer

The virtual machine that executes Copland instructions relies on an external connection server to manage communication with other appraisers to perform AT operations.  The VM uses a very simple, Unix Domain Socket to transmit requests to the ConnectionServer that must be running on the local machine.  A single ConnectionServer can serve an arbitrary number of Appraiser clients on the same machine.
        *  to start the ConnectionServer, run the shell script, `startCS.sh`.  It requires no arguments.

--- -->

### Datatype/JSON Generator and Translator

There are two primary functions of the Generator/Translator:

1)  Generate random well-formed Haskell ADTs(Abstract Data Types) and JSON objects relevant to our Attestation Manager
2)  Translate to/from ADTs and their JSON representation

It is meant to be useful for testing against implementations outside of the Haskell ecosystem.  It can provide well-formed test inputs, and it can also act as an oracle for JSON parsing.  The Haskell ADT definitions are here:  [CoplandLang.hs](https://github.com/ku-sldg/haskell-am/blob/master/copland-interp/src/CoplandLang.hs).  A formal description of the ADTs and their JSON representations is here:  [Copland terms and JSON](https://ku-sldg.github.io/copland///resources/copland_core.pdf).

* You must provide EXACTLY ONE of the following options:  `-q`, `-p`, `-t`, `-e` that specify which of the following type of thing you'd like to generate/translate:  RequestMessage, ResponseMessage, Copland Term, or Copland Evidence.
*  If `-d` is set, the OUTPUT will be the Haskell ADT representation.  Otherwise the OUTPUT will be the JSON representation.
*  Generator Mode:  `-n N` will generate N random things of the type you specify and output them newline-separated.
*  Translator Mode:  If `-n N` is NOT set (or if N==0), the executable becomes a translator to/from JSON and the ADT representations.  The input representation is always opposite of the output representation(determined by `-d`).  For example: if `-d` is set the input will be JSON and the output will be the ADT (and vice-versa if `-d` is NOT set).
* `-i FILENAME` optional input file (stdIn if omitted)
* `-o FILENAME` optional output file (stdOut if omitted)
*  **NOTE**:  input and output terms/JSON are always newline-separated.
*  The main module for the Generator and Translator is [GenMain.hs](https://github.com/ku-sldg/haskell-am/blob/master/copland-interp/app/GenMain.hs)
*  Type `make helpgen` for a complete description of the Generator/Translator command line options.

## Advanced Configuration
---

Some advanced configuration will be necessary during development (while working within the stack project).  To persist environment variables upon exiting a terminal session, consider adding the export commands to your shell startup configuration (i.e. .profile for MacOS).

### Environment Variables
* `COPLAND_BUILD` :  Point to top level of the haskell-am repository (parent directory of stack project).
    * `export COPLAND_BUILD=<haskell-am_repo>`
    *  Used internally for configuration (i.e. to find the default key file if `COPLAND_KEY` is not defined).
* `COPLAND_KEY`:  specifies a custom key file for signing.
    * `export COPLAND_KEY=<path_to_key>`
    *  This will take priority over the `COPLAND_BUILD` environment variable.


<!--
* `COPLAND_UD_SOCKET`:  specifies a custom file for the Unix Domain socket used to contact ConnectionServer.
    * `export COPLAND_UD_SOCKET=<path_to_key>`
    *  This will take priority over the `COPLAND_BUILD` environment variable.

 *  Used internally for configuration (i.e. to find the default Unix Domain socket  if `COPLAND_UD_SOCKET` is not defined).
-->


### Command-Line Options
* See the help text (`make help`) for complete syntax and quick descriptions of options for all three executables.  To see the help text for individual executables: , `make helpclient`, `make helpserver`, `make helpgen`.
* See the Makefile(in this directory) for commented working examples of command line option combinations.
<!--
* One useful option is -s for "simulation mode".  Simulation mode fakes real measurement and cryptographic operations, and is meant for testing/quick feedback about the protocol control flow (and the structure of resulting evidence).
-->
### Development Flow/Hints
*  The easiest strategy for an interactive development style is via invoking the `make ghci*` family of commands from the repo toplevel.  These invoke the `stack ghci` feature to load a familar Haskell ghci REPL environment within the stack project (that reacts to source modifications).  See the [Haskell Stack documentation](https://docs.haskellstack.org/en/stable/README/) for more details.
*  Because `stack ghci` only allows loading one Main module at a time,
   we need separate commands that respond to changes in the source code
   of each Main module independently: `make ghciclient`(client_app/ClientMain.hs), `make ghciserver`(server_app/ServerMain.hs), `make ghcigen`(gen_app/GenMain.hs).  
*  `make ghci` loads code for the main stack project executable (copland-interp), which effectively loads all shared library source files (ignoring the client/server/gen executable sources).  If you've changed code in multiple Main modules, a safe bet is to simply type `make`(an alias for `stack build`).  However, this will not give you a REPL loop and usually takes quite a bit longer(10-20 seconds) than simply re-loading a ghci session (via `:r`)--so it is best used sparingly (i.e. before deploying everything for an end-to-end test run).
* Useful ghci commands (once in a ghci REPL session):
    * `:r` -- reload a session (re-runs type checker after source file changes).
    * `:q` -- quit a ghci session
    * `:i` `<expression>` -- print out useful info about a Haskell expression (its type, where it is defined, etc.).
    * `:t` `<expression>` -- check the type of a Haskell expression

## Source Files
---

All Haskell source files are within the copland-interp stack project directory:  `copland-interp/`.  Within that directory, the Main modules for executables are in the sub-directories: `client_app`(ClientMain.hs), `server_app`(ServerMain.hs), and `gen_app`(GenMain.hs).  The rest of the common library source files are under the directory `src/ `.

* ClientMain.hs:  Main module for Appraiser Client.
    * Top-level Attestation Manager/Appraiser protocol evaluation (assumes appraiser is at place 0).
* ServerMain.hs:  Main module for Attestation Server.
    * Standalone Attestation Server executable.  Handles requests to interpret Copland phrases.
* GenMain.hs:  Main module for Generator/Translator.
    * Generates random well-formed datatypes and JSON objects that are relevant during attestation.  Also acts as an oracle for bi-directional translation between datatypes and their JSON representation.
* CoplandLang.hs:  Copland language definition-  terms and concrete evidence.
    * Implements language specification from here: [Copland terms and JSON](https://ku-sldg.github.io/copland///resources/copland_core.pdf).
* JsonCopland.hs:  _ToJSON_/_FromJSON_ instances for Copland language and other attestation-relevant datatypes.
    * Implements data exchange format from here:  [Copland terms and JSON](https://ku-sldg.github.io/copland///resources/copland_core.pdf).
    * Implemented via Haskell's Aeson library for JSON parsing.
* DisplayCopland.hs:  Pretty-printing for protocol terms and evidence.
    * Uses:  http://hackage.haskell.org/package/prettyprinter-1.2.1
* Copland.hs:  Interface module that simply exports CoplandLang, JsonCopland, and DisplayCopland modules.
* Interp.hs:  main interpreter (interp function) for Copland terms.
* MonadCop.hs:  Definition of the COP Monad.
    *  COP Monad is an environment for interpreting Copland phrases.
    *  Reader Monad Transformer with IO.
* MonadAM.hs:  Definition of the AM Monad.
    *  AM (Attestation Manager) Monad is a computational context for managing attestation protocols.
    *  Provides primitives for generating nonces, running multiple Copland phrases, collecting results, and performing appraisal.
    *  State Monad Transformer with Reader and IO.
* CryptoImpl.hs:  primitive crypto operations
    * Crypto libraries chosen initially for simplicity, not necessarily for security.
* CommImpl.hs:  communication actions specialized for evaluation of Copland terms.
    * Synchronous message sends/receives.
* CommUtil.hs:  Utility library for communication.
* Comm.hs:  Interface module that simply exports CommImpl and CommUtil modules.
* GenCopland.hs:  Helper functions for random samples of Copland terms and concrete evidence, output to a file or stdout.
* QcCopland.hs:  (QuickCheck Copland).  Helper library to GenCopland.
    * Uses Haskell's QuickCheck library to define _Arbitrary_ instances for terms and concrete evidence.
* ServerProgArgs.hs:  Specifies command-line options for the Attestation Server executable.
    * Uses:  http://hackage.haskell.org/package/optparse-applicative
* ClientProgArgs.hs:  Specifies command-line options for the Appraiser Client executable.
    * Uses:  http://hackage.haskell.org/package/optparse-applicative
* GenProgArgs.hs:  Specifies command-line options for the Generator/Translator executable.
    * Uses:  http://hackage.haskell.org/package/optparse-applicative
* Appraise.hs:  Utility library for appraisal primitives.
* CoplandInstr.hs:  Copland Instruction Set definition .
* MonadVM.hs: Definition of VM monad.
    * Virtual Machine Monad for execution of Copland Instruction Set.
    *  Wraps a State Monad around a COP Monad, that holds a stack necessary for instruction execution.
* ExecCopland.hs: Machinery to execute Copland instructions.
    * Also includes a compiler from Copland term to a sequence of Copland instructions.
* UDcore.hs: Functions to run UnixDomain Socket clients and servers.
* StringConstants.hs:  Constant strings used in pretty-printing and JSON generation.

<!--
* ConnectionServerMain:  Main module for the Connection Server.
    * A single ConnectionServer serves an arbitrary number of Attestion Managers running on the same platform.
    -->

## Examples
---

### `make run`
---

`make run` executes the following command:  `cd copland-interp ; stack exec -- copland-app-exe -w -a`.  This runs the Appraiser Client with the `-w` and `-a` options.  `stack exec` is the Haskell Stack command for running executables managed by one of its projects.  `copland-app-exe` is the name we associated with the the Appraiser Client executable in the stack project cabal file(`copland-interp/copland-interp.cabal`).  `cd copland-interp` is necessary because we must be within the stack project directory(`copland-interp/`) to run `stack exec`.  See the Haskell Stack documentation for more details on `stack exec`.

Note that since we do *not* provide a custom Copland phrase to execute with the `-t FILENAME` option, it instead executes the following hard-coded computation in the AM(Attestation Manager) monad:

```haskell
am_proto_1 :: AM Ev
am_proto_1 = do
    n <- am_genNonce
    resEv <- am_runCOP proto1 n
    b <- appraise_proto_1 resEv
    liftIO $ putStrLn $ "appraisal success: " ++ (show b)
    return resEv
  where proto1 = AT 1
                (LN
                (BRP (ALL,NONE) CPY (USM 1 ["target.txt"]))
                SIG)

```

This describes a typical attestation protocol that first generates a nonce(am_genNonce), then passes that nonce as initial evidence to the execution of a Copland phrase named `proto1`(am_runCOP proto1 n).  `proto1` starts with an AT request to Place 1 to perform some measurement actions.  Upon receiving the request, Place 1 copies the nonce, measures the file "target.txt" (hashes its contents), bundles these two items, and finally signs the bundle before sending it back to the client.  A complete description of the Copland language and its semantics can be found here: [Orchestrating Layered Attestations](https://ku-sldg.github.io/copland///resources/copland-post-2019.pdf).  Upon receiving this evidence, the client performs appraisal and outputs the result.

The `-a` option tells the client to perform appraisal on the resulting evidence.  Currently appraisal is written as an ad-hoc function in Haskell so that it can only appraise evidence produced by the `proto1` protocol above.  The `-w` option is crucial here because it configures and spawns an attestation server thread for Place 1 before sending the initial request.  If we omit the `-w` option we would have to 1) use the `-n FILENAME` option to specify a Place -> Address mapping that includes an entry for Place 1, and 2) start a server at that same Address manually before sending the request.

### `make term`
---

`make term` executes the appraiser client with the `-w` and `-t ../t.hs` options.  The `-t ../t.hs` option executes a custom Copland phrase provided in the file `t.hs` in the top-level directory of the repo(the `../` is necessary because the executable runs in the context of the stack project which is one directory deep).  The user can of course tweak the command from the Makefile to replace `../t.hs` with their own fully qualified path to the input file or additionally provide custom ininitial evidence via the `-e FILENAME` option (`Mt` evidence is the default).  Input terms and evidence must be valid Haskell ADTs of type `T` and `Ev` respectively (both defined in [CoplandLang.hs](https://github.com/ku-sldg/haskell-am/blob/master/copland-interp/src/CoplandLang.hs)).

* Note:  only the term at the TOP LINE of the input file is read as input.
* Using a .hs file extension for the input file and an editor that supports Haskell syntax highlighting is useful if you are constructing Copland terms by hand.


### `make termCompiled`
---

`make termCompiled` acts exactly `make term` described above, but with the addition of the `compile` option.  This option tells the appraiser client to compile the copland phrase provided, and then to execute the sequence of instructions generated.  Should generate the same result as `make term`.  **NOTE**:  a ConnectionServer must be running on your machine before this action is started.

---
