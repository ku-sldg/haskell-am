# Haskell Attestation Manager

## Overview
---

TODO: overview text.  

Please send questions/comments to Adam Petz(ampetz@ku.edu) or submit a GitHub issue.

## Installation
---

### Dependencies
* Haskell Stack:  https://docs.haskellstack.org/en/stable/README/
* Haskell AM source: https://github.com/ku-sldg/haskell-am.git

### QuickStart Guide
1) Install Haskell Stack (via instructions at the link above)
1) Clone the haskell-am source repository (git clone https://github.com/ku-sldg/haskell-am.git)
1) In the top-level of that repository:  type `make` 
    * NOTE:  this may take a while (TODO: time estimate) the first time due to the Haskell dependencies.
1) Type `make run`
1) Successful installation/execution will include output that ends with something like "Evidence Result:" followed by a pretty-printed concrete evidence value (the result of executing a hard-coded protocol).  See the Examples section below for a description of what `make run` does.  

* This will use a default key for signing (located at `keys/key0.txt`) by _**temporarily**_ setting the `COPLAND_BUILD` environment variable.  See the [Advanced Configuration](#advanced-configuration) section for instructions on setting this variable (or the custom `COPLAND_KEY` variable) more permanently for development/deployment purpuses.

## Haskell AM Executables
---

The Haskell AM project is organized as three logically distinct executables:

1)  Copland Interpreter Server (Attestation Server)
1)  Attestation Manager Client (Appraiser Client)
1)  Datatype/JSON Generator and Translator

These executables share common libraries(see Source Files section below TODO:link to Source Files).  Their purpose and usage are described individually in the following sections.

---

### Copland Interpreter Server (Attestation Server)

An Attestation Server handles requests from clients that ask it to interpret a Copland phrase.

*  Note:  The Attestation Server may also act as an Attestation Client.  When it encounters an AT term in a Copland phrase it must itself send a request to the place specified.  
*  The `-r ADDRESS` command line option specifies a specific port where the server should listen for connections.  If ommited, a random available port is selected.
*  In the `RequestMessage` a client includes a mapping from Place to Address where Address is currently a port string.  This tells the server the intended Address of each Place it encounters in the Copland phrase.
*  A full description of the Request/Response Messages handled by the server and their JSON representations is here:  (TODO: link to json schema document).
*  Type `make helpserver` for a complete description of Attestation Server command line options.

---

### Attestation Manager Client (Appraiser Client)

The Attestation Manager Client can generate nonces, sequence exection of multiple Copland phrases, gather evidence results, provide evidence results as initial evidence to other Copland phrase executions, and perform appraisal over evidence.  It is responsible for sending the first request during an attestation protocol run, and also performs the final appraisal.

*  The `-w` option spawns attestation servers as separate background threads before performing the client actions.  This option is useful for testing purposes-  it alleviates the hassle of configuring and starting each Attestation Server as its own executable.  The appropriate number of servers (and at appropriate addresses) are spawned based on the protocol terms involved (and the custom name mapping if provided via the  `-n` option).
*  The `-n FILENAME` option allows specifying the mapping from Place to Address in a file.  The format is newline-separated strings of the form:  \<Place\>:\<Address\>, where \<Place\> is a place identifier(now a number) and \<Address\> is an address string (now just a port number, but may become more sophisticated).  An example file might look something like:
    ```
    0:3000  
    1:3001 
    2:3002  
    ```

    The appraiser client will package and forward this mapping in its requests to the attestation servers so that they share a notion of the Place->Address association.   If the `-n` option is omitted, random available ports are assigned to each Place involved. 

    NOTE:  You may only omit the `-n` option when the `-w` is set.  This is because it only makes sense to randomly generate server ports when the appraiser client is also responsible for spawning the server threads.  When `-w` is NOT set the user is responsible for manually configuring and starting attestation servers at appropriate Addresses according to the name mapping provided via `-n`.

*  The `-v` option specifies that spawned servers should run in simulation mode.  This option only has effect if `-w` is also set.
*  The `-t FILENAME` and `-e FILENAME` command line options are a convenient way to specify execution of a *single* Copland phrase and initial evidence.
*  If the `-t` option is empty or omitted, a hard-coded protocol written as a computation in the AM monad will run instead.
*  Type `make helpclient` for a complete description of Appraiser Client command line options.


---

### Datatype/JSON Generator and Translator

There are two primary functions of the Generator/Translator:

1)  Generate random well-formed datatypes and JSON objects relevant to our Attestation Manager
2)  Translate to/from datatpyes and their JSON representation

It is meant to be useful for testing against implementations outside of the Haskell language.  It can provide well-formed test inputs, and it can also act as an oracle for JSON parsing.

* You must provide EXACTLY ONE of the following options:  `-q`, `-p`, `-t`, `-e` that specify which of the following type of thing you'd like to generate/translate (respectively):  RequestMessage, ResponseMessage, Copland Term, Copland Evidence.  These datatypes and their JSON representations are described here:  (TODO:  link to doc)
*  The `-n N` option will generate N random things of the type you specify and output them newline-separated(output location determined by `-o` option).
*  If the `-d` option is set, the OUTPUT will be the Haskell algebraic datatype representation.  Otherwise the OUTPUT will be the JSON representation.
*  If the `-n N` option is NOT set or if N==0, it becomes a translator to/from JSON and the ADT representations.  The input representation is always the opposite of the output representation(determined by `-d`).  For example: if `-d` is set the input will be JSON and the output will be the ADT representation (and vice-versa if `-d` is NOT set).
* `-i FILENAME` is an optional input file (stdIn if omitted)
* `-o FILENAME` is an optional output file (stdOut if omitted)
*  Note:  input and output terms are always newline separated.
*  Type `make helpgen` for a complete description of the Generator/Translator command line options.

## Advanced Configuration
---

Some advanced configuration will be necessary during development, when working inside the stack project.  To persist environment variables upon exiting a terminal session, consider adding these export commands to ~/.profile (or equivalent for your OS).

### Environment Variables
* `COPLAND_KEY`:  specifies a custom key file for signing.
    * `export COPLAND_KEY=<path_to_key>`
    *  This will take priority over the `COPLAND_BUILD` environment variable.
* `COPLAND_BUILD` :  Should point to the top level of the haskell-am repository (parent directory of the stack project).
    * `export COPLAND_BUILD=<haskell-am_repo_toplevel>`
    *  Used internally for configuration (i.e. to find the default key file if `COPLAND_KEY` is not defined).

### Command-Line Options
* See the help text (`make help`) for complete syntax and quick descriptions of options for all three executables.  To see the help text for individual executables: `make helpserver`, `make helpclient`, `make helpgen`.
* See the Makefile(in this directory) for commented working examples of command line option combinations.
* One useful option is -s for "simulation mode".  Simulation mode fakes real measurement and cryptographic operations, and is meant for testing/quick feedback about the protocol control flow (and the structure of resulting evidence).
### Development Flow/Hints
*  After permantently setting one of the preceding environment variables, the easiest strategy for an interactive development style is via the `make ghci` command.  It uses the `stack ghci` feature to load a familar ghci environment within the stack project (that reacts to source modifications).  See the [Haskell Stack documentation](https://docs.haskellstack.org/en/stable/README/) for more details. 
*  Because `stack ghci` only allows loading one Main module at a time, we need three separate commands that respond to source changes in each of the Main modules: `make ghciserv`(ServerMain.hs), `make ghciapp`(AppMain.hs), `make ghcigen`(GenMain.hs).  `make ghci` is the same as `make ghciserv` and should respond to all source changes besides to the other two Main modules.  If you've changed code in multiple Main modules, a safe bet is to simply type `make`(an alias for `stack build`).  However, this will not give you a REPL loop and usually takes quite a bit longer(10-20 seconds) than re-loading a ghci session--so I tend to use it sparingly (i.e. before I deploy the whole thing for an end-to-end test run).

## Source Files
---

All Haskell source files are within the stack project directory:  `copland-interp/`.  The Main modules for the executables are in `copland-interp/app/`, the rest of the common library source files are in `copland-interp/src/ `.

* ServerMain.hs:  Main module for Attestation Server.
    * Standalone Attestation Server executable.  Handles requests to interpret Copland phrases.
* ClientMain.hs:  Main module for Appraiser Client.
    * Top-level Attestation Manager/Appraiser protocol evaluation (assumes appraiser is at place 0).
* GenMain.hs:  Main module for Generator/Translator.
    * Generates random well-formed datatypes and JSON objects that are relevant during attestation.  Also acts as an oracle for bi-directional translation between datatypes and their JSON representation.
* CoplandLang.hs:  Copland language definition-  terms and concrete evidence.
    * Implements language specification here (TODO: update link): [Copland Language spec](https://github.com/ku-sldg/CAPTools/blob/master/doc/copland/copland_core.pdf).
* JsonCopland.hs:  _ToJSON_/_FromJSON_ instances for Copland language and other attestation-relevant datatypes.
    * Implements data exchange format here (TODO: update link): [Copland JSON spec](https://github.com/ku-sldg/CAPTools/blob/master/doc/copland/copland_core.pdf).
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
* StringConstants.hs:  Constant strings used in pretty-printing and JSON generation.

## Examples
---

### `make run`

`make run` executes the Appraiser Client with the `-w` and `-a` options.  Since we do *not* provide a custom Copland phrase to execute with the `-t FILENAME` option, it instead executes the following hard-coded computation in the AM(Attestation Manager) monad:

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

This describes a simple attestation protocol that first generates a nonce(am_genNonce), then passes that nonce as initial evidence to the execution of a Copland phrase named `proto1`(am_runCOP proto1 n).  `proto1` starts with an AT request to Place 1 to perform the remaining measurement actions.  Upon receiving the request, Place 1 copies the nonce, measures the file "target.txt" (hashes its contents), bundles these two items, and finally signs the bundle before sending it back to the client.  A complete description of the Copland language and its semantics can be found here: (TODO: link to orchestrating layered attestations paper).  Upon receiving this evidence, the client performs appraisal and outputs the result.
  
The `-a` option tells the client to perform appraisal on the resulting evidence.  Currently appraisal is written as an ad-hoc function in Haskell so that it can only appraise evidence produced by the `proto1` protocol above.  The `-w` option is crucial here because it configures and spawns the attestation server thread for Place 1 before sending the initial request.  If we omit the `-w` option we would have to 1) use the `-n FILENAME` option to specify a Place -> Address mapping that includes an entry for Place 1, and 2) start a server at that same Address manually before sending the request.

### `make term`

`make term` executes the appraiser client with the `-w` and `-t ../t.hs` options.  The `-t` option allows the user to execute a custom Copland phrase given in the input file ../t.hs.  Note:  only the term at the TOP of the file is read as input.  As specified, the t.hs file must appear in the top-level directory of the repo(the ../ is necessary because the executable runs in the context of the stack project which is one directory deep).  The user can of course tweak the command from the Makefile and provide their own fully qualified path to the input file.  Using a .hs file extension for the input file and an editor that supports Haskell syntax highlighting is useful if you are constructing Copland terms by hand.  Terms must be valid Haskell ADTs of type `T`, defined in CoplandLang.hs (TODO: put link).

---

