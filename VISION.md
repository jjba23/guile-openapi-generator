# Vision

`guile-openapi-generator` reads an OpenAPI 3.x document and writes the Guile you would have written: records that validate themselves, a route table with a dispatcher, and a client whose procedures take and return records rather than strings. Its hardest problem is not parsing — there is a YAML reader in Scheme for that — but taste: the generated code has to be short, idiomatic and worth keeping, because it is meant to be checked into the repository next to the application that uses it. This document is about where that principle should go next: from a generator that is good at one direction, into a tool that treats a specification and a Guile codebase as two views of the same thing.

## Where guile-openapi-generator is today

The pipeline is 24 modules and it is legible end to end. `reader.scm` decides what a document is (YAML or JSON, a file, standard input, a URL), refuses Swagger 2 by name, and validates that the `openapi` member is 3.x. `bundle.scm` folds external `$ref`s into `components/`, in `targeted` or `aggressive` mode, with cycles handled by reserving a local name before walking its target. `transforms.scm` holds the opt-in rewrites — `x-extensible-enum` becoming an enumeration only when asked — kept separate from the readers so a transform is testable on a document that is never generated from. Then `spec.scm`, `schema.scm`, `naming.scm`, and the three emitters: `models.scm`, `controllers.scm`, `client.scm`, all of them building s-expressions that `code.scm` lays out.

What comes out is deliberately ordinary. A model is a record with a keyword constructor that validates, a `valid?` that answers instead of raising, `->jso` and `jso->`, a predicate per enum value (`pet-status-sold`) rather than a type, named predicates for named scalars and lists, and an RFC 7386 `apply-patch` because a merge patch is how a partial update is spelled. Absent optional members are left out of the JSON, while a nullable one set to `#f` becomes JSON null — the distinction that makes "leave alone" and "delete" different requests. Controllers come with a route table, per-operation procedures that decode their body by the media type it actually arrived with (including multipart), and `guile-handler`, which adapts Guile's `run-server`; an Artanis adapter registers the same routes if you ask for it. Clients come in plain Guile or libcurl, with a default 30-second timeout, authentication from the security schemes, and an `expect-status` that raises rather than returning a 500 body as if it were the model.

The flags are the interesting part of the design, because each one is a decision the generator refuses to make for you: `--check` reports stale files instead of writing them, so a build can fail rather than silently regenerate; `--self-contained` copies the runtime beside the generated code, for a tree that must depend on nothing; `--controllers none` and `--http none` and `--no-models` let a project take one half; `--request-response-models` with `--exclude-read-only` and `--exclude-write-only` splits a single model set into the two the API actually has; `--fault-tolerant-enums` maps an unknown string to `unrecognized`; `--non-null-response-payloads` fails on a declared success body that is missing. An unknown value for any of them is an error, not a silent fallback — a typo in `--controllers artanis` should not leave you with a plain Guile server.

The second command is the reason this project is more than a code generator. `openapi-generator spec` reads the routes of a server that already exists — a route table this generator wrote, a hand-written `(get "/healthz" handler)`, or an Artanis application's `#:consumes`, `#:produces` and `#:with-auth` — and writes an OpenAPI 3.0.3 document. It reads *source*, as data: the application is never loaded and no handler runs. The compiled pattern is undone rather than approximated, each capture group becoming a placeholder again; a pattern whose captures disagree with its parameter list raises instead of describing some other route; the handler name becomes the `operationId`. The tests read the result back with `parse-spec`, because a specification this project cannot parse would be of no use whatever it looked like.

Generated code is data, not text, and that is the decision that keeps the emitters coherent: every one of them builds the s-expressions it wants with quasiquote and hands them to `(openapi-generator code)`, which owns indentation, line breaking, argument alignment and docstring quoting. A new layout rule lives in one module and every generator benefits; a new emitter adds forms and inherits the layout. The suite is Veritas, the fixtures are real (petstore, polymorphism with discriminators, Kubernetes merge patches, a handful of production specifications), and the coverage report is an lcov tree.

Two constraints are explicit in the documentation, and both are right. It is *not* a multi-language code factory, and it is *not* a runtime framework: it generates ordinary Guile modules, and from there the application is yours. What it does not do is visible in the same place — the reader accepts a 3.x document but the schema layer understands the 3.0 dialect, which is why `--json-schema-file` exists as a separate door for a plain JSON Schema; responses are built by helpers that serialise whatever a handler hands them, with no check against the declared shape; there are no retries, no pagination and no streaming in the client; and the two server adapters are the whole set.

The manual is nine pages, split by task rather than by module — install, quick start, build a complete application, the command line, the reverse direction, what gets generated, the pipeline, the reference — and `examples/todo/` is a runnable application with one specification, a generated tree and the handful of handlers that make it real. Distribution is as wide as any project in this collection: Guix, Nix, a container image, and Debian, openSUSE and Arch packaging built by `maak package-deb`, `package-rpm` and `package-arch`. The version is 0.1.5, which is the honest number: the model layer is mature, the flags are considered, and the surface that 3.1, mocks and streaming will need has not been designed yet.

## The vision

1. **One schema layer covers the whole 3.x surface, 3.1 included.** Type arrays, `null` in a type, `$defs` and `prefixItems` are what modern specifications actually contain, and a second door for JSON Schema stops being necessary once the first one is wide enough.

   What has to be true first: the schema layer learns to represent a union of types without inventing a record for it, and the fixtures grow documents that only 3.1 can express.

2. **The two directions are inverses, and that is a law rather than a hope.** Specification to code to specification returns the document you started with, modulo the four rules the writer fills in; a route table read from source and generated back is the same table. A property test over a corpus, not a handful of examples, is what makes that credible.

   What has to be true first: a corpus of real specifications small enough to vendor and wide enough to hurt, and a canonical form for a document so two equal meanings compare equal.

3. **The set of server targets is open.** A third adapter should be a module that implements the same contract as the two that exist — decode a body by media type, map a handler's return to a response, name an error condition — rather than a fork of the controller emitter. Guile's web stack is not one framework, and the generator should not assume it is.

   What has to be true first: the adapter's responsibilities written down as a protocol, and `controllers.scm` split so that vanilla and Artanis are two implementations of it rather than two branches of one file.

4. **The generated client is one a Guile programmer would ship.** Retries with backoff, pagination conventions, streaming responses and cancellation are the things that make a client usable against a real API, and every one of them is described by the specification or decided by a flag — never guessed.

   What has to be true first: the HTTP layer grows the vocabulary (`--http guile` and `--http curl` must both be able to express a retry), and the flags say what happens when a specification does not describe the thing.

5. **A generated tree is reviewable, and regenerating it changes nothing when nothing changed.** `--check` already refuses a stale tree; the rest is a stable output contract — a version line in the banner, a deterministic order for everything, and a diff a reviewer can read — so that a regeneration commit is either empty or exactly the change the specification made.

   What has to be true first: goldens for each fixture, and a decision about what the banner promises.

## Features that are in scope and not built yet

The code already anticipates these; each is a seam the current design leaves open.

- **OpenAPI 3.1.** The reader validates the version but the schema layer speaks 3.0, and `--json-schema-file` is the workaround. The hard part is a union type that does not become a record: `type: ["string", "null"]` has to map to "a string or nothing" in a language where `#f` already means something.
- **Response validation on the server.** `ok`, `created` and `error-response` serialise whatever the handler returns; the client's `--non-null-response-payloads` is the mirror of a check that does not exist on the serving side. The hard part is that checking a response needs the declared shape at runtime, which is exactly what `--self-contained` exists to make possible.
- **Retries and backoff.** A timeout exists (30 seconds by default) and a retry does not. The hard part is deciding what is safe to retry and telling the caller, rather than retrying a POST because it looked idempotent.
- **Pagination.** The specification describes cursors and offsets in prose, not in structure. The hard part is a flag that names the convention — `--pagination cursor|offset|none` — and a generated procedure that yields pages rather than records.
- **Streaming and server-sent events.** Long-running responses are increasingly the point of an API, and neither generated client nor dispatcher models them. The hard part is that Guile's `(web client)` is a blocking call and streaming wants fibres.
- **A third controller adapter.** The seam exists and has two implementations. The hard part is not writing the adapter but specifying what an adapter owes: authentication, body decoding, error mapping, and what a handler may return.
- **Callbacks and webhooks.** OpenAPI can describe a request the API makes to you, and the generator ignores it. The hard part is generating a *client* into a *server* — the shapes look like the operation models and mean the opposite.
- **A mock server.** The dispatcher, the route table and the models are all there; serving `examples` from a specification would make a front end possible before the back end is written. The hard part is choosing between examples, examples per media type, and generated defaults, honestly, in the documentation.
- **Callbacks as the missing half of the client.** An OpenAPI `callbacks` member describes a request the API makes to you, and generating the handler that receives it is the mirror of generating the client that sends one. The hard part is that a callback has no route table to hang from: it needs a dispatcher of its own, built from the same models.
- **A `--config` that can name a target tree.** Reading options from YAML or JSON exists; naming several output trees — client here, controllers there, models in a shared library — is the configuration a monorepo needs, and the hard part is that a shared model set has to be generated once and referenced, not regenerated per tree.
- **Naming that can be told what to do.** `--operation-id-transform`, `--model-suffix` and a `--module` root exist, which is a start. The hard part is a strategy that survives an acronym, a plural and a reserved word at once — the naming module is the place where "looks hand-written" is won or lost.

## New features that would be worth having

- **`openapi-generator diff OLD NEW`** — what changed between two specifications, in terms of the generated API: a removed operation, a renamed field, a newly required member, a narrowed enum. It is a natural extension of a tool that already reads both directions, and it is the feature that would stop a breaking change from being a surprise at runtime.
- **Generated conformance tests.** The specification says which bodies are required, which enums are closed and which ranges apply. Emitting a Veritas suite that asserts a running server obeys them is a small step from the existing fixtures, and it makes the specification pay for itself twice.
- **A reader for more Guile servers.** `routes.scm` is a model and `guile-routes.scm` and `artanis-routes.scm` are two static readers of source. A reader for a hand-rolled `(web server)` dispatcher, or for a fibres app, is a module — and the reverse direction is what makes this project interesting.
- **`--watch` regeneration.** Change the specification, get the tree; the REPL workflow the project's own documentation advocates deserves a loop that closes on its own.
- **Documentation from the spec.** The `readme` target exists and the generated procedures carry docstrings. Emitting a browsable HTML reference from the same source — the way the project's own manual is built — would give the generated tree a manual.
- **A `guix.scm` next to `dependencies.scm`.** `--dependencies guix` writes a manifest of what the generated code needs. Turning that into a package definition for the application, with `--self-contained` as the opposite choice, would make a generated service installable in one command.
- **A `--check` that names the change.** The flag says a file is stale. Saying *how* — a line diff, or "operation Pet was removed from controllers.scm" — is the difference between a build failure and a review comment.
- **Canonical form for a specification.** `parse-spec` and the writer already meet in the middle; a `openapi-generator canonicalize` that sorts and normalises a document would make diffs of specifications readable, and it is the prerequisite for the round-trip law.
- **An `examples` target.** A fixture specification is a document; a generated example application — handlers that answer with the declared examples, a `maak` task that runs it, a README that says how to call each operation — is the fastest possible way for a reader to believe the generated tree works.
- **A `--stats` report.** How many operations, models, parameters, enums, required members and documented responses a specification has, and which of them the generator could not express. It is the cheapest diagnostic for the question "why is this API not generating well", and it is data the pipeline already computes.
- **Error conditions for the two directions, documented together.** `validation-failure`, `auth-failure` and `unimplemented-operation` exist at runtime; the writer raises its own errors for a pattern that disagrees with its parameter list. One page, one table, one vocabulary would make both halves feel like one program.

## Contributions that would move the needle

Ranked roughly by value per hour.

1. **Goldens for the generated trees.** The fixtures already exist and each one generates a tree deterministically, so the test is a directory and a comparison. It is what turns `--check` from a flag into a contract, and what makes every emitter change reviewable.
2. **The canonical form, then the round-trip property test.** Two weeks of work that pays for itself the first time someone reads a specification this project wrote and finds it wrong.
3. **Retries.** A small, well-understood addition to the client with a flag to turn it off, and the feature most likely to be the reason someone chooses a generated client over a hand-written one.
4. **`--pagination`.** The convention flag plus a procedure that yields pages; the hard part is in the decision, not the code, which makes it a good first design contribution.
5. **Response validation, behind a flag.** `--validate-responses` on the serving side, reusing `valid?` and the constraints that already exist per record.
6. **A third adapter, as the proof of the protocol.** Whoever writes it either finds the adapter contract or writes it down, and both outcomes are what the vision above needs.
7. **`openapi-generator diff`.** Larger, and the feature with the clearest audience: everyone who has shipped a client against an API that moved.
8. **The 3.1 schema layer.** The largest single piece of work here, and the one that decides whether this generator is current in three years' time.
9. **The `examples` target.** Small, and the most persuasive thing a generator can ship: a runnable tree that answers, so a reader can call an operation before writing one.
10. **A documented adapter protocol.** Written from the two implementations that exist, it is what makes a third one an afternoon and what turns "not a framework" from a disclaimer into a design.

## Compared to related projects

| Project | What it is | Where guile-openapi-generator differs |
|---|---|---|
| OpenAPI Generator (the Java one) | A generator with 60-odd language targets, driven by Mustache templates and a large option surface | It has the breadth and the integrations this project deliberately refuses. This one has one target, written by hand, that emits records, keyword arguments and docstrings rather than filling in a template for another language |
| swagger-codegen | The predecessor of the above, still used for Swagger 2 documents | Same shape of tool, an older input dialect. This project refuses Swagger 2 by name rather than half-supporting it |
| Kiota (Microsoft) | A client generator for many languages, built around a request-adapter abstraction and its own description format | Kiota is about generating clients everywhere; this project generates a client *and* the server and the models, in the language the whole program is written in |
| oapi-codegen (Go) | Generates Go types, servers and clients from an OpenAPI document, with a strong type-mapping story | The Go community's answer to the same problem and a good model for how far type mapping can go. This project's difference is the reverse direction: `oapi-codegen` cannot read a Go server and describe it |
| openapi-typescript | Generates TypeScript types from a document, no runtime, no validation | Types only, and structurally they are free. Here a model is a record with a validating constructor, so a wrong shape is an error rather than a type that lies |
| Prism (Stoplight) | A mock server and validator that serves an OpenAPI document, with request and response validation | Prism is what a mock server should be, and this project has none. The dispatcher, route table and models make one cheap; the design question is what a mock answers when the specification gives examples rather than schemas |
| oasdiff, openapi-diff | Compare two OpenAPI documents and report breaking changes | Exactly the analysis `openapi-generator diff` would need, done well and without any generated code. This project could report the same findings in terms of the Guile API they break, which is a narrower and more useful claim |
| schemathesis | Property-based testing generated from an OpenAPI document, against a running server | A different end of the same idea: it tests the server, this project writes it. Generated conformance tests would overlap, and the honest design is to emit Veritas suites rather than to reimplement the strategy engine |
| Guile's `(web server)`, Artanis, `guile-curl`, `guile-json` | The runtime pieces a generated application stands on: a server, a framework, an HTTP client, a JSON reader | Not generators, and not competitors — they are what this project targets. The distinction matters: the generated code depends on them and on nothing else, which is why `--self-contained` is possible at all |

The others do four things this project could still add: Prism's mock server, schemathesis's generated tests, oasdiff's breaking-change report, and the plain breadth of targets that makes OpenAPI Generator the default answer. The first three are small enough to be features here, and each is a natural consequence of already having a document model, a route model and a record-per-schema.

Where this project is ahead and should stay ahead: the output is idiomatic Guile in the sense a Guile programmer means it — `receive`, ports, parameters, `#:keyword` constructors, docstrings that the manual can be generated from — rather than a template in another language; the reverse direction exists at all, and almost nothing else does it; every layout decision lives in one module, so a new emitter cannot format itself differently; the YAML reader is in Scheme, so the dependency tree is the language; and `--check` presumes the generated tree is committed and reviewed, which is the opposite of the usual "generate into a build directory" assumption.

## In the long run

- **The second direction is the moat.** Generators are one-way by convention, and a tool that reads a Guile server and describes it can do things a one-way generator cannot: bootstrap a specification for an existing service, check that the routes still match the document, and answer what a change would break.
- **Everything else generated from the same document.** Code was the first output. Tests, documentation, a mock server and a changelog are all consequences of the same model, and the bet is that the document becomes the source for all of them.
- **The layout module as a shared asset.** `code.scm` is a Scheme pretty-printer for generated forms with opinions about alignment and docstrings. Every other generator in this collection — a scaffolder, a documentation tool, a formatter — wants the same thing, and it would be a fine export.
- **Guile's web stack as the target of record.** If Guile grows a conventional web framework, this project should be its code generator; the adapter protocol above is how that stays possible without guessing which framework wins.
- **Specifications as the collection's contract.** The Artanis services in this account have routes and no documents. A repository of OpenAPI files, generated from their sources and checked in, would make them describable to each other — and this project already has the reader that makes that a task rather than a project.
- **A generator that can explain itself.** Every flag here is a decision, and the documentation argues each one. The long bet is that the reasoning becomes a first-class output: a generated tree that carries the decisions it made, so the next person to read it knows why an optional collection defaulted to empty rather than `#f`.
- **One vocabulary for documents and code.** A specification, a route table, a record and a client are four representations of the same interface. The bet is that this project ends up with one model of an interface and four ways of writing it, rather than a reader and a writer that happen to agree.

## A closing note

The distinguishing decision here is not that it parses OpenAPI; several projects do that better. It is that the generated code is *data* — s-expressions built by emitters and laid out by one module — which is what makes the output uniform, the layout reviewable, and a second target language a matter of writing another `code.scm` rather than another fifty templates. Everything on the list above is easier because of it.

The second decision is the one worth protecting as the project grows: `--check` presumes the generated tree is committed, read and reviewed. That is a statement about who uses this software — people who want a hundred accessors written for them and the rest written by hand — and every feature above either serves that reader or does not belong here.

*This document describes intent, not commitments: it names directions the maintainer finds worth taking, and nothing here is a promise that a given feature will exist, in the order listed, or at all.*
