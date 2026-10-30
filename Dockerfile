# syntax=docker/dockerfile:1.7

# guile-openapi-generator -- OpenAPI 3 to Guile Scheme code generator
#
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright © 2026 Josep Bigorra <jjbigorra@gmail.com>
#
# The image installs the same tree the distribution recipes install, and it
# gets it from the same place: scripts/install-tree, which asks Guile where the
# modules and their byte-compiled copies belong instead of assuming a directory
# that differs between architectures and between a distribution's Guile and one
# from a profile.  Arch, Debian and openSUSE run that script too, so the image
# cannot drift from the packages.

# ==============================================================================
# Build stage
# ==============================================================================

FROM opensuse/tumbleweed:latest AS builder

ARG VERSION=dev

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

WORKDIR /src

# ------------------------------------------------------------------------------
# Build dependencies
# ------------------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/zypp \
    set -eu; \
    echo "==> Refreshing openSUSE repositories"; \
    zypper --non-interactive refresh; \
    echo "==> Installing build dependencies"; \
    zypper --non-interactive install --no-recommends \
        guile \
        guile-json; \
    echo "==> Cleaning package metadata"; \
    zypper clean --all

# ------------------------------------------------------------------------------
# Source
# ------------------------------------------------------------------------------

COPY src/ ./src/
COPY scripts/ ./scripts/
COPY man/ ./man/
COPY COPYING README.org ./

# ------------------------------------------------------------------------------
# Install into a staging root
#
# The staging root is copied into the runtime stage whole, so what the image
# holds is what this step proved to work.  The layout itself is not described
# here: scripts/install-tree installs it, byte-compiles every module and fails
# when a compiled copy is missing, exactly as it does for the distribution
# recipes.  Only the documentation is added on top, because %license/%doc,
# dh_installdocs and this line are where each package decides about it.
# ------------------------------------------------------------------------------

RUN set -eu; \
    echo "==> Staging the installation"; \
    root=/tmp/root; \
    rm -rf "$root"; \
    sh scripts/install-tree --destdir "$root"; \
    install -Dm644 COPYING "$root/usr/share/doc/guile-openapi-generator/COPYING"; \
    install -Dm644 README.org "$root/usr/share/doc/guile-openapi-generator/README.org"; \
    echo "==> Staged files"; \
    ls -R "$root"

# ------------------------------------------------------------------------------
# Smoke test
#
# The staged tree runs before it is copied anywhere: the launcher finds the
# library from its own prefix, the modules load from the site directory, and
# both commands produce what they promise.  The specification is the smallest one
# that exercises models, controllers and client: one operation, one schema.
# ------------------------------------------------------------------------------

RUN set -eu; \
    echo "==> Smoke testing the staged installation"; \
    /tmp/root/usr/bin/guile-openapi-generator --version; \
    echo "==> Checking the version the image claims"; \
    if [ "${VERSION}" != dev ]; then \
        /tmp/root/usr/bin/guile-openapi-generator --version \
            | grep -qx "guile-openapi-generator ${VERSION}"; \
    fi; \
    work=$(mktemp -d); \
    printf '%s\n' \
        'openapi: 3.0.3' \
        'info: {title: Smoke, version: "1.0.0"}' \
        'paths:' \
        '  /ping:' \
        '    get:' \
        '      operationId: ping' \
        '      responses:' \
        '        "200":' \
        '          description: pong' \
        '          content:' \
        '            application/json:' \
        '              schema: {$ref: "#/components/schemas/Pong"}' \
        'components:' \
        '  schemas:' \
        '    Pong:' \
        '      type: object' \
        '      required: [message]' \
        '      properties:' \
        '        message: {type: string}' \
        >"$work/api.yaml"; \
    /tmp/root/usr/bin/guile-openapi-generator generate \
        -i "$work/api.yaml" -o "$work/gen" -m smoke; \
    test -f "$work/gen/smoke/models.scm"; \
    test -f "$work/gen/smoke/controllers.scm"; \
    printf '%s\n' '(get "/ping" ping-handler)' >"$work/app.scm"; \
    /tmp/root/usr/bin/guile-openapi-generator spec \
        -i "$work/app.scm" -o "$work/openapi.yaml"; \
    grep -q '/ping' "$work/openapi.yaml"; \
    rm -rf "$work"; \
    echo "==> The staged installation works in both directions"

# ==============================================================================
# Runtime stage
# ==============================================================================

FROM opensuse/tumbleweed:latest AS runtime

ARG VERSION=dev

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

LABEL org.opencontainers.image.title="guile-openapi-generator" \
      org.opencontainers.image.description="OpenAPI 3 to Guile Scheme code generator" \
      org.opencontainers.image.url="https://codeberg.org/jjba23/guile-openapi-generator" \
      org.opencontainers.image.source="https://codeberg.org/jjba23/guile-openapi-generator" \
      org.opencontainers.image.licenses="AGPL-3.0-or-later" \
      org.opencontainers.image.version="${VERSION}"

# ------------------------------------------------------------------------------
# Runtime dependencies
# ------------------------------------------------------------------------------

RUN --mount=type=cache,target=/var/cache/zypp \
    set -eu; \
    echo "==> Refreshing openSUSE repositories"; \
    zypper --non-interactive refresh; \
    echo "==> Installing runtime dependencies"; \
    zypper --non-interactive install --no-recommends \
        guile \
        guile-json \
        ca-certificates; \
    echo "==> Cleaning package metadata"; \
    zypper clean --all

# ------------------------------------------------------------------------------
# Application
# ------------------------------------------------------------------------------

COPY --from=builder /tmp/root/ /

# The command reads a specification from the working directory and writes the
# tree beside it, so the image starts in a directory meant to be mounted over.
WORKDIR /work

# ------------------------------------------------------------------------------
# Runtime diagnostics
#
# These run on the finished image: the command and the manual page are where
# PATH and man(1) expect them, the library loads without any source tree, both
# commands answer, and every module has a byte-compiled copy beside the sources,
# so nothing is compiled the first time a module is used.
# ------------------------------------------------------------------------------

RUN set -eu; \
    echo "==> Installation"; \
    command -v guile-openapi-generator; \
    ls -l /usr/share/man/man1/guile-openapi-generator.1; \
    echo "==> Version"; \
    guile-openapi-generator --version; \
    echo "==> The library loads from the site directory"; \
    guile -c '(use-modules (openapi-generator)) (use-modules (openapi-generator yaml-writer))'; \
    echo "==> The compiled copies are the ones Guile loads"; \
    GUILE_AUTO_COMPILE=0 guile \
        -c '(use-modules (openapi-generator) (openapi-generator cli) (openapi-generator yaml-writer) (openapi-generator routes) (openapi-generator artanis-routes))'; \
    echo "==> Both commands answer"; \
    guile-openapi-generator generate --help | head -n 1; \
    guile-openapi-generator spec --help | head -n 1; \
    echo "==> Byte-compiled modules"; \
    site_dir=$(guile -c '(display (%site-dir))'); \
    ccache_dir=$(guile -c '(display (%site-ccache-dir))'); \
    sources=$(ls "$site_dir/openapi-generator/" | wc -l); \
    compiled=$(ls "$ccache_dir/openapi-generator/" | wc -l); \
    echo "    $compiled of $sources modules byte-compiled in $ccache_dir"; \
    test "$compiled" -eq "$sources"

ENTRYPOINT ["/usr/bin/guile-openapi-generator"]
CMD ["--help"]
