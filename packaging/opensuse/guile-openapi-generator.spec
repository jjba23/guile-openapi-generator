#
# spec file for package guile-openapi-generator
#
# Copyright (c) 2026 SUSE LLC and contributors
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

# Scheme sources plus a bash launcher: there is no compiled binary to
# debug, so the empty -debugsource package is skipped.
%global debug_package %{nil}

Name:           guile-openapi-generator
Version:        0.0.7
Release:        0
Summary:        OpenAPI 3 to Guile Scheme code generator
License:        AGPL-3.0-or-later
URL:            https://codeberg.org/jjba23/guile-openapi-generator
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  pkgconfig(guile-3.0)
BuildRequires:  guile-json
BuildRequires:  veritas
Requires:       guile
Requires:       guile-json
Recommends:     guile-curl

# veritas is produced by the same packaging batch; build it first.

%description
guile-openapi-generator reads OpenAPI 3.x specifications written in YAML or
JSON and generates Guile Scheme code: validated record data models,
controllers (plain Guile or Artanis) and HTTP clients (plain Guile or
libcurl), including polymorphism, enumerations, JSON merge patch and the
security schemes in the specification.

%prep
%autosetup -n %{name}-%{version}

%build
# Nothing to build: the program is Scheme sources plus a launcher, and
# the sources are byte-compiled during %install so that they land in
# the interpreter's compiled-module directory.

%install
# Both directories are asked of Guile's pkg-config file rather than
# assumed, so the modules land where this distribution's Guile looks
# for them.
SITE_DIR="%{buildroot}$(pkg-config --variable=sitedir guile-3.0)"
CCACHE_DIR="%{buildroot}$(pkg-config --variable=siteccachedir guile-3.0)"

mkdir -p "$SITE_DIR/openapi-generator" "$CCACHE_DIR/openapi-generator"
cp -r src/openapi-generator/. "$SITE_DIR/openapi-generator/"
cp src/openapi-generator.scm "$SITE_DIR/"

# Byte-compile every module so that loading it does not compile it
# again on first use.
find "$SITE_DIR/openapi-generator" -name '*.scm' | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

%check
VERITAS_TEST_TYPE=unit VERITAS_RUN_BENCHMARKS=false guile -L ./src -L ./test -c '((@ (veritas runner) run-tests))'

%files
%license COPYING
%doc README.org
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%dir %{_datadir}/guile/site/3.0
%{_datadir}/guile/site/3.0/openapi-generator
%{_datadir}/guile/site/3.0/openapi-generator.scm
%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/openapi-generator

%changelog
