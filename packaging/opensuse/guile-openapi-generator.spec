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
Version:        0.1.0
Release:        0
Summary:        OpenAPI 3 to Guile Scheme code generator
License:        MIT
URL:            https://codeberg.org/jjba23/guile-openapi-generator
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  pkgconfig(guile-3.0)
# The installer asks Guile's pkg-config file for the two module directories.
BuildRequires:  pkgconf
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
# The layout — launcher, manual page, modules and their byte-compiled
# copies — comes from the installer every recipe and the container share.
# It asks Guile's pkg-config file for the two directories, so the modules
# land where this distribution's Guile looks for them (%{_libdir} here,
# the multiarch libdir on Debian, /usr/lib on Arch).
sh scripts/install-tree --destdir %{buildroot} --prefix %{_prefix}

# Licences and documentation are not installed here: %license and %doc
# below put them under the directories this distribution expects, which
# are not the ones the installer's own layout would use.

%check
# The libcurl transport test skips itself when guile-curl is not installed;
# everything else runs, which is what makes the check useful without pulling
# an optional transport into the build requirements.
VERITAS_TEST_MODULE=veritas \
VERITAS_TEST_TOP_DIR=test \
VERITAS_TEST_TYPE=unit \
VERITAS_RUN_BENCHMARKS=false \
guile -L ./src -L ./test -c '(exit ((@ (veritas runner) run-tests)))'

%files
%license LICENSE
%doc README.org
%{_bindir}/guile-openapi-generator
%{_mandir}/man1/guile-openapi-generator.1%{?ext_man}
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%dir %{_datadir}/guile/site/3.0
%{_datadir}/guile/site/3.0/openapi-generator
%{_datadir}/guile/site/3.0/openapi-generator.scm
%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/openapi-generator
%{_libdir}/guile/3.0/site-ccache/openapi-generator.go

%changelog
