Name:		blow
Version:	0.4
Release:	3%{?dist}
License:	GPLv3
URL:		http://spindazzle.org/blow
BuildRoot:	%{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
# The upstream sources are repacked to remove abcl jars (and source).
Source0:	blow-%{version}.tar.gz
Summary:	A Lisp-based web application framework for Java servlet containers
BuildArch:	noarch
Group:		Development
BuildRequires:	jpackage-utils
Requires:	abcl

%description
Blow is a framework for writing web applications in Common Lisp.  It
is designed to run entirely within Java servlet container
infrastructure and features:
    * a 100% Pure Java Common Lisp implementation
    * automatic compilation of Lisp source to Java bytecode
    * a JSP-like template engine for embedding Lisp in HTML content.
    * a simple HTML generation library
    * a convenient Lisp/Java bridge
    * no documentation
    * bugs 

%prep
%setup -q 

%build
sed -i -e 's/# BLOWHOME/BLOWHOME/' blow

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_datadir}/blow
mkdir -p $RPM_BUILD_ROOT%{_bindir}

rm template/app/WEB-INF/lib/abcl.jar
tar cf - template | (cd $RPM_BUILD_ROOT%{_datadir}/blow; tar xf -)
tar cf - lib | (cd $RPM_BUILD_ROOT%{_datadir}/blow; tar xf -)
build-jar-repository $RPM_BUILD_ROOT%{_datadir}/blow/template/app/WEB-INF/lib abcl
install -p -m 555 blow $RPM_BUILD_ROOT%{_bindir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root, -)
%doc ChangeLog
%{_bindir}/blow
%{_datadir}/blow

%changelog
* Mon Dec 14 2009 Anthony Green <green@spindazzle.org> - 0.4-1
- Upgrade.

* Mon Dec 14 2009 Anthony Green <green@spindazzle.org> - 0.3-2
- Fix lib installation.

* Mon Dec 14 2009 Anthony Green <green@spindazzle.org> - 0.3-1
- Upgrade.

* Sat Dec 12 2009 Anthony Green <green@spindazzle.org> - 0.2-1
- Upgrade.

* Thu Jul 04 2008 Anthony Green <green@spindazzle.org> - 0.1-2
- Require abcl package.  Repack source to remove abcl.

* Thu Jul 03 2008 Anthony Green <green@spindazzle.org> - 0.1-1
- Created!
