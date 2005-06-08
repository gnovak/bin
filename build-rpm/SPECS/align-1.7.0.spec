%define _prefix /usr/local

Summary: Align text into columns
Name: align
Version: 1.7.0
Release: 1
License: GPL
Group: gsn
URL: http://www.cs.indiana.edu/~kinzler/align/
Source0:http://www.cs.indiana.edu/~kinzler/align/%{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Align text into columns
%prep
%setup -q

%build
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_bindir}
make install BINDIR=$RPM_BUILD_ROOT%{_bindir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/*
%doc COPYING README 

%changelog
* Mon Jun  6 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

