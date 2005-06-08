%define _prefix /usr/local
%define _libdir %{_prefix}/lib
%define _mandir %{_prefix}/man
%define _infodir %{_prefix}/info

Summary: Plotutils
Name: plotutils
Version: 2.4.1
Release: 1
License: GPL
Group: gsn
URL: http://www.gnu.org/software/plotutils/
Source0: ftp://mirrors.usc.edu/pub/gnu/plotutils/%{name}-%{version}.tar.gz
Patch0:%{name}-%{version}.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Gnu plotutils

%prep
%setup -q
%patch -p 1

%build
CXXFLAGS=-Wno-deprecated
%configure --enable-libplotter --enable-libxmi

make -k

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc

%{_bindir}/*
%{_includedir}/*
%{_libdir}/*
%{_infodir}/*
%{_mandir}/man1/*
%{_datadir}/libplot
%{_datadir}/ode
%{_datadir}/pic2plot
%{_datadir}/tek2plot

%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

