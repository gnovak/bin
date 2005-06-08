%define _prefix /usr/local

Summary: Convert PS to editable vector graphics
Name: pstoedit
Version: 3.40
Release: 1
License: GPL
Group: gsn
URL: http://pstoedit.org
Source0: %{name}-%{version}.tar.gz
Patch0: %{name}-%{version}.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Requires: plotutils

%description
Convert PS to editable vector graphics

%prep
%setup -q
%patch0 -p 1

%build
CXXFLAGS=-Wno-deprecated
%configure
make -k

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall
mkdir -p $RPM_BUILD_ROOT%{_mandir}/man1
cp doc/%{name}.1 $RPM_BUILD_ROOT%{_mandir}/man1

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc

   %{_bindir}/*
   %{_includedir}/pstoedit
   %{_libdir}/libpstoedit.la
   %{_libdir}/libpstoedit.so.0.0.0
   %{_libdir}/pkgconfig/pstoedit.pc
   %{_libdir}/pstoedit
   %{_datadir}/aclocal/pstoedit.m4
   %{_datadir}/pstoedit
   %{_mandir}/man1/*

%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

