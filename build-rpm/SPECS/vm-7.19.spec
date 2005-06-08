%define _prefix /usr/local
%define _infodir %{_datadir}/info

Summary: VM views mail in Emacs
Name: vm
Version: 7.19
Release: 1
License: GPL
Group: Emacs
URL: http://www.wonderworks.com/vm/download.html
Source0: ftp://tug.org/vm/%{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Views mail in emacs

%prep
%setup -q

%build
# make all INFODIR=%{_infodir} LISPDIR=%{_datadir}/emacs/site-lisp PIXMAPDIR=%{_datadir}/emacs/etc/vm BINARYDIR=%{_bindir}
make all EMACS=emacs22

%install
rm -rf $RPM_BUILD_ROOT
make install INFODIR=$RPM_BUILD_ROOT%{_infodir} \
	LISPDIR=$RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp \
	PIXMAPDIR=$RPM_BUILD_ROOT%{_datadir}/emacs/etc/vm \
	BINDIR=$RPM_BUILD_ROOT%{_bindir} \
	EMACS=emacs22
%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc COPYING README 
   %{_bindir}/*
   %{_datadir}/emacs/etc/vm
   %{_datadir}/emacs/site-lisp/*
   %{_infodir}/*

%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

