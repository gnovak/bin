%define _prefix /usr/local
%define _infodir %{_datadir}/info

Summary: Planner Mode for Emacs
Name: planner-mode
Version: dev
Release: 1
License: GPL
Group: gsn
URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php
Source0: http://sacha.free.net.ph/notebook/emacs/sacha-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Planner Mode for Emacs
%prep
# %setup -q
tar xzf ../SOURCES/sacha-%{version}.tar.gz
cd sacha-dev

%build
cd sacha-%{version}/emacs-wiki
makeinfo emacs-wiki.texi

%install
rm -rf $RPM_BUILD_ROOT
cd sacha-%{version}
mkdir -p $RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp
mkdir -p $RPM_BUILD_ROOT%{_infodir}

cp planner/*.el emacs-wiki/*.el remember/*.el \
	$RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp

cp planner/*.info remember/*.info emacs-wiki/*.info \
	$RPM_BUILD_ROOT%{_infodir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_datadir}/emacs/site-lisp/*
%{_infodir}/*

%doc


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

