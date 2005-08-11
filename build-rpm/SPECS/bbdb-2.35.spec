%define _prefix /usr/local
Summary: Emacs address book
Name: bbdb
Version: 2.35
Release: 1
License: LGPL
Group: Text
URL: http://bbdb.sourceforge.net/
Source0: http://bbdb.sourceforge.net/%{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
Requires: vm

%description
Emacs Address Book

%prep
%setup -q

%build
%configure 
make VMDIR=/usr/local/share/emacs/site-lisp Makefile bbdb info vm 

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp
mkdir -p $RPM_BUILD_ROOT%{_infodir}
cp lisp/*.el lisp/*.elc $RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp
cp texinfo/*.info $RPM_BUILD_ROOT%{_infodir}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
   /usr/local/share/emacs/site-lisp/*
   /usr/share/info/bbdb.info.gz
%doc ChangeLog INSTALL README 


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

