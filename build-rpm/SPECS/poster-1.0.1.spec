%define _prefix /usr/local
%define _mandir %{_datadir}/man

Summary: Print posters on letter paper
Name: poster
Version: 1.0.1
Release: 1
License: ?
Group: gsn
URL: http://printing.kde.org/downloads/
Source0: ftp://ftp.kde.org/pub/kde/printing/%{name}.tar.bz2
Patch0: %{name}-%{version}.patch
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Print posters on letter paper
%prep
# %setup -q
cp ../SOURCES/%{name}.tar.bz2 .
bunzip2 %{name}.tar.bz2
tar xf %{name}.tar
rm -f %{name}.tar
cd %{name}

%patch0 -p 1

%build
cd %{name}
make

%install
cd %{name}
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_bindir}
mkdir -p $RPM_BUILD_ROOT%{_mandir}/man1
make install PREFIX=$RPM_BUILD_ROOT%{_prefix}

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{_bindir}/*
%{_mandir}/*


%doc %{name}/COPYING %{name}/README


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

