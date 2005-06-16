Summary: Astronomy latex macros
Name: aastex
Version: 52
Release: 1
License: Restrictive
Group: gsn
URL: http://www.journals.uchicago.edu/AAS/AASTeX/
Source0: http://www.journals.uchicago.edu/AAS/AASTeX/%{name}%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description

%prep
#%setup -q
tar xzf ../SOURCES/%{name}%{version}.tar.gz
cd %{name}%{version}

%build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_datadir}/texmf/tex/latex/aastex
cd %{name}%{version}
make install INSTALLDIR=$RPM_BUILD_ROOT%{_datadir}/texmf/tex/latex/aastex
cp *.dvi *.ps *.tex $RPM_BUILD_ROOT%{_datadir}/texmf/tex/latex/aastex
# Postinstall script: texconfig rehash

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/share/texmf/tex/latex/aastex
%doc


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

