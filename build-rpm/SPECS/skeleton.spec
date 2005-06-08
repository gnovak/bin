Summary: 
Name: skeleton
Version: 
Release: 1
License: 
Group: 
URL: 
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description

%prep
%setup -q

%build

%install
rm -rf $RPM_BUILD_ROOT

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

