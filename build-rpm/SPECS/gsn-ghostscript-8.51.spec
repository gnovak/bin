%define _prefix /usr/local
%define actual_name ghostscript
# %define _mandir %{_prefix}/share/man
# %define _infodir %{_prefix}/share/info

Summary: A PostScript(TM) interpreter and renderer.
Name: gsn-ghostscript
Version: 8.51
Release: 1
License: non-GPL
Group: Applications/Publishing
URL: http://www.ghostscript.com/doc/gnu/
Source0: ftp://mirror.cs.wisc.edu/pub/mirrors/ghost/gnu/gs705/%{name}-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
Ghostscript is a set of software that provides a PostScript(TM)
interpreter, a set of C procedures (the Ghostscript library, which
implements the graphics capabilities in the PostScript language) and
an interpreter for Portable Document Format (PDF) files. Ghostscript
translates PostScript code into many common, bitmapped formats, like
those understood by your printer or screen. Ghostscript is normally
used to display PostScript files and to print PostScript files to
non-PostScript printers.

If you need to display PostScript files or print them to
non-PostScript printers, you should install ghostscript. If you
install ghostscript, you also need to install the ghostscript-fonts
package.

%prep
cp ../SOURCES/%{name}-%{version}.tar.bz2 .
bunzip2 %{name}-%{version}.tar.bz2
tar xf %{name}-%{version}.tar
rm -f %{name}-%{version}.tar
cd %{actual_name}-%{version}

# %setup -q

%build
cd %{actual_name}-%{version}
./configure 
#%configure 
#make so prefix=%{_prefix}
make prefix=%{_prefix}

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT
cd %{actual_name}-%{version}
make install prefix=$RPM_BUILD_ROOT%{_prefix} \
    exec_prefix=$RPM_BUILD_ROOT%{_prefix} \
    bindir=$RPM_BUILD_ROOT%{_bindir} \
    libdir=$RPM_BUILD_ROOT%{_libdir} \
    mandir=$RPM_BUILD_ROOT%{_mandir} \
    datadir=$RPM_BUILD_ROOT%{_datadir}


#%makeinstall 
# make install soinstall prefix=$RPM_BUILD_ROOT%{_prefix}

# Clean out german man pages
rm -rf $RPM_BUILD_ROOT%{_mandir}/de

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/*
%{_datadir}/%{actual_name}
%{_mandir}/man1/*

%doc


%changelog
* Tue Jun  7 2005 Greg Novak <novak@dionysus.ucolick.org> - 
- Initial build.

