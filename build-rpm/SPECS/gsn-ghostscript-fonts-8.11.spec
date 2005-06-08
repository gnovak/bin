%define _prefix /usr/local
%define actual_name ghostscript-fonts

Summary: Fonts for the Ghostscript PostScript(TM) interpreter.
Name: gsn-ghostscript-fonts
Version: 8.11
Release: 1
License: GPL
Group: Applications/Publishing
BuildRoot: /var/tmp/gsfonts-root
URL: http://gnu-gs.sourceforge.net/
Source: ftp://ftp.cs.wisc.edu/pub/ghost/gnu/gs550/ghostscript-fonts-std-%{version}.tar.gz
Requires: ghostscript
BuildArchitectures: noarch

%description
Ghostscript-fonts contains a set of fonts that Ghostscript, a
PostScript interpreter, uses to render text. These fonts are in
addition to the fonts shared by Ghostscript and the X Window System.

You'll need to install ghostscript-fonts if you're installing
ghostscript.

%prep
%setup -q -c %{actual-name}-%{version}

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/share/fonts/default/ghostscript
cp fonts/* $RPM_BUILD_ROOT/usr/share/fonts/default/ghostscript

%post -p /usr/bin/fc-cache

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
/usr/share/fonts/default

%changelog
* Fri Sep  3 2004 Tim Waugh <twaugh@redhat.com> 5.50-13
- Own /usr/share/fonts/default (bug #131650).

* Tue Jun 15 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Fri Feb 13 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Thu Jan 22 2004 Tim Waugh <twaugh@redhat.com> 5.50-10
- Rebuilt. (The 8.11 package which briefly appeared in rawhide should
  not be used.)  Bug #99323, bug #113866.

* Wed Jan 22 2003 Tim Powers <timp@redhat.com>
- rebuilt

* Wed Nov 20 2002 Tim Powers <timp@redhat.com>
- rebuild in latest collinst

* Mon Sep  2 2002 Owen Taylor <otaylor@redhat.com>
- Run /usr/bin/fc-cache in the %%post

* Fri Jun 21 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Thu May 23 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Wed Jan 09 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Thu Jul 13 2000 Prospector <bugzilla@redhat.com>
- automatic rebuild

* Sun Jun 18 2000 Matt Wilson <msw@redhat.com>
- rebuild for next release

* Mon Feb 14 2000 Bernhard Rosenkraenzer <bero@redhat.com>
- 5.50

* Sun Mar 21 1999 Cristian Gafton <gafton@redhat.com> 
- auto rebuild in the new build environment (release 3)

* Wed Feb 24 1999 Preston Brown <pbrown@redhat.com>
- Injected new description and group.

* Wed Jan 13 1999 Preston Brown <pbrown@redhat.com>
- renamed package to be consistent with new ghostscript.

* Fri Nov 13 1998 Preston Brown <pbrown@redhat.com>
- removed the std fonts...now shared between X11 and gs with URW fonts pkg.

* Thu Jul  2 1998 Jeff Johnson <jbj@redhat.com>
- update to 4.03.

* Mon May 04 1998 Erik Troan <ewt@redhat.com>
- set the owner and group of all of the files to 0.0

* Tue Sep 23 1997 Erik Troan <ewt@redhat.com>
- made a noarch package
