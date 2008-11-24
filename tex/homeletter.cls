\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{homeletter}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{letter}}
\ProcessOptions
\LoadClass{letter}

\signature{Greg Novak}
\address
{
  Greg Novak \\
  311 Lakeview Terrace \\
  Princeton, NJ 08540 \\
  609-865-0318 \\
  greg.novak@gmail.com
%   Greg Novak \\ 
%   3205 California St Apt E \\
%   Berkeley, CA, 94703 \\
%   831-459-0424 \\
%   novak@ucolick.org 
  }
