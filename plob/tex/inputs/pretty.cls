% REPORT STANDARD DOCUMENT STYLE -- Released 14 January 1992
%    for LaTeX version 2.09
% Copyright (C) 1992 by Leslie Lamport

% kirschke@informatik.uni-hamburg.de (HK) 14-Sep-93:
%   Derived this style from german report.sty
%   The style of the page headers is changed. The page header is
%   now printed in bold face and divided from the text by a \hrule.

% HK 14-Sep-93:
%\typeout{Standard Document Style `report' <14 Jan 92>.}
%\typeout{Document Style `pretty' <14 Sep 93>, derived from german report.sty}

% HK 1994/11/09
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pretty}
              [1994/11/09 v1.0
 Modified document class of `report']

\DeclareOption{nothink}{\PassOptionsToPackage{\CurrentOption}{think}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\newif\if@wide\@widefalse
\DeclareOption{narrow}{\@widefalse}
\DeclareOption{wide}{\@widetrue}

\ProcessOptions

\LoadClass{report}

\AtBeginDocument{%
\PassOptionsToPackage{nothink}{think}%
\RequirePackage{ifthen}
\RequirePackage{think}%
\RequirePackage{versionno}%
}
%
\if@wide
%
% {20mm} ergibt die fuer LaTeX 2.09 benutzte \textwidth:
\addtolength\textwidth      {20mm}%
%%old:
%%\addtolength\oddsidemargin  {-10mm}%
\addtolength\oddsidemargin  {-5.926pt}%
%
%%\addtolength\evensidemargin {-20mm}%
\addtolength\evensidemargin {-20mm}%
%
%\addtolength\topmargin      {-20mm}%
%
\else\if@twoside\relax\else
%
\addtolength\oddsidemargin  {-10mm}%
\addtolength\marginparwidth {15mm}%
%
\fi\fi%
%\addtolength\textheight     {20mm}%
% {16.10072mm} ergibt die fuer LaTeX 2.09 benutzte \textheight:
\addtolength\textheight     {16.10072mm}%

\newcommand\marginlabel[1]% See LaTeX companion, p. 74
{\mbox{}\marginpar[\raggedleft\hspace{0pt}\small\em#1]%
{\raggedright\hspace{0pt}\small\em#1}}

\if@twoside%

\def\marginnumber#1{%
\marginlabel{%
\ifthenelse{\isodd{\value{page}}}%
{{\em#1}\hspace*{\fill}}%
{\hspace*{\fill}{\em#1}}}}

\else%

\def\marginnumber#1{%
\marginlabel{{\em#1}\hspace*{\fill}}%
}%

\fi%\if@twoside


\def\@StoreRep#1#2#3#4{%
  \ifx\@writetorep\undefined\else%
    \@writetorep{[#1]}{#2.#3}{#4}%
  \fi%
  \ifx\hyperdef\undefined\relax\else%
    \hyperdef{#2}{#3}{\null}%
  \fi%
}%\@StoreRep

\renewcommand\tableofcontents{%
    \if@twocolumn%
      \@restonecoltrue\onecolumn%
    \else%
      \@restonecolfalse%
    \fi%
    \let\@old@makeschapterhead\@makeschapterhead%
    \def\@makeschapterhead{%
      \@StoreRep{\contentsname}{chapter}{Contents}{0}%
      \@old@makeschapterhead}%
    \chapter*{\contentsname
    % HK 1994/11/09
    % \@mkboth{\uppercase{\contentsname}}{\uppercase{\contentsname}}%
    \@mkboth{\contentsname}{\contentsname}%
        }%
    \let\@makeschapterhead\@old@makeschapterhead%
    \@starttoc{toc}
    \if@restonecol\twocolumn\fi
    }

\renewcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \let\@old@makeschapterhead\@makeschapterhead%
    \def\@makeschapterhead{%
      \@StoreRep{\listfigurename}{chapter}{Figures}{0}%
      \@old@makeschapterhead}%
    \chapter*{\listfigurename
      \@mkboth%
      % HK 1994/11/09
      %{\uppercase{\listfigurename}}%
      %{\uppercase{\listfigurename}}
      {\listfigurename}{\listfigurename}%
      }%
    \let\@makeschapterhead\@old@makeschapterhead%
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }

\renewcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \let\@old@makeschapterhead\@makeschapterhead%
    \def\@makeschapterhead{%
      \@StoreRep{\listtablename}{chapter}{Tables}{0}%
      \@old@makeschapterhead}%
    \chapter*{\listtablename
      \@mkboth%
      % HK 1994/11/09
      %{\uppercase{\listtablename}}
      %{\uppercase{\listtablename}}%
      {\listtablename}{\listtablename}
      }%
    \let\@makeschapterhead\@old@makeschapterhead%
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }

\ifx\newblock\undefined%
\def\newblock{\hskip .11em \@plus.33em \@minus.07em}%
\fi%

\renewenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth%
                % HK 1994/11/09
                %{\uppercase{\indexname}}%
                %{\uppercase{\indexname}}%
                {\indexname}{\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}

% HK 1994/11/09
\newcommand\@headrule{%
\@incvers%
\\[-2pt]%
\ifdothink%
% Mit Versions-Angabe:
{\rm\fontseries{m}\fontshape{n}\fontsize{9}{11pt}\selectfont
Version \ifx\version\undefined\@hvers.\@lvers\else\version\fi\ / %
\@date}\vspace*{-\baselineskip}\vspace*{2pt}\\%
\fi%
\rule{\textwidth}{1.2pt}\vspace*{-\baselineskip}\vspace*{2pt}\\%
\rule{\textwidth}{0.3pt}%
}%

\newcommand\@hkcmarkl[2]{%
\protect\hspace{-\textwidth}%
\protect\parbox[t]{\textwidth}{\protect\raggedleft{%
%\rm\bf{}%
\usefont{T1}{\rmdefault}{b}{n}%
\ifx#1\@empty\else%
\ifnum\c@secnumdepth>\m@ne\@chapapp\ \thechapter.\ \fi \fi#2%
}\protect\@headrule}}%

\newcommand\@hkcmarkr[2]{%
\protect\parbox[t]{\textwidth}{{%
%\rm\bf{}%
\usefont{T1}{\rmdefault}{b}{n}%
\ifx#1\@empty\else%
\ifnum\c@secnumdepth>\m@ne\@chapapp\ \thechapter.\ \fi \fi#2%
}\protect\@headrule}%
\protect\hspace*{-\textwidth}}%

\newcommand\@hksmark[1]{%
\protect\parbox[t]{\textwidth}{{%
%\rm\bf{}%
\usefont{T1}{\rmdefault}{b}{n}%
\ifnum\c@secnumdepth>\z@\thesection. \ \fi #1%
}\protect\@headrule}%
\protect\hspace*{-\textwidth}}%

\newcommand\@hkmkboth[2]{%
\markboth{\@hkcmarkl{}{#1}}{\@hkcmarkr{}{#2}}
}%


\if@twoside
    \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      % HK 1994/11/09
      %\def\@evenhead{\thepage\hfil\slshape\leftmark}%
      %\def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      %\let\@mkboth\markboth
      \def\@evenhead{\thepage\hfil\leftmark}%
      \def\@oddhead{\rightmark\hfil\thepage}%
      \let\@mkboth\@hkmkboth%
      %
      % HK 1994/11/09
      %\def\chaptermark##1{%
        %\markboth {\uppercase{\ifnum \c@secnumdepth >\m@ne
            %\@chapapp\ \thechapter. \ \fi
            %##1}}{}}%
      %\def\sectionmark##1{%
        %\markright {\uppercase{\ifnum \c@secnumdepth >\z@
            %\thesection. \ \fi
            %##1}}}%
      \def\chaptermark##1{\markboth{\@hkcmarkl{t}{##1}}{}}%
      \def\sectionmark##1{\markright{\@hksmark{##1}}}%
      }
\else
    \def\ps@headings{%
      \let\@oddfoot\@empty
      % HK 1994/11/09
      %\def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      %\let\@mkboth\markboth
      %\def\chaptermark##1{%
        %\markright {\uppercase{\ifnum \c@secnumdepth >\m@ne
            %\@chapapp\ \thechapter. \ \fi
            %##1}}}%
      \def\@oddhead{\rightmark\hfil\thepage}% Heading.
      \let\@mkboth\@hkmkboth%
      \def\chaptermark##1{\markright{\@hkcmarkr{t}{##1}}}%
      }
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    % HK 1994/11/09
    %\def\@evenhead{\thepage\hfil\slshape\leftmark}%
    %\def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \def\@evenhead{\thepage\hfil\leftmark}%
    \def\@oddhead{\rightmark\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }

\if@titlepage
  \renewcommand\maketitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  % HK 1994/11/15
  \parindent\z@%
  \null\vfil
  % HK 1994/11/09
  %\vskip 60\p@
  \@StoreRep{Title}{chapter}{Title}{0}%
  \vskip 100\p@
  % HK 1994/11/09
  %\begin{center}%
    {% HK 1994/11/09
     \ifx\avb\undefined\relax\else\avb\fi%
     \LARGE%
     % HK 1994/11/09
     \baselineskip 3ex%
    \@title \par}%
    % HK 1994/11/09
    %\vskip 3em%
    \vskip 1.5em\par%
    {\large \@date}\par\vskip 3em\par%       % Set date in \large size.
    {\large
     \lineskip .75em%
      % HK 1994/11/09
      %\begin{tabular}[t]{c}%
        \@author
      % HK 1994/11/09
      %\end{tabular}%
      \par}%
      \vskip 1.5em%
    % HK 1994/11/09
    %{\large \@date \par}%       % Set date in \large size.
  %\end{center}%
  \par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\else
\renewcommand\maketitle{\par
  \begingroup
    \renewcommand\thefootnote{\fnsymbol{footnote}}%
    \def\@makefnmark{\hbox to\z@{$\m@th^{\@thefnmark}$\hss}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hbox to1.8em{\hss$\m@th^{\@thefnmark}$}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\fi

\renewenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      % HK 1994/11/18
      %\if@compatibility
        %\setcounter{page}{0}
      %\else
        \setcounter{page}{1}%
      % HK 1994/11/18
      %\fi%
      }%
    {\if@restonecol\twocolumn \else \newpage \fi
      \setcounter{page}{1}%
      % HK 1994/11/18
      %% \setcounter{page}{2}% No good idea
    }

\def\@makechapterhead#1{%
  % HK 1994/11/09
  %\vspace*{50\p@}%
  \vspace*{30\p@}%
  {\parindent \z@ \raggedright \reset@font%
    % HK 1994/11/09
    \rule{\textwidth}{1.2pt}\vspace*{20pt}
    \ifnum \c@secnumdepth >\m@ne%
         % HK 1994/11/09
         %\huge\bfseries \@chapapp{} \thechapter%
         \huge\ifx\avb\undefined\relax\else\avb\fi \@chapapp{} \thechapter%
         \par%
         \vskip 20\p@%
       \fi%
    % HK 1994/11/09
    %\Huge \bfseries #1\par%
    \Huge\ifx\avb\undefined\relax\else\avb\fi #1\par%
    \nobreak%
    % HK 1994/11/09
    %\vskip 40\p@
    \vskip10pt\rule{\textwidth}{0.3pt}\vskip30pt%
  }}

\def\@makeschapterhead#1{%
  % HK 1994/11/09
  %\vspace*{50\p@}%
  \vspace*{30\p@}%
  {\parindent \z@ \raggedright%
    \reset@font%
    % HK 1994/11/09
    %\Huge \bfseries  #1\par
    \rule{\textwidth}{1.2pt}\vspace*{20pt}
    \Huge \ifx\avb\undefined\relax\else\avb\fi #1\par%
    \nobreak%
    % HK 1994/11/09
    %\vskip 40\p@%
    \vskip10pt\rule{\textwidth}{0.3pt}\vskip30pt%
  }}

\newcommand\makebibliography{%
\ifx\chapter\undefined\relax\else%
\clearpage\thispagestyle{empty}\cleardoublepage%
\fi%
\bibliography}

\ifx\thebibliography\undefined%
\let\@tempa\newenvironment%
\else\let\@tempa\renewenvironment\fi%
\@tempa{thebibliography}[1]
     {\ifx\chapter\undefined%
       \section*{\@StoreRep{\bibname}{section}{Bib}{1}\bibname
         \@mkboth%
         % HK 1994/11/09
         %{\uppercase{\bibname}}{\uppercase{\bibname}}
         {\bibname}{\bibname}%
         }%
       % HK 1996/02/08: Added toc-entry:
       \addcontentsline{toc}{section}%
         {\protect\numberline{}{\protect\bibname}}%
      \else%
       \let\@old@makeschapterhead\@makeschapterhead%
       \def\@makeschapterhead{%
         \@StoreRep{\bibname}{chapter}{Bib}{0}%
         \@old@makeschapterhead}%
       \chapter*{\bibname
         \@mkboth%
         % HK 1994/11/09
         %{\uppercase{\bibname}}{\uppercase{\bibname}}
         {\bibname}{\bibname}%
         }%
       \let\@makeschapterhead\@old@makeschapterhead%
       % HK 1996/02/08: Added toc-entry:
       \addcontentsline{toc}{chapter}%
         {\protect\numberline{}{\protect\bibname}}%
      \fi
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\ifx\@openbib@code\undefined\let\@openbib@code\relax\fi

%% HK 1996/02/12: Some PostScript-printers (esp. the printers from
%% Apple) don't have the correct character codes for german left single
%% quote and german right single quote. \rglq and \rgrq will produce
%% the right output:
%\RequirePackage{rotating}
%\def\rglq{\raisebox{-0.05ex}{,}}
%\def\rglqq{\glqq}
%\def\rgrq{\raisebox{1.35ex}{\begin{turn}{180},\end{turn}}}
%\def\rgrqq{\grqq}

\endinput
