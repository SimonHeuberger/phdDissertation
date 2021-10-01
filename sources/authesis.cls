%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%    IDENTIFICATION   %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% AUTHESIS DOCUMENT CLASS  for LaTeX version 2e %%%%%%%%%%%%%
\ProvidesClass{sources/authesis}[2016/07/23 v2.51 University Thesis Class]
\NeedsTeXFormat{LaTeX2e}[2001/06/01]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANNOUNCEMENTS:
\typeout{
au-cms.sty is not supported. Please use natbib.sty for your bibliography.}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% KNOWN BUGS: 
%AI   There is no paper size option.  It is assumed that you will print
%     your thesis on letter paper (8.5in x 11in).  (Note that if you use
%     pdflatex you need to make sure it is not preconfigured for A4
%     ---check the pdflatex configuration file.  For details see
%     http://miktex.org/manual/chgpdftexcfg.html
%     I may add a size specification with geometry.sty to deal with this.
%AI:  Large list of tables may overun page number for list of tables?
%     I am pretty sure this is fixed.  Please report any difficulties.
%ABC  Large numbers of floats may lead to float placement difficulties.
%     At this point it is unclear if these are class specific.
% OTHER CONSIDERATIONS
%AI:  In the 'cas' option, page numbering must be a little odd, and this
%     is currently handled in a hackish way.  The problem is that the
%     Guide requests that page numbers sometimes occur at the bottom of the 
%     page but *inside* the normal text area (i.e., above the normal 1 inch
%     margin).  Look at a few real books and you will see that this is
%     a very unusual request---like many aspects of the CAS style, it
%     traces back to typewriter use and does not respect typesetting
%     conventions.  To accommodate typesetting packages the Guide now
%     says that page numbers "may" (NOT "must") occur at the bottom center
%     in certain situations.  The 'cas' option uses this placement only
%     for certain special pages (and even this was a pain to achieve).
%     CAS and the library are usually aware that the AUThesis class uses
%     Guide conforming page numbering, but
%     on occasion it seems to slip their minds, so you may have to 
%     remind someone, especially if they are new to dissertation vetting.
%     The most likely library objection is to the top-right page numbering in 
%     the front matter: you may need to show them this note to remind them
%     that this location is in fact Guide conforming and has passed library
%     inspection many times in the past.
%     Live with it, or better yet switch to the nicer 'econ' option,
%     which always puts page numbers at the top right.  (This too is
%     Guide conforming, according to the most recent edition of the Guide,
%     and has passed library inspection numerous times.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO DO (possibly):
% Copy titlepage changes over to proposal.sty
% Subclass report.cls
% Change font-size option to call base class options size10.clo etc
%    (that will require checking for deviations in the defs,
%     and there are some important ones)
%    But subclassing report.cls may allow use of its font-size options...
% \RequirePackage{chngpage} and put front-matter page number bottom middle
%  http://www.tug.org/teTeX/tetex-texmfdist/tex/latex/chngpage/chngpage.sty
% Switch to geometry package to set margins.
% Use toclof.sty to handle multiple lines, or switch to memoir class?
% Use setspace.sty to handle spacing (shd eliminate need for controlling
% spacing of tabular environments).  For reasons I do not understand 
% under the old fontsize defs this seems to require a def of \@currsize such as
% \def\@currsize{\normalsize}
% but I commented out the old defs and used the report class options defs
%
% MODIFICATION LOG:
% Last modified: 20060828
% ***********************************************************************
%AI: 20060926: change APPENDICES to APPENDIX (incomplete fix: 
%              CAS wants in toc CHAPTER but APPENDICES ... ah well: TODO)
%AI: 20060926: change APPENDIX to APPENDICES (later reverted: see above)
%AI: 20060901: change def of \sc and \sl (but why not just remove these??)
%AI: 20060828: move \parindent redef so that it works. (Puzzle: why need to move?)
%AI: 20060828: change some things to mixed case (Figure, Table, Page, Chapter, By, In)
%AI:           in response to a library request.  This reverses some previous requests ...
%AI: 20060217: slightly shorten \textheight to deal with slight bottom margin problem in PDF files
%AI: 20051011: add automatic loading of amsmath package
%AI: 20050717: in response to request:
%              use def (instead of newcommand) for scshape and slshape
%              because SWP somehow defines these before the document class is read??
%              again: not sure if this is the best approach
%AI: 20050717: in response to request:
%              enable clubpenalty and widowpenalty; not sure if this is a
%              good idea unless we allow ragged bottom.  And why not allow
%              raggedbottom? (Can place foonotes with yafoot package!)
%AI: 20050713: put title page in environment to restrict scope of changes.
%       This fixes a problem with the footnoterule.
%AI: 20050415: adjust spacing below captions (see: belowcaptionskip)
%AI: 20050402: change formatting of abstract; use abstract for abstractn
%AI: 20050402: move common formatting from option files to authesis.cls
%AI: 20050331: switch to tabular* in titlepage in order to span page
%AI: 20050329: Try to fix top margin on title page to exactly 2 inches
%AI: 20050329: Change BIBLIOGRAPHY to REFERENCES. (Glen Arnold of CAS
%AI:           says the library wants this, despite the Guide, for
%AI:           parenthetical citations.
%AI: 01/15/05: Change 'field' to 'degreefield'
%AI: 11/25/04: Increase label width for section and subsection,
%AI:           in order to accommodate >9 sections
%AI:           increase section label width from 2.3 to 2.7
%AI:           increase subsection label width from 3.2 to 3.4
%AI: 1/12/03:  Fix handling of appendices in auecon.clo and aucas.clo
%AI: 12/09/02: Fix formatting of title page in large type.
%AI:           Force title to uppercase in front matter.
%AI:           Adjust presentation of abstract.
%AI:           (Also: now works for long abstracts, but UMI will
%AI:           only print the first 350 words.)
%AI: 11/19/02: Fix bug affecting formatting of chapter 12, 23, etc
%AI: 03/21/02: Update some commands from recent report class.
%AI:           Change default options to 10pt and econ.
%AI:           Change the thebibliography environment.
%ABC 03/21/02:      Float placement parameters have been changed.
%ABC                Tables should be better placed. 
%ABC 10/22/01: Fixed page numbering problem with the "cas" option frontmatter.
%ABC 02/23/01: Changed table, figure, and equation numbering scheme to include
%ABC           the chapter number for "econ" option only. They remain 
%ABC	       sequential from the start of the document for the "cas"
%ABC           option.
%ABC 07/19/98: Previous change introduced a problem with the coverpage
%ABC	       table spacing. To solve, I added a \ssp before the tabular
%ABC           environment.
%ABC 07/01/98: Moved \ssp from the \tabular to the \table definition. This
%ABC	       solved the conflict with the threeparttable.sty package. 	
%ABC ********************************************************************
%    OTHER CHANGES RELATIVE TO VERSION 1:
%ABC   - Eliminaion of all references to two columns, double sided and 
%ABC     draft printing.
%ABC   - Additional \documentclass option, 'cas' or 'econ' to select 
%ABC     between the old AU-CAS dissertation format or the new improved
%ABC     economics department format.
%ABC
%ABC  The 'econ' format adopts an interpretation of the changes to 
%ABC  the AU dissertation format requirements approved in the
%ABC  summer of 1998. 
%ABC
%ABC  The economics format differs from the old format in the following 
%ABC  ways:
%ABC   - All numbered pages have the page number in the top right hand
%ABC     corner.
%ABC   - Section and subsection headers are numbered, subsubsection headers
%ABC     are not. All section headers are left justified.
%ABC   - NO UNDERLINING!!!!! The much more visually appealing italic font
%ABC     is used instead of underlining.
%
%
% ====================================================================
%     @LaTeX-class-file{
%        filename        = "authesis.cls",
%        version         = "2.26",
%        date            = "1 April 2005",
%        portedby		   = "Alan B. Cibils and Alan G. Isaac",
%   	   email		   = "acibils@american.edu",
%        codetable       = "ISO/ASCII",
%        keywords        = "LaTeX, authesis",
%        supported       = "until August 30, 1999"
%        docstring       = "This file is the main file for the authesis
%                           class, which is intended to meet the formatting
%   			    requirements for American University, College
%                           of Arts and Sciences Ph.D. dissertations." }
%   
%ABC		This file was originally hacked by Alan Cibils from UCTHESIS.CLS 
%ABC		v3.0 by Blaise B. Frederick. There have been substantial changes.
%ABC            Some changes introduced by Alan Cibils are indicated by the string 
%ABC            '% ABC' with appropriate explanatory comments where possible.
%AI:            Some changes introduced by Alan Isaac are indicated by the string 
%AI:            '%AI:' with appropriate explanatory comments where possible.

\typeout{IMPORTANT MESSAGES:}
\typeout{1: You MUST run LaTeX THREE (3) times after running BibTeX in order to make sure that entries in the table of contents are correct.}
\typeout{2: Before handing in the final version of your document make sure you have the latest version of AUTHESIS class.}



%    ****************************************
%    *               OPTIONS                *
%    ****************************************

% Font size option:
\newcommand\@ptsize{}
\DeclareOption{10pt}{\renewcommand\@ptsize{0}}
\DeclareOption{11pt}{\renewcommand\@ptsize{1}}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}}

%ABC The following options are to allow authesis.cls users to pick between
%ABC the traditional AU-CAS style and the default econ style.
\newcommand\@thesistype{}
\newcommand\@econstyle{}
\newcommand\@casstyle{}
\DeclareOption{econ}{\renewcommand\@thesistype{econ}
		     \renewcommand\@econstyle{1}
		     \renewcommand\@casstyle{0}}
\DeclareOption{cas}{\renewcommand\@thesistype{cas}
		     \renewcommand\@casstyle{1}
		     \renewcommand\@econstyle{0}}


%  DEFAULT OPTIONS: 10 point, econ
\ExecuteOptions{10pt,econ}      

%% CHOOSING THE TYPE SIZE:
%  The type size option is handled by reading a different file for each
%  size, as follows, to define font size-specific commands:
%       10pt : aut10.clo
%       11pt : aut11.clo
%       12pt : aut12.clo

\ProcessOptions   %AI: executes user chosen document options, if they exist
\input{sources/au\@thesistype.clo}\relax              % ABC to choose between format
\input{sources/aut1\@ptsize.clo}\relax                 

%  ****************************************
%  *            PACKAGES                  *
%  ****************************************
%AI: added 2005-10-11 to deal with questions
%AI: but I am not sure this is the best approach

\RequirePackage{amsmath}

%  ****************************************
%  *               FONTS                  *
%  ****************************************
%  Need to execute options before next line, which sets font.
\normalsize  % Choose the normalsize font.
% The shortform font selection commands are not defined in LaTeX2e.  This
% defines them so that they do non-orthogonal font selection, which is
% useful if you are in the middle of converting a document from 2.09.
% These commands are no longer used internally in this class. (BBF 10/31/94)

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
%\newcommand{\sl}{\slshape}  %AI: changed since SWP defines these
%\newcommand{\sc}{\scshape}  %AI: changed since SWP defines these
%AI: replace next two lines (which lack needed braces!)
%\def\sl\slshape
%\def\sc\scshape
%AI: hope this creates no problem for SWP users!
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}


%  ****************************************
%  *           PARAGRAPHING               *
%  ****************************************
%  Also: see the option specific paragraphing in the .clo files.
%\parskip 0pt plus 1pt  %AI: this was a big problem, replace with:
\setlength\parskip{0\p@ \@plus \p@}          % Extra vertical space between paragraphs.
\setlength\parindent{6ex}



% The following page-breaking penalties are defined

\@lowpenalty   51      % Produced by \nopagebreak[1] or \nolinebreak[1]
\@medpenalty  151      % Produced by \nopagebreak[2] or \nolinebreak[2]
\@highpenalty 301      % Produced by \nopagebreak[3] or \nolinebreak[3]

\@beginparpenalty -\@lowpenalty    % Before a list or paragraph environment.
\@endparpenalty   -\@lowpenalty    % After a list or paragraph environment.
\@itempenalty     -\@lowpenalty    % Between list items.

% LaTeX sets \clubpenalty and \widowpenalty to 150 and \displaywidowpenalty to 50
\clubpenalty = \@highpenalty         % 'Club line'  at bottom of page. %AI: was disabled
\widowpenalty = \@highpenalty        % 'Widow line' at top of page. %AI: was disabled
% \displaywidowpenalty % Math display widow line.
% \predisplaypenalty   % Breaking before a math display.
% \postdisplaypenalty  % Breaking after a math display.
% \interlinepenalty    % Breaking at a line within a paragraph.
% \brokenpenalty       % Breaking after a hyphenated line.


%%%%%%%%%%%%%% BEGIN: PAGE SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%


%AI: replace next 2 lines
%\lineskip 1pt
%\normallineskip 1pt
\setlength\lineskip{1\p@}   % \lineskip is 1pt for all font sizes.
\setlength\normallineskip{1\p@}

%\def\baselinestretch{1.37}  %AI: replace with:
\renewcommand\baselinestretch{1.37}   %AI: why not 1.67????

%  ****************************************
%  *            PAGE LAYOUT               *
%  ****************************************
%
% All margin dimensions measured from a point one inch from top and side
% of page.

% SIDE MARGINS:
                         % Values for one-sided printing:
\oddsidemargin 0.1in     %   Note that \oddsidemargin = \evensidemargin
\evensidemargin 0.1in
\marginparwidth 0pt
%\marginparsep           % option specific (but why?)

% VERTICAL SPACING:
                         % TOP OF PAGE:
\topmargin -0.25in       %    Nominal distance from top of page to top
                         %    of box containing running head.
						 % ABC was 0.0in
\headheight 12pt         %    Height of box containing running head.
\headsep 25pt            %    Space between running head and text.
%\topskip                %    option specific; set to point size
                         % BOTTOM OF PAGE:
\footskip 20pt           %    Distance from baseline of box containing
                         %    foot to baseline of last line of text.
 						 % ABC was 30pt

% DIMENSION OF TEXT:

%\textheight=8.80in
\textheight=8.75in       %replace 8.80, which is right, with 8.75, to compensate for odd PDF bottom margin printout
%AI 20160722: changed 5.95truein due to narrower left margin (see current guide)
\textwidth 6.3truein     % Width of text line.
                         % For two-column mode:
\columnsep 10pt          %    Space between columns
\columnseprule 0pt       %    Width of rule between columns.

% A \raggedbottom command causes 'ragged bottom' pages: pages set to
% natural height instead of being stretched to exactly \textheight.
%%%%%%%%%%%%%%%% END: PAGE SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%




%AI: this is a horrible hack and should be replaced
%the following lines may (??) create problems in the tabbing environment
\ifnum \@ptsize = 0 \def\AIindent{-0.55in}\fi   % ABC for indentation of
\ifnum \@ptsize = 1 \def\AIindent{-0.60in}\fi   % ABC CHAPTER and APPENDIX 
\ifnum \@ptsize = 2 \def\AIindent{-0.25in}\fi   % ABC lines. 


%%SPACING:
% For some environments we switch back to single-spacing, also
% provides \dsp in case you need double-spacing where single-spacing
% is otherwise used.
%ABC: changed 1.37 to 1.67 for double space.
%AI:  but not for all font sizes?  check why

\def\ssp{\def\baselinestretch{1.0}\large\normalsize}
\def\dsp{\def\baselinestretch{1.67}\large\normalsize}

% \smallssp is used to produce tabular environments in the small font.
% This is required because single-spacing requires a change in font size.
% \scriptsizessp is a still smaller version of the same thing.

\def\smallssp{\def\baselinestretch{1.0}\large\small}
\def\scriptsizessp{\def\baselinestretch{1.0}\large\scriptsize}

%% PREPARING A FOREIGN LANGUAGE VERSION:
%
% This document style is for documents prepared in the English language.
% To prepare a version for another language, various English words must
% be replaced.  Many of the English words that required replacement are
% indicated below, where we give the name of the command in which the
% words appear, and which must be redefined, with the actual words
% underlined.
%
% Other English words that need replacement can be found in the macros
% supporting the title and approval pages.
%
%school or college (e.g., College of Arts and Sciences)
\newcommand\deansigline{\parbox{2.4in}{Dean of the School of Public Affairs}}   % used below dean's signature
\newcommand\schoolorcollege{College of Arts and Sciences}   % used in Faculty specification
% \tableofcontents:
\newcommand\contentsname{TABLE OF CONTENTS}   % capitalization required by Guide
% \listoffigures:
\newcommand\listfigurename{LIST OF FIGURES}   % capitalization required by Guide
% \listoftables:
\newcommand\listtablename{LIST OF TABLES}     % capitalization required by Guide
% \thebibliography:  see natbib.cfg!!
\newcommand\bibname{REFERENCES}               % Library wants "REFERENCES" for parenthetical cites
% \theindex:
\newcommand\indexname{Index}
% figure environment:
\newcommand\figurename{Figure}
% table environment:
\newcommand\tablename{Table}
% \chapter:
\newcommand\chaptername{CHAPTER}		  %Capitalized, (Library wants this ...)
% \appendix:
\newcommand\appendixname{APPENDIX}		   % ABC Capitalized, CAS requirement
% \part
\newcommand\partname{PART}
% abstract environment:
\newcommand\abstractname{ABSTRACT}            % ABC Capitalized, CAS requirement 
% acknowledge environment:
\newcommand\acknowledgename{ACKNOWLEDGEMENTS} % ABC capitalized, CAS requirement
%                    ~~~~~~~~~~~~~~~~




% ********************************************************
% ********************************************************
% ************* BEGIN: FRONT MATTER **********************
% ******************************************************** 
% ******************************************************** 


%
% DECLARATIONS
%
% These macros are used to declare arguments needed for the
% construction of the front matter.  

% The year the degree will be officially conferred
\def\degreeyear#1{\gdef\@degreeyear{#1}}

% The full (unabbreviated) name of the degree
\def\degree#1{\gdef\@degree{#1}}

% The name of your thesis committee chair
\def\chair#1{\gdef\@chair{#1}}

% The names of your other committe members, one per line
%
% ABC I added the following definitions because I needed to be able
% ABC to access the other committee members names individually to
% ABC palce under the signature line. A maximum of six committee members
% ABC will be allowed as a result. To add more, simply add more
% ABC definitions like those below.  

\def\secondreader#1{\gdef\@secondreader{#1}}
\def\thirdreader#1{\gdef\@thirdreader{#1}}
\def\fourthreader#1{\gdef\@fourthreader{#1}}
\def\fifthreader#1{\gdef\@fifthreader{#1}}
\def\sixthreader#1{\gdef\@sixthreader{#1}}

% The name of your degree field (e.g. Economics, Computer Science) 
\def\degreefield#1{\gdef\@degreefield{#1}}





% ********************************************************
% ************* BEGIN: TITLE PAGE  ***********************
% ******************************************************** 

%%% Define the fonts that will be used in the front matter headings
\def\fmfont{\fontsize\@xiipt{14.5}\selectfont}
\def\fmsmallfont{\fontsize\@xiipt{10pt}\selectfont}

%AI: CAS wants exactly 2 inch top margin on title page
%AI: if you see something else, try fiddling with the numbers
\newcommand{\biggertopmargin}{%
\ifnum \@ptsize = 0 \vspace*{28bp} \fi
\ifnum \@ptsize = 1 \vspace*{27bp} \fi             %AI: changed to fit page
\ifnum \@ptsize = 2 \vspace*{26bp} \fi        %AI: changed to fit page
}

%%% Define the title page environment         %AI: added 20050713
\newenvironment{titlepage}
    {%
\setcounter{page}{1}%                         %AI: superfluous, I believe
\let\footnotesize\small%
\let\footnoterule\relax%
\clearpage%\biggertopmargin%                 %AI 20160722 commented out biggertopmargin (see note above)
\thispagestyle{empty}
    }%
    {\setcounter{footnote}{0}
    }

%%% Define 'maketitle'
%%% \maketitle outputs the complete titlepage.
%%% It requires the above font macros and titlepage environment.
\newcommand{\maketitle}{%%begin: maketitle def
\begin{titlepage}
{\centering\dsp%                                                 % change from ssp to dsp for title
\fmfont\bfseries \uppercase\expandafter{\@title}\par}  % typeset TITLE of the Dissertation
{\centering\ssp%                                                    % switch back to single space (ssp)
\setlength{\parskip}{0.75\baselineskip}
%\fmsmallfont                        % typeset AUTHOR name in small font no longer permitted
By \par {\@author}
\par
Submitted to the \par                              
Faculty of the School of Public Affairs \par   
of American University \par                        
in Partial Fulfillment of \par                     
the Requirements for the Degree \par               
of {\@degree} \par
In \par
{\@degreefield}\par
}

\vspace{0.4cm}
% **********************************************************
% AA - Assen Assenov - Change this part for the needs of SPA or CAS 
%%% Approval lines.
% ABC  I did this with the tabular environment because
% ABC  it seemed a lot easier, given the AU requirement of having the 
% ABC  professor names under the signature line. 
%AI: switch to tabular* in order to span page
% AA - Added Dean;s name
{%%begin approval lines
\ssp
\parindent\z@ 
\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}} l r r l l l r }
 &          & Chair: &                             & & &\\\cline{4-7}
 &          &        & {\@chair}                   & & &\\
 &          &        &                             & & &\\
 &          &        &                             & & &\\ \cline{4-7}
 &          &        & {\@secondreader}            & & &\\
 &          &        &                             & & &\\
 &          &        &                             & & &\\ \cline{4-7}
 &          &        & {\@thirdreader}             & & &\\
 &          &        &                             & & &\\
 &          &        &                             & & &\\ \cline{4-7}
 &          &        & {\@fourthreader}             & & &\\
 &          &        &                             & & &\\
 &          &        &                             & & &\\ \cline{4-7}
 &          &        & {\@fifthreader}            & & &\\
 &          &        &                             & & &\\
 &          &        &                             & & &\\ \cline{1-2}
%\small{Dean of the School \phantom{ CAS or SPA}} & & & & & &\\
 \small{\deansigline} & & & & & &\\
&          &                             & & &\\
                                            & & & & & \\ \cline{1-2}
 \small{Date}                               & & & & & \\
\end{tabular*}
}%%end approval lines

\vfill
 
{\ssp\centering
{\@degreeyear} \par 
American University \par
Washington, D.C. 20016 \par} 
%\thispagestyle{empty}%
\end{titlepage}
}%%end: maketitle def



% ********************************************************
% *************** END: TITLE PAGE  ***********************
% ******************************************************** 



% ********************************************************
% ****************  COPYRIGHTPAGE  ***********************
% ********************************************************
%
% This is a macro, not an environment, because it can be generated
% with the \title, \author, and \year macros.

\newcommand{\copyrightpage}{\clearpage\biggertopmargin
{\centering\dsp\fmfont\copyright \ COPYRIGHT  \par  % typeset TITLE of the Dissertation
by \par
\@author \par
\@degreeyear \par
ALL RIGHTS RESERVED\par}%end centering 

\thispagestyle{empty}
}


% ********************************************************
% ********************  ABSTRACT  ************************
% ********************************************************

%AI: old def did not follow abstractn because it did not use chapter*
%AI: old def (commented out):
%\def\abstract{\clearpage
%\thispagestyle{empty}       % ABC so that page # does not print
%\setlength{\topmargin}{0.45in}
%\dsp   	
%\begin{center}              
%{\fmfont
%{\bfseries\uppercase\expandafter{\@title}}\par
%by \par
%\@author \par
%\bfseries{\abstractname} \par
%}
%\vspace{.4in}
%\end{center}
%}
%
%\def\endabstract{\par\vfil\null}

%AI: new def:
\def\abstract{%
\chapter*{\centering\uppercase\expandafter{\@title}}
\vspace*{-1.67\baselineskip}
{\centering\dsp\fmsmallfont\mdseries by\par\@author\par}
\vspace*{0.67\baselineskip}
{\centering\fmfont\bfseries\uppercase\expandafter{\abstractname}\par} 
\vspace*{0.67\baselineskip}
\pagestyle{empty}                     %AI: no subsequent page numbers
\thispagestyle{empty}                 %AI: no current page number
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% BEGIN:  ABSTRACTN  %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AI: Abstract on Numbered page
%ABC This second abstract environment produces an abstract page
%ABC with the first page number printed at bottom center. This is to 
%ABC fulfill AU-CAS requirements of two abstract pages, one numbered.
%ABC 
%ABC I tried to get this to work like an environment, but I got really
%ABC strange results with page number placement in the 'econ' class. Since
%ABC I was not able to correct it, I decided to use it like a \chapter 
%ABC rather than an environment.

\newcommand{\abstractn}{
\setcounter{page}{1}
\abstract
\pagestyle{plain} 
\thispagestyle{chapfirst}
	\addcontentsline{toc}{chapter}{\abstractname}
	\addtocontents{toc}{\protect\addvspace{10\p@}}%
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%  ACKNOWLEDGEMENTS  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The acknowledgements environment puts a large, bold, centered 
% "Acknowledgements" label at the top of the page.
%ABC 
%ABC I tried to get this to work like an environment, but I got really
%ABC strange results with page number placement in the 'econ' class. Since
%ABC I was not able to correct it, I decided to use it like a \chapter 
%ABC rather than an environment.

\def\acknowledgements{
  \chapter*{\centering\acknowledgename}
  \addcontentsline{toc}{chapter}{\acknowledgename}
  \addtocontents{toc}{\protect\addvspace{10\p@}}%
}
%\def\endacknowledgements{\relax}

% ********************************************************
% **************  FRONTMATTER environment  ***************
% ********************************************************
%
% The 'frontmatter' environment makes sure that page numbering is set
% correctly (roman, lower-case, starting at 2) for the front matter
% that follows the copyright page.  It also resets page-numbering for
% the remainder of the dissertation (arabic, starting at 1).
\newenvironment{frontmatter}
{\pagestyle{chapfirst}                          %AI: I do not think this is used
\setcounter{page}{2}
\renewcommand{\thepage}{\roman{page}}
}
{\newpage
\pagestyle{plain}
\renewcommand{\thepage}{\arabic{page}}
\setcounter{page}{1}}


%    ****************************************
%    *                LISTS                 *
%    ****************************************
%
% ENUMERATE
%
%  Enumeration is done with four counters: enumi, enumii, enumiii
%  and enumiv, where enumN controls the numbering of the Nth level
%  enumeration.  The label is generated by the commands \labelenumi
%  ... \labelenumiv.  The expansion of \p@enumN\theenumN defines the
%  output of a \ref command.
%AI: use more recent report.cls for defintions

\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}

\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}

\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}

%AI: old versions
%\def\theenumi{\arabic{enumi}}
%\def\theenumii{\alph{enumii}}
%\def\theenumiii{\roman{enumiii}}
%\def\theenumiv{\Alph{enumiv}}


%
% ITEMIZE
%
% Itemization is controlled by four commands: \labelitemi, \labelitemii,
% \labelitemiii, and \labelitemiv, which define the labels of the
% various itemization levels.

\def\labelitemi{$\m@th\bullet$}
\def\labelitemii{\bfseries --}
\def\labelitemiii{$\m@th\ast$}
\def\labelitemiv{$\m@th\cdot$}

% ABC the following lines were added to indent lists the same amount as
% ABC paragraphs. Following lines are back to left margin following 
% ABC Turabian manual. Hey, she said it, not me!!!!
% ABC If you have nested lists you might run into problems,
% ABC but you can simply add definitions for \leftmarginii-iv greater
% ABC than the value below. NOTE: displacements are from previous level.

\setlength{\leftmargini}{0.0em}              

% ABC The following code was lifted from latex.ltx. The problem is
% ABC that \itemindent can only be set in the list_decl portion of
% ABC the \list definition. 

\def\list#1#2{\ifnum \@listdepth >5\relax \@toodeep
     \else \global\advance\@listdepth\@ne \fi
  \rightmargin \z@ \listparindent\z@ \itemindent 1.5em  % ABC was \z@
  \csname @list\romannumeral\the\@listdepth\endcsname
  \def\@itemlabel{#1}\let\makelabel\@mklab \@nmbrlistfalse #2\relax
  \@trivlist
  \parskip\parsep \parindent\listparindent
  \advance\linewidth -\rightmargin \advance\linewidth -\leftmargin
  \advance\@totalleftmargin \leftmargin
  \parshape \@ne \@totalleftmargin \linewidth
  \ignorespaces}

%
% VERSE
%
%   The verse environment is defined by making clever use of
%   list environment parameters.  The user types \\ to end a line.
%AI: take from report class

\newenvironment{verse}
               {\let\\\@centercr
                \list{}{\ssp\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item\relax}
               {\endlist}

%
% QUOTATION
%
%   Fills lines
%   Indents paragraph
%
% ABC Modified \listparindent and \leftmargin to conform to Turabian.
% ABC Block quotes are supposed to be indented four characters (which
% ABC I take to mean that they start on the fifth) and quote paragraph
% ABC is also four characters which is why \leftmargion is set to 
% ABC \listparindent.
%AI: change to "newenvironment"

\newenvironment{quotation}
               {\list{}{\ssp\listparindent 2em%
                        \itemindent    \listparindent
                        \rightmargin\z@
                        \leftmargin\listparindent
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}

%
% QUOTE 
%
%   Same as quotation except no paragraph indentation,
%
% ABC Modified \listparindent and \leftmargin to conform to Turabian.
% ABC Quotes are supposed to be indented four characters (which
% ABC I take to mean that they start on the fifth) and no paragraph
% ABC  indentation.
%AI: change to "newenvironment"

\newenvironment{quote}
               {\list{}{\ssp\listparindent 0em%
                        \itemindent    \listparindent
                        \rightmargin\z@
                        \leftmargin 2em
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}

%
% DESCRIPTION
%
%  To change the formatting of the label, you must redefine
%  \descriptionlabel.
%
% ABC Modified this to not be at all indented, conforming to the 
% ABC itemize and enumerate environment requirements.

\def\descriptionlabel#1{\hspace\labelsep \emph{#1}}
\def\description{\list{}{\labelwidth\z@ \itemindent 0.0em
                                        \leftmargin 0.0em
       \let\makelabel\descriptionlabel}}

\let\enddescription\endlist

\newdimen\descriptionmargin
\descriptionmargin=3em


%    ****************************************
%    *         OTHER ENVIRONMENTS           *
%    ****************************************
%
%
% VERBATIM
%
% standard verbatim mode driver macro is modified to use
% single-spacing.  There appears to be a small problem with extra
% vertical space (1 or 2 points worth) after this environment.
% Probably, this results from returning to double-spacing before
% issuing a \parskip, but I am not enough of TeX hacker to figure out
% the source of the problem. (EVM, 9/25/94)
% 94/11/03 BBF converted \tt to \ttfamily

\def\@verbatim{\trivlist\ssp \item[]\if@minipage\else\vskip\parskip\fi
\leftskip\@totalleftmargin\rightskip\z@
\parindent\z@\parfillskip\@flushglue\parskip\z@
%%RmS 91/08/26 Added \@@par to clear possible \parshape definition
%%from a surrounding list (the verbatim guru says)
\@@par
\@tempswafalse \def\par{\if@tempswa\hbox{}\fi\@tempswatrue\@@par
\penalty\interlinepenalty}%
\obeylines \ttfamily \catcode``=13 \@noligs \let\do\@makeother
\dospecials}

%
% ARRAY AND TABULAR
%

\arraycolsep 5pt     % Half the space between columns in an array
                     % environment.
\tabcolsep 6pt       % Half the space between columns in a tabular
                     % environment.
\arrayrulewidth .4pt % Width of rules in array and tabular environment.
\doublerulesep 2pt   % Space between adjacent rules in array or tabular
                     % environment.

% We have to redefine array here in order to make it single-spaced.
% (Not sure if this is really necessary.)
\def\array{\let\@acol\@arrayacol \let\@classz\@arrayclassz
 \let\@classiv\@arrayclassiv
 \let\\\@arraycr\let\@halignto\@empty\@tabarray}

%
% TABBING
%

\tabbingsep \labelsep   % Space used by the \' command.
                        %  (See LaTeX manual.)'


%
% TABULAR
%
% We have to redefine tabular here in order to make it single-spaced.
%ABC removed \ssp due to conflict with threeparttable
\def\tabular{\par\let\@halignto\@empty\@tabular} 
\expandafter \def\csname tabular*\endcsname #1%
{\par\ssp\def\@halignto{to#1}\@tabular}

% \smalltabular and \smalltabular* are alternative tabular styles
% in the \small font.  They are required because single-spacing implies a 
% change of font to a particular size.
% EVM, 7/20/93 

\def\smalltabular{\par\smallssp\let\@halignto\@empty\@tabular}
\def\endsmalltabular{\endtabular}
\expandafter \def\csname smalltabular*\endcsname #1%
{\par\smallssp\def\@halignto{to#1}\@tabular}
\expandafter \let \csname endsmalltabular*\endcsname = \endtabular

% \scriptsizetabular and \scriptsizetabular* are analogs of \smalltabular

\def\scriptsizetabular{\par\scriptsizessp\let\@halignto\@empty\@tabular}
\def\endscriptsizetabular{\endtabular}
\expandafter \def\csname scriptsizetabular*\endcsname #1%
{\par\scriptsizessp\def\@halignto{to#1}\@tabular}
\expandafter \let \csname endscriptsizetabular*\endcsname = \endtabular

%
% MINIPAGE
%
%  \@minipagerestore is called upon entry to a minipage environment to
%  set up things that are to be handled differently inside a minipage
%  environment. In the current styles, it does nothing.
%
% \skip\@mpfootins : plays same role for footnotes in a minipage as
%                    \skip\footins does for ordinary footnotes

\skip\@mpfootins = \skip\footins

%
% FRAMEBOX
%
\fboxsep = 3pt    % Space left between box and text by \fbox and
                  % \framebox.
\fboxrule = .4pt  % Width of rules in box made by \fbox and \framebox.


%    ****************************************
%    *        CHAPTERS AND SECTIONS         *
%    ****************************************
%
% DEFINE COUNTERS:
%ABC 6/24/98: Set \secnumdepth to 2 so that only \section and \subsection
%ABC          are numbered.
%AI: take from report class
\setcounter{secnumdepth}{2}
\newcounter{part}
\newcounter{chapter}
\newcounter{section}[chapter]
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]
\newcounter{subparagraph}[paragraph]
%AI: redefine associated \the commands
\renewcommand{\thepart}{\@Roman\c@part}
\renewcommand{\thechapter}{\@arabic\c@chapter}
\renewcommand{\thesection}{\thechapter.\@arabic\c@section}
\renewcommand{\thesubsection}{\thesection.\@arabic\c@subsection}
\renewcommand{\thesubsubsection}{\thesubsection .\@arabic\c@subsubsection}
\renewcommand{\theparagraph}{\thesubsubsection.\@arabic\c@paragraph}
\renewcommand{\thesubparagraph}{\theparagraph.\@arabic\c@subparagraph}
\newcommand{\@chapapp}{\chaptername}

% \@chapapp is initially defined to be '\chaptername'.  The \appendix
% command redefines it to be '\appendixname'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        PART                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ABC WARNING: I have not really modified this enough to make sure
% ABC          that it conforms to AU standards. If you plan to use
% ABC          the \part command, further modification will be required,
% ABC          including table of contents modifications, which can be
% ABC          messy. Good luck.

\def\part{\cleardoublepage   % Starts new page.
   \thispagestyle{chapfirst}% ABC Changed from 'plain'.
   \hbox{}\vfil               % Add fil glue to center title
   %\bgroup  \centering      % BEGIN centering %% Removed 19 Jan 88
  \secdef\@part\@spart}

\def\@part[#1]#2{\ifnum \c@secnumdepth >-2\relax  % IF secnumdepth > -2
        \refstepcounter{part}%                    %   THEN step
                                                  %         part counter
        \addcontentsline{toc}{part}{\thepart      %        add toc line
        \hspace{1em}#1}\else                      %   ELSE add
                                                  %         unnumb. line
        \addcontentsline{toc}{part}{#1}\fi        % FI
   \markboth{}{}%
   {\centering                       % %% added 19 Jan 88
    \interlinepenalty \@M            %% RmS added 11 Nov 91
    \ifnum \c@secnumdepth >-2\relax  % IF secnumdepth > -2
      \large\bfseries \partname~\thepart    %   THEN Print '\partname' and                            % ABC changed from huge
   % \par                             %         number in \huge bold.
    \vskip 20\p@\fi    % ABC commented %        Add space before title.
    \large \bfseries                 % FI  % ABC changed from huge 
    #2\par}\@endpart}                % Print Title in \large bold.
                                     % Bug Fix 13 Nov 89: #1 -> #2

% \@endpart finishes the part page
%
\def\@endpart{\vfil\newpage}   % End page with 1fil glue.
   
\def\@spart#1{{\centering      % %% added 19 Jan 88
   \interlinepenalty \@M       %% RmS added 11 Nov 91
   \large \bfseries            % ABC Print title in \large boldface
   #1\par}\@endpart}

%    ****************************************
%    *         TABLE OF CONTENTS, ETC.      *
%    ****************************************
%
% A \subsection command writes a
%       \contentsline{subsection}{TITLE}{PAGE}
% command on the .toc file, where TITLE contains the contents of the
% entry and PAGE is the page number.  If subsections are being numbered,
% then TITLE will be of the form
%       \numberline{NUM}{HEADING}
% where NUM is the number produced by \thesubsection.  Other sectioning
% commands work similarly.
%
% A \caption command in a 'figure' environment writes
%    \contentsline{figure}{\numberline{NUM}{CAPTION}}{PAGE}
% on the .lof file, where NUM is the number produced by \thefigure and
% CAPTION is the figure caption.  It works similarly for a 'table'
% environment.
%
% The command \contentsline{NAME} expands to \l@NAME.  So, to specify
% the table of contents, we must define \l@chapter, \l@section,
% \l@subsection, ... ; to specify the list of figures, we must define
% \l@figure; and so on.  Most of these can be defined with the
% LaTeX command \@dottedtocline, which works as follows.
%
% \@dottedtocline{LEVEL}{INDENT}{NUMWIDTH}{TITLE}{PAGE}
%    LEVEL    : An entry is produced only if LEVEL < or = value of
%               'tocdepth' counter.  Note, \chapter is level 0, \section
%               is level 1, etc.
%    INDENT   : The indentation from the outer left margin of the start
%               of the contents line.
%    NUMWIDTH : The width of a box in which the section number is to go,
%               if TITLE includes a \numberline command.
%
% This command uses the following three parameters, which are set
% with a \def (so ems can be used to make them depend upon the font).
%   \@pnumwidth : The width of a box in which the page number is put.
%   \@tocrmarg  : The right margin for multiple line entries.  One
%                 wants \@tocrmarg > or = \@pnumwidth
%   \@dotsep    : Separation between dots, in mu units.  Should be
%                 \def to a number like 2 or 1.7

\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{2}	% ABC Set to 2 so that
				% ABC \subsubsections do not appear 
				% ABC in table of contents
%
% TABLEOFCONTENTS
%
%  In authesis style, \tableofcontents, \listoffigures, etc. are always
%  set in single-column style.  @restonecol
% ABC Table of contents must be double spaced with entries longer than
% ABC one line single spaced.

\newcommand\tableofcontents{
\pagestyle{plain}
%\pagestyle{chapfirst}  %cannot have this bec changes margin-bottom
  \chapter*{\contentsname
       \@mkboth{\uppercase{\contentsname}}{\uppercase{\contentsname}}}%
  {\ssp\@starttoc{toc}}} % ABC changed to dsp

\def\l@part#1#2{\addpenalty{-\@highpenalty}%
   \addvspace{2.25em plus\p@}% space above part line
   \begingroup
   \@tempdima 3em         % width of box holding part number, used by
     \parindent \z@ \rightskip \@pnumwidth             %% \numberline
     \parfillskip -\@pnumwidth
     {\large \bfseries          % set line in \large boldface
     \leavevmode          % TeX command to enter horizontal mode.
     #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
     \nobreak             % Never break after part entry
    \global\@nobreaktrue                        
    \everypar{\global\@nobreakfalse\everypar{}}
                                               
   \endgroup}

% ABC The reason there is a chapter line and a chap line is that most
% ABC of the entries in the table of contents use the chapter format.
% ABC Since the chapter must be indented, I needed an extra definition.

\def\l@chapter{\@dottedtocline{0}{0.0em}{1.0em}}
\def\l@chap{\@dottedtocline{0}{2.0em}{1.6em}}
\def\l@section{\@dottedtocline{1}{3.0em}{2.7em}}           %AI: width was 2.3
\def\l@subsection{\@dottedtocline{2}{4.0em}{3.4em}}        %AI: width was 3.2
\def\l@subsubsection{\@dottedtocline{3}{5.0em}{4.1em}}
\def\l@paragraph{\@dottedtocline{4}{6em}{5em}}
\def\l@subparagraph{\@dottedtocline{5}{7em}{6em}}

%
% LIST OF FIGURES
%
% ABC Double space list of figures, with entries longer than one line
% ABC single spaced.

\def\listoffigures{
\pagestyle{plain} 
%\pagestyle{chapfirst}  %cannot have this bec changes margin-bottom
  \chapter*{\listfigurename 
  \@mkboth{\uppercase{\listfigurename}}%
{\uppercase{\listfigurename}}}
   \addcontentsline{toc}{chapter}{\listfigurename}
   \addtocontents{toc}{\protect\addvspace{10\p@}}%
   {\ssp\@starttoc{lof}}			
   	\addtocontents{lof}{\noindent{Figure}\hfill{Page}\par}%AI: abc had \hspace{-.4in} before TABLE
	\addtocontents{lof}{\protect\addvspace{10\p@}}}
			% ABC added above line per AU requirement

\def\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}

%
% LIST OF TABLES
%
% ABC Double space list of tables, with entries longer than one line
% ABC single spaced.

\def\listoftables{
\pagestyle{plain} 
%\pagestyle{chapfirst}  %cannot have this bec changes margin-bottom
  \chapter*{\listtablename 
  \@mkboth{\uppercase{\listtablename}}%
{\uppercase{\listtablename}}}
   \addcontentsline{toc}{chapter}{\listtablename}
   \addtocontents{toc}{\protect\addvspace{10\p@}}%
   {\ssp\@starttoc{lot}}			% ABC changed to dsp
   	\addtocontents{lot}{\noindent{Table}\hfill{Page}\par}%AI: abc had \hspace{-.4in} before TABLE
	\addtocontents{lot}{\protect\addvspace{10\p@}}}
			% ABC added above line per AU requirement
\let\l@table\l@figure

%    ****************************************
%    *             BIBLIOGRAPHY             *
%    ****************************************
% 
%AI: 
% For citations plese use:
%       natbib.sty with our natbib.cfg 
%       and au-cms.bst.
% These have modifications specific to AU requirements.
% Any other approach, and you are on your own!

%AI: deleted redundant bibname definition 
%AI: took thebibliography environment mostly from report.cls. 
%AI: Really this is intended to be a dummy thebibliography environment,
%AI: replaced by natbib.sty
%AI: So once au-cms.sty is truly gone for good we should just use
%AI:  \newenvironment{thebibliography}{}{}
%AI:   \RequirePackage[options,...]{natbib}

\newenvironment{thebibliography}{}{}
\newcommand{\newblock}{}                                   %AI: dummy def

% old version
%% \newenvironment{thebibliography}[1]
%%    {\chapter*{\bibname test
%%         \@mkboth{\MakeUppercase\bibname}{\MakeUppercase\bibname}}%
%%       \list{\@biblabel{\@arabic\c@enumiv}}%
%%            {\settowidth\labelwidth{\@biblabel{#1}}%
%%    \addtocontents{toc}{\hspace{\AIindent}{  }}  
%%    \addcontentsline{toc}{chapter}{\bibname}
%%    \leftmargin\labelwidth
%%    \advance\leftmargin\labelsep
%%    \advance\leftmargin\bibindent
%%    \itemindent -\bibindent
%%    \listparindent\itemindent
%%    \parsep 10\p@         % ABC was \z@: entries must be double spaced
%%    \usecounter{enumi}}
%%    \sloppy
%%       \clubpenalty4000
%%       \@clubpenalty \clubpenalty
%%       \widowpenalty4000%
%%       \sfcode`\.\@m}
%%      {\def\@noitemerr
%%        {\@latex@warning{Empty thebibliography environment}}%
%%       \endlist}
%%    

%    ****************************************
%    *             FOOTNOTES                *
%    ****************************************
%
% \footnoterule is a macro to draw the rule separating the footnotes
% from the text.  It should take zero vertical space, so it needs a
% negative skip to compensate for any positive space taken by the rule.
% (See PLAIN.TEX.)

%AI: updated from report.cls
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule\@width.4\columnwidth   % The \hrule has default height of .4pt.
  \kern2.6\p@}
  

\@addtoreset{footnote}{chapter}  % Numbers footnotes within chapters

%   \@makefntext{NOTE} :
%        Must produce the actual footnote, using \@thefnmark as the mark
%        of the footnote and NOTE as the text.  It is called when
%        effectively inside a \parbox of width \columnwidth (i.e., with
%        \hsize = \columnwidth).
%
%        The following macro indents all lines of the footnote by 10pt,
%        and indents the first line of a new paragraph by 1em.  To
%        change these dimensions, just substitute the desired value for
%        '10pt' [in both places] or '1em'.  The mark is flushright
%        against the footnote.
%          \long\def\@makefntext#1{\@setpar{\@@par\@tempdima \hsize
%             \advance\@tempdima-10pt\parshape \@ne 10pt \@tempdima}\par
%             \parindent 1em\noindent
%             \hbox to \z@{\hss$\m@th^{\@thefnmark}$}#1}
%
%        A simpler macro is used, in which the footnote text is
%        set like an ordinary text paragraph, with no indentation except
%        on the first line of a paragraph, and the first line of the
%        footnote.  Thus, all the macro must do is set \parindent
%        to the appropriate value for succeeding paragraphs and put the
%        proper indentation before mark.

\long\def\@makefntext#1{\parindent 1em\noindent 
            \hbox to 3.5em{\hss$\m@th^{\@thefnmark}$}#1}
           % ABC    |-----| 
		   % ABC was 1.8em Changed so it is same indent as paragraphs.
				   
% \@makefnmark : A macro to generate the footnote marker that goes
%    in the text.  Default used.

% Single-space footnotes.

\long\def\@footnotetext#1{\insert\footins{\ssp\reset@font\footnotesize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
   \edef\@currentlabel{\csname p@footnote\endcsname\@thefnmark}\@makefntext
    {\rule{\z@}{\footnotesep}\ignorespaces
      #1\strut}}}
 
%    ****************************************
%    *         FIGURES AND TABLES           *
%    ****************************************
%
% Float placement parameters.  See LaTeX manual for their definition.
% ABC (Page 199 of Lamport book.)
%
\setcounter{topnumber}{2}
\def\topfraction{.7}
\setcounter{bottomnumber}{1}
\def\bottomfraction{.3}
\setcounter{totalnumber}{3}
\def\textfraction{.2}
\def\floatpagefraction{.99}     %ABC Changed from .5
\setcounter{dbltopnumber}{2}
\def\dbltopfraction{.7}
\def\dblfloatpagefraction{.5}

% \@makecaption{NUMBER}{TEXT} : Macro to make a figure or table caption.
%      NUMBER : Figure or table number--e.g., 'Figure 3.2'
%      TEXT   : The caption text.
%  Macro should be called inside a \parbox of right width, with \normalsize.


%AI: comment out old def
%\long\def\@makecaption#1#2{% Add space only if something is ...
%   \par\ifdim\prevdepth>-\p@ \vskip 10\p@\relax \fi % above the caption
%   \setbox\@tempboxa\hbox{\vbox to\topskip{}#1. #2\unskip\strut}% 
%   \ifdim \wd\@tempboxa >\hsize      % IF longer than one line:
%       {\ssp#1. #2\unskip\strut\par} %   THEN set as ordinary paragraph.
%     \else                           %   ELSE  center.
%       \hbox to\hsize{\hfil\unhbox\@tempboxa\hfil}%
%   \fi}

%AI: 20050415 switch to method in report.cls
%(But note table and figure numbers must be followed by periods, not colons.)
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{9\p@}
\setlength\belowcaptionskip{9\p@}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1. #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1. #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}

% To define a float of type TYPE (e.g., TYPE = figure), the document
% style must define the following.
%
%  \fps@TYPE   : The default placement specifier for floats of type
%                TYPE.
%
% \ftype@TYPE : The type number for floats of type TYPE.  Each TYPE
%                has associated a unique positive TYPE NUMBER, which
%                is a power of two.  E.g., figures might have type
%                number 1, tables type number 2, programs type number
%                4, etc.
%
% \ext@TYPE : The file extension indicating the file on which the
%                contents list for float type TYPE is stored.  For
%                example, \ext@figure = 'lof'.
%
%  \fnum@TYPE  : A macro to generate the figure number for a caption.
%                For example, \fnum@TYPE == Figure \thefigure.
%
%  The actual float-making environment commands--e.g., the commands
%  \figure and \endfigure--are defined in terms of the macros \@float
%  and \end@float, which are described below.
%
% \@float{TYPE}[PLACEMENT] : Macro to begin a float environment for a
%     single-column float of type TYPE with PLACEMENT as the placement
%     specifier.  The default value of PLACEMENT is defined by
%     \fps@TYPE.  The environment is ended by \end@float.  E.g.,
%     \figure == \@float{figure}, \endfigure == \end@float.

%
% FIGURE
%

\newcounter{figure}			% ABC Figure numbers should be sequential
\ifnum \@econstyle = 1
	\@addtoreset{figure}{chapter} % Makes \chapter reset 'figure' counter.
	\def\thefigure{\thechapter.\@arabic\c@figure} 
  \else	
    	\def\thefigure{\@arabic\c@figure} % ABC No chapter # in figure #
\fi
%\def\fps@figure{htp}        % ABC Changed from tbp to avoid placement 
			     % ABC below footnotes.
\def\fps@figure{t} 
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\def\figure{\@float{figure}}
\let\endfigure\end@float
\@namedef{figure*}{\@dblfloat{figure}}
\@namedef{endfigure*}{\end@dblfloat}

%
% TABLE
%

\newcounter{table}	      % ABC Table numbers should be sequential
\ifnum \@econstyle = 1
	\@addtoreset{table}{chapter} % Makes \chapter reset 'table' counter.
	\def\thetable{\thechapter.\@arabic\c@table}
  \else
	\def\thetable{\@arabic\c@table} % ABC No chapter number intable #
\fi

% ABC Table placement was originally {tbp}. First I changet it to
% ABC {htp}, but then decided to force placement at TOP of page
% ABC to be able to mess with separation between table and text.
% ABC The parameter to do that is \textfloatsep which affects 
% ABC separation between text and table at BOTH top and bottom. 
% ABC Therefore, if tables are only at top, can change separation
% ABC between table and following text, which was the original
% ABC problem.
\def\fps@table{t}            
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\def\table{\ssp\@float{table}} %ABC added ssp which I removed from \tabular
\let\endtable\end@float
\@namedef{table*}{\@dblfloat{table}}
\@namedef{endtable*}{\end@dblfloat}

%
% CAPTIONS
%
% ABC I lifted the following code straight out of latex.ltx. The reason
% ABC is that I needed to add a blank line after each entry in the 
% ABC lists of figures and tables. This is the only way I could figure 
% ABC out to deal with AU requirement to have list of tables
% ABC double spaced except when a given entry exceeds a line, in which case 
% ABC it should be single spaced.

\def\caption{\refstepcounter\@captype \@dblarg{\@caption\@captype}}
\long\def\@caption#1[#2]#3{%
  \par \par
  \addcontentsline{\csname ext@#1\endcsname}{#1}%
    {\protect\numberline{\csname the#1\endcsname.}{\ignorespaces #2}}%
  \addtocontents{\csname ext@#1\endcsname}{\protect\addvspace{10\p@}}%
  \begingroup
    \@parboxrestore
    \if@minipage
      \@setminipage
    \fi
    \normalsize
    \@makecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
  \endgroup}
  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% BEGIN: APPENDIX %%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AI: The following lines are used to define appendix and chapter.
%AI: chapter is defined in aucas.clo and auecon.clo
%ABC because the 'cas' and 'econ' options have slightly different
%ABC definitions.
\ifnum \@ptsize = 0 \def\chindent{-0.35in}\fi   % ABC for indentation of
\ifnum \@ptsize = 1 \def\chindent{-0.40in}\fi   % ABC CHAPTER and APPENDIX
\ifnum \@ptsize = 2 \def\chindent{-0.40in}\fi   % ABC lines.

% The \appendix command must do the following:
%    -- reset the chapter counter to zero
%    -- set \@chapapp to Appendix (for messages)
%    -- redefine the chapter counter to produce appendix numbers
%    -- reset the section counter to zero
%    -- redefine the \chapter command if appendix titles and headings
%       are to look different from chapter titles and headings.
%AI: moved here from .clo files; use most of def from report.cls

\newcommand\appendix{\par
  %chapters
  \setcounter{chapter}{0}%
  \renewcommand{\thechapter}{\@Alph\c@chapter}
  %sections
  \setcounter{section}{0}%
  \gdef\@chapapp{\appendixname}%
  \addtocontents{toc}{\hspace{\AIindent}{  }\par} 			%ABC
  %\addtocontents{toc}{\hspace{\chindent}{\appendixname}\par}     %AI:20020624
  %\addtocontents{toc}{\protect\addvspace{10\p@}}
  }		%ABC



%    ****************************************
%    *            PAGE STYLES               *
%    ****************************************
%
% The page style 'foo' is defined by defining the command \ps@foo.  This
% command should make only local definitions.  There should be no stray
% spaces in the definition, since they could lead to mysterious extra
% spaces in the output.
%
% The \ps@... command defines the macros \@oddhead, \@oddfoot,
% \@evenhead, and \@evenfoot to define the running heads and
% feet---e.g., \@oddhead is the macro to produce the contents of the
% heading box for odd-numbered pages.  It is called inside an \hbox of
% width \textwidth.
%
% To make headings determined by the sectioning commands, the page style
% defines the commands \chaptermark, \sectionmark, ... , where
% \chaptermark{TEXT} is called by \chapter to set a mark, and so on.
% The \...mark commands and the \...head macros are defined with the
% help of the following macros.  (All the \...mark commands should be
% initialized to no-ops.)
%
% MARKING CONVENTIONS:
% LaTeX extends the TeX \mark facility by producing two kinds of marks
% a 'left' and a 'right' mark, using the following commands:
%     \markboth{LEFT}{RIGHT} : Adds both marks.
%     \markright{RIGHT}      : Adds a 'right' mark.
%     \leftmark  : Used in the \@oddhead, \@oddfoot, \@evenhead or
%                  \@evenfoot macro, gets the current 'left' mark.
%                  Works like the TeX \botmark command.
%     \rightmark : Used in the \@oddhead, \@oddfoot, \@evenhead or
%                  \@evenfoot macro, gets the current 'right' mark.
%                  Works like the TeX \firstmark command.
% The marking commands work reasonably well for right marks 'numbered
% within' left marks--e.g., the left mark is changed by a \chapter
% command and the right mark is changed by a \section command.  However,
% it does produce somewhat anomalous results if two \bothmark occur on
% the same page.
%
%
% Commands like \tableofcontents that should set the marks in some
% page styles use a \@mkboth command, which is \let by the pagestyle
% command (\ps@...)  to \markboth for setting the heading or
% \@gobbletwo to do nothing.

\mark{{}{}}   % Initializes the TeX marks

% \ps@empty defined in 'latex.tex'

% Definition of 'headings' page style
%  Note the use of ##1 for parameter of \def\chaptermark inside the
%  \def\ps@headings.
%
% 91/03/26 FMi: Added extra set of braces arround |\sl| in |\@oddhead|
% to support NFSS (|\sl| is a shape |\rm| a family). Also remove
% unnecessary |\hbox{}| commands.
%
% 94/11/3 BBF: Eliminated two letter font selection commands \rm and \sl
% in favor of \rmfamily and \slshape.
%

\def\ps@headings{\let\@mkboth\@gobbletwo
\def\@oddfoot{}\def\@evenfoot{}%     No feet.
\def\@oddhead{{\slshape \rightmark}\hfil \rmfamily\thepage}% Heading.
\def\chaptermark##1{\markright {\uppercase{\ifnum \c@secnumdepth >\m@ne
  \@chapapp\ \thechapter. \ \fi ##1}}} }

%
% Definition of 'myheadings' page style.
%

\def\ps@myheadings{\let\@mkboth\@gobbletwo
 \def\@oddhead{{\slshape\rightmark}\hfil \rmfamily\thepage}%
 \def\@oddfoot{}\def\@evenhead{\rmfamily \thepage\hfil\slshape\leftmark}%
 \def\@evenfoot{}\def\chaptermark##1{}\def\sectionmark##1{}%
 \def\subsectionmark##1{}}

%
% Definition of 'plain' page style.
%
\def\ps@plain{\let\@mkboth\@gobbletwo
\def\@oddfoot{}\def\@evenfoot{}				% no feet
\def\@oddhead{\hbox{}\hfil\rmfamily\thepage}		% heading (right)
\def\@evenhead{\rmfamily\thepage\hfil\hbox{}}}		% heading (left)


%    ****************************************
%    *            MISCELLANEOUS             *
%    ****************************************
%
% DATE
%
\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}

% EQUATION and EQNARRAY -- must follow \chapter definition
% Make \chapter reset equation counter
\@addtoreset{equation}{chapter}
%AI: from report class:
\renewcommand\theequation
  {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@equation}




%    ****************************************
%    *           INITIALIZATION             *
%    ****************************************
%
% Default initializations

\ps@plain                   % 'plain' page style
\pagenumbering{arabic}      % Arabic page numbers
\dsp  %set text to double spacing

\endinput

