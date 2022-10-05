% Arrange to retrieve the title text of ``parts''. Unfortunately,
% pretocmd doesn't seem to work on \part or \@part
\let\oldpart\part
\renewcommand{\part}[1]{\oldpart{#1}\gdef\parttitle{#1}}

% Arrange to reset chapter numbering at the start of each "part"
\makeatletter
\@addtoreset{chapter}{part}
\makeatother

% Setup page headers and footers
\makeatletter 
  \fancypagestyle{normal}{ 
    \fancyhf{} 
    \fancyfoot[LE,RO]{{\py@HeaderFamily\thepage}} 
    \fancyfoot[LO]{{\py@HeaderFamily\nouppercase{\rightmark}}} 
    \fancyfoot[RE]{{\py@HeaderFamily\nouppercase{\leftmark}}} 
    \fancyhead[LE]{{\py@HeaderFamily \@title\space[\version]}}
    \fancyhead[RO]{{\py@HeaderFamily \parttitle}}
    \renewcommand{\headrulewidth}{0.4pt} 
    \renewcommand{\footrulewidth}{0.4pt} 
  } 
\makeatother 

\setcounter{tocdepth}{2}

