% Arrange to retrieve the title text of ``parts''. Unfortunately,
% pretocmd doesn't seem to work on \part or \@part
\let\oldpart\part
\renewcommand{\part}[1]{\oldpart{#1}\gdef\parttitle{#1}}

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

