% Straight up stealing preamble from Eli Holmes 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%START PREAMBLE THAT IS THE SAME FOR ALL EXAMPLES
\documentclass{article}

%Required: You must have these
\usepackage{Sweave}
\usepackage{graphicx}
\usepackage{tabularx}

%Strongly recommended
%put your figures in one place
%you will want these for pretty captioning
\usepackage[small]{caption}
\setkeys{Gin}{width=0.8\textwidth}  %make the figs 50 perc textwidth
\setlength{\captionmargin}{30pt}
\setlength{\abovecaptionskip}{0pt}
\setlength{\belowcaptionskip}{10pt}
% manual for caption  http://www.dd.chalmers.se/latex/Docs/PDF/caption.pdf

%Optional: I like to muck with my margins and spacing in ways that LaTeX frowns on
%Here is how to do that
\topmargin -1.5cm        
\oddsidemargin -0.04cm   
\evensidemargin -0.04cm  % same as oddsidemargin but for left-hand pages
\textwidth 16.59cm
\textheight 21.94cm 
%\pagestyle{empty}       % Uncomment if don not want page numbers
\parskip 7.2pt           % sets spacing between paragraphs
%\renewcommand{\baselinestretch}{1.5} 	% Uncomment for 1.5 spacing between lines
\parindent 0pt		  % sets leading space for paragraphs
\usepackage{setspace}
%\doublespacing

%Optional: I like fancy headers
\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhead[LO]{Wolkovich et al.
\fancyhead[RO]{2012}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END PREAMBLE THAT IS THE SAME FOR ALL EXAMPLES

%Start of the document
\begin{document}
\bibliographystyle{/Users/Cat Chamberlain/Documents/git/falsespring/resources.bib}

\title{False Spring: \\ \\ A comparison of false spring index calculations}
\author{C. Chamberlain}
%\date{\today}
\maketitle  %put the fancy title on

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand{\thetable}{S\arabic{table}}
\renewcommand{\thefigure}{S\arabic{figure}}
\renewcommand{\labelitemi}{$-$}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section\Large{Introduction}
\newline
Plants that grow in temperate environments are at risk of being exposed to late spring
freezes, which can be detrimental to plant growth. According to Gu et al. (2008), there 
are two phases involved in late spring freezing: rapid vegetative growth prior to the 
freeze and the post freeze setback. This combined process is known as a false spring. 
Freeze and thaw fluctuations can cause xylem embolism and decreased xylem conductivity 
which can result in crown dieback [@Gu2008]. More frequently, however, plants that have 
been exposed to a false spring will experience leaf loss and slower canopy development 
[@Hufkens2012]. With anthropogenic climate change, the severity of damage incurred from
a false spring phenomena is predicted to be heightened due to earlier spring onset and 
greater fluctuations in temperatures. It is anticipated that there will be a 
decrease in false spring occurrence overall, however, the severity of temperature 
variation is likely to increase [@Allstadt2015]. 
\newline
Different species exhibit varying responses to late spring freezing events and 
the level of damage also varies across phenophases. Generally, reproductive phases 
are more sensitive to false spring events than vegetative phases and developing 
leaves are more susceptible to damage than opening buds or expanding shoots 
[@Peterson2014]. False spring events also put seedling and sapling trees at 
greater risk to damage than adult trees [@Vitasse2014]. Warm temperatures earlier
in the year (i.e. in February) do not seem to affect species, most likely 
because it is too soon for bud burst to take place and sufficient chilling has 
not yet occurred. Frost damage usually occurs when there is a warmer than average
March, a freezing April, and enough growing days between the high temperatures and 
the last freeze date [@Augspurger2013]. In a study performed by Peterson and 
Abatzoglou (2014), it had been determined that 7 days between bud burst and last 
freeze date is a significant parameter. There is much debate over the definition 
of freezing temperatures and has resulted in two types of freezes: a "hard"
freeze at -2.2&deg;C and a "soft" freeze at -1.7&deg;C 
[@Augspurger2013; @Kodra2011; @Vavrus2006].

<<echo=FALSE>>=
options(width=90)
@

<<label=libraries, echo=FALSE, results=hide>>=
options(stringsAsFactors=FALSE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)

setwd("~/Documents/git/falsespring")
source("FinalFSI.R")

<<label=bbtable, results="asis", message=FALSE, echo=FALSE>>=

print(xtable::xtable(bb.table,caption = "Last Freeze julian dates recorded from 2008 to 2014 and day of bud burst recorded for all three methodologies."),comment=FALSE, caption.placement="top")
@



