%% -*- mode: LaTeX; compile-command: "./build.sh" -*-
\documentclass{amsart}

%include polycode.fmt

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Packages

\usepackage{hyperref}
\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{amssymb}
\usepackage{stmaryrd}
\usepackage[all]{xy}
\usepackage{prettyref}
\usepackage{mdframed}
\usepackage{todonotes}
\usepackage{xspace}
\usepackage{url}
\usepackage{xcolor}
\usepackage{soul}
\usepackage{caption}

\usepackage[backend=pgf,extension=pgf,input,outputdir=diagrams]{diagrams-latex}

\usepackage{textgreek}
\usepackage{latex/agda}
\usepackage{catchfilebetweentags}

\usepackage{ucs}
\usepackage[utf8, utf8x]{inputenc}
\usepackage{autofe}

\usepackage[authoryear]{natbib}
\usepackage{bibentry}
\nobibliography*

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Theorem-like environments

\newtheorem{thm}{Theorem}
\newtheorem{prop}{Proposition}
\newtheorem{lem}{Lemma}
\newtheorem{cor}{Corollary}
% \newtheorem{conj}[thm]{Conjecture}

\theoremstyle{definition}

\newtheorem{defn}{Definition}
\newtheorem{ex}{Example}

\theoremstyle{remark}
\newtheorem*{rem}{Remark}
\newtheorem*{nota}{Notation}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commentary environment

\newmdenv[skipabove=1em, skipbelow=1em, innermargin=1.5em, outermargin=1.5em, backgroundcolor=black!8, linecolor=black!10]{commentary}

%% XXX use a different font for commentary?  Or a different font for
%% original paper?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Translation

\colorlet{lightpurple}{purple!15}
\sethlcolor{lightpurple}
\newcommand{\trans}[2]{\hl{#1 [\textit{#2}]}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Todo notes

\presetkeys{todonotes}{backgroundcolor=yellow!20!white,inline}{}

\newcommand\todoin[2][]{\todo[inline, caption={todo}, #1]{\begin{minipage}{\textwidth-4pt}#2\end{minipage}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Math formatting

\newcommand{\B}{\mathbb{B}}
\newcommand{\E}{\mathbb{E}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\N}{\mathbb{N}}
\DeclareMathOperator{\el}{el}
\DeclareMathOperator{\Res}{Res}

\newcommand{\bij}{\stackrel{\sim}{\rightarrow}}
\newcommand{\inj}{\hookrightarrow}
\newcommand{\comp}{\circ}
\newcommand{\id}{\mathit{id}}

\DeclareMathOperator{\Card}{Card}
\DeclareMathOperator{\Aut}{Aut}
\DeclareMathOperator{\aut}{aut}
\DeclareMathOperator{\Mon}{Mon}
\newcommand{\unl}[1]{\tilde{#1}}
\newcommand{\Simp}{\mathcal{S}}
\newcommand{\Poly}[2]{#1 \llbracket #2 \rrbracket}
\newcommand{\MPoly}[2]{#1 \{\!\{ #2 \}\!\} }

\newcommand{\xx}{\ensuremath{\mathbf{x}}}

\newcommand{\term}[1]{\emph{#1}}
\newcommand{\ie}{\term{i.e.}\xspace}

\newcommand{\union}{\cup}
\newcommand{\intersect}{\cap}

\newcommand{\pointed}[1]{#1^{\bullet}}
\newcommand{\Gc}{\pointed{G_c}}
\newcommand{\Gcc}{\pointed{G_{cc}}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prettyref

\newcommand{\Sect}{\S}

\newrefformat{fig}{Figure~\ref{#1}}
\newrefformat{chap}{Chapter~\ref{#1}}
\newrefformat{sec}{\Sect\ref{#1}}
\newrefformat{eq}{equation~\eqref{#1}}
\newrefformat{prob}{Problem~\ref{#1}}
\newrefformat{tab}{Table~\ref{#1}}
\newrefformat{thm}{Theorem~\ref{#1}}
\newrefformat{lem}{Lemma~\ref{#1}}
\newrefformat{prop}{Proposition~\ref{#1}}
\newrefformat{defn}{Definition~\ref{#1}}
\newrefformat{cor}{Corollary~\ref{#1}}
\newrefformat{conj}{Conjecture~\ref{#1}}
\newrefformat{ex}{Example~\ref{#1}}

\newcommand{\pref}[1]{\prettyref{#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Haskell formatting

%format :+: = "\mathbin{:\!\!+\!\!:}"
%format :*: = "\mathbin{:\!\!*\!\!:}"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Agda formatting

%format Nat = "\mathbb{N}"

\newcommand{\AD}{\AgdaDatatype}
\newcommand{\AF}{\AgdaFunction}

\makeatletter
\newcommand{\dotminus}{\mathbin{\text{\@@dotminus}}}

\newcommand{\@@dotminus}{%
  \ooalign{\hidewidth\raise1ex\hbox{.}\hidewidth\cr$\m@@th-$\cr}%
}
\makeatother

\DeclareUnicodeCharacter{2238}{\ensuremath{\dotminus}}
\DeclareUnicodeCharacter{2115}{\ensuremath{\mathbb{N}}}
\DeclareUnicodeCharacter{2192}{\ensuremath{\to}}
\DeclareUnicodeCharacter{2295}{\ensuremath{\oplus}}
\DeclareUnicodeCharacter{2299}{\ensuremath{\odot}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other formatting

\setcounter{figure}{-1}

\newcommand{\etc}{\textit{etc.}\xspace}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{center}
  {\textbf{A COMBINATORIAL THEORY OF FORMAL SERIES}}
  \bigskip

  {\small ANDR\'E JOYAL} \bigskip

  {\small Translation and commentary by \\
    BRENT A. YORGEY}
  \vspace{0.5in}

  {\small \textbf{This is an unofficial translation of an article
       that appeared in an Elsevier publication. Elsevier has not
       endorsed this translation.} \medskip

     See \url{https://www.elsevier.com/about/our-business/policies/open-access-licenses/elsevier-user-license}.
  } \bigskip

  {\small \bibentry{joyal1981theorie}} \vspace{0.5in}

\end{center}

\section*{Preface}

In his classic 1981 paper \emph{Une th\'eorie combinatoire des
  s\'eries formelles (A combinatorial theory of formal series)}, Joyal
introduces the notion of \term{(combinatorial) species}, which has had
a wide-ranging influence on combinatorics.  During the course of
researching my PhD thesis on the intersection of combinatorial species
and programming languages, I read a lot of secondary literature on
species, but not Joyal's original paper---it is written in French,
which I do not read. I didn't think this would be a great loss, since
I supposed the material in his paper would be well-covered elsewhere,
for example in the textbook by \citet{bll}, which thankfully
\emph{has} been translated into English. However, I eventually came
across some questions to which I could not find answers, only to be
told that the answers were already in Joyal's paper
\citep{trimble-not-analytic}. Somewhat reluctantly, I found a copy and
began trying to read it, whereupon I discovered two surprising things.

First, armed with a dictionary and Google Translate, reading
mathematical French is not too difficult---even for someone who does
not know any French!---though it certainly helps if one already
understands the mathematics.  Second, it turns out that Joyal's paper
makes for excellent reading, and is full of insights and examples
which, as far as I know, do not appear in any of the secondary
literature.  The paper, and the theory of species more generally, has
a lot to offer to computer science, and to functional programming in
particular.

My initial joy at being able to read the paper was quickly tempered,
however, by the knowledge that there are still formidable barriers
preventing it from being more widely accessible within the functional
programming community. The language barrier is the obvious one, for
those who cannot read French; even armed with Google translate it is
still very slow going.  The second, more serious barrier is that the
paper assumes background knowledge---in mathematics in general, and
combinatorics in particular---which many in the programming languages
community may not have (even those who can read French).

I have therefore decided to produce an English translation,
accompanied by my own additional commentary, examples, and code which
attempt to explain and illuminate the content to a wider audience.
The commentary is typeset using inset boxes with a grey background,
like this:
\begin{commentary}
  This is some commentary.
\end{commentary}

\todo{Say something about language used for code examples, and how it
  fits/what background you need}

\todo{Note \url{https://agda.readthedocs.io/en/v2.5.3/tools/generating-latex.html}}

This is a long-term project; I do not know how long it will take to
finish, but plan to work on it slowly and steadily.  Collaboration and
contributions are welcome---see the public git repository hosted at
\url{https://github.com/byorgey/series-formelles}. Things I could
particularly use help with include:
\begin{itemize}
\item Translation.  If you know both French and English well, I would
  love to have your eyes on the translation.  There are particular
  places where I am not sure about the translation, which I have
  indicated \trans{like this}{comme \c{c}a}.  However, just because I
  am sure about other parts does not mean I am right! All of the
  translation would benefit from checking for accuracy and style.
\item Commentary. I would of course be happy to receive contributions
  of additional commentary, especially from those who are more deeply
  versed in the relevant mathematics or history than I am, or from
  those with a different perspective to offer.
\item Feedback. Even (especially!) if you are not an expert, an
  incredibly helpful way you can contribute is simply to try reading
  the translation and commentary, and let me know of any parts you
  find confusing or unclear---a signal that the commentary needs to be
  clarified, or more commentary added.
\item I am also interested to eventually publish this somehow---if you
  represent an interested publisher, or have a recommendation of a
  suitable one, please let me know!
\end{itemize}


\subsection*{Technical details}
\label{sec:tech-details}

The original paper is available only in scanned form, so as a first
step I ran it through the \texttt{tesseract} OCR
engine.\footnote{\url{https://github.com/tesseract-ocr/tesseract}}
\texttt{tesseract} does an excellent job with the text, although
naturally it completely mangles the diagrams and equations.  From
there I have loaded the resulting text document into the Google
Translator Toolkit, and use that to aid in the translating process,
also typing up equations as I go.  The result gets pasted into a
\LaTeX\ document, after which I proofread and add diagrams
(all produced using the \texttt{diagrams} vector graphics
framework\footnote{\url{http://projects.haskell.org/diagrams/}}) and
commentary.

A git repository containing the latest state of this project can be
found at \begin{quote}\url{https://github.com/byorgey/series-formelles}.\end{quote}
A PDF compiled from the most recent source (automatically updated
every time I record a commit) can be downloaded from \url{http://ozark.hendrix.edu/~yorgey/pub/series-formelles.pdf}.

\newpage

\section*{Background}
\label{sec:background}

The informal notion of a \term{combinatorial class} or
\term{combinatorial family} had already been around for quite a while
before Joyal's paper.  The basic idea is of a set of ``shapes'' or
``structures'' containing some sort of ``atoms''.
(Functional programmers should think of an algebraic data type with a
type parameter, although this does not capture the full sense.)  As an
example, the combinatorial class of \term{rooted binary trees}
consists of things like this:
  \begin{center}
  \begin{diagram}[width=150]
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams
import           Diagrams.Prelude hiding (Empty)

bnode = BNode ()
leafU = leaf ()

s1 :: BTree ()
s1 = bnode (bnode (bnode leafU leafU) Empty) (bnode leafU (bnode Empty leafU))

s2 :: BTree ()
s2 = bnode Empty leafU

s3 :: BTree ()
s3 = bnode Empty (bnode Empty (bnode (bnode Empty (bnode leafU leafU)) leafU))

dia :: Diagram B
dia = [s1, s3, s2]
  # map (drawBinTreeWide . fmap (const (circle labR # fc black)))
  # hsep 1
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \end{center}
  To start, we will suppose that the ``atoms''---indicated by dots in
  the diagram above---are indistinguishable (there will be much more
  to say about this later!).

Combinatorial classes can be combined in various algebraic ways.
Two of the most prominent and familiar are sum
(disjoint union) and product (pairing):
\begin{itemize}
\item The \term{sum} $F + G$ of two classes $F$ and $G$ consists of
  the disjoint union of the classes.  In other words, an $F+G$
  structure consists of \emph{either} an $F$ structure or a $G$
  structure (along with a tag saying which).  This corresponds to a
  sum type, for example, |Either F G| in Haskell.
\item The \term{product} $F \cdot G$ of two classes $F$ and $G$
  consists of the class of all \term{ordered pairs} of structures from
  the two classes, that is,
  \[ F \cdot G = \{ (f,g) \mid f \in F, g \in G \}. \] This
  corresponds to a product or pair type, for example, |(F,G)| in
  Haskell.
\end{itemize}

Calling these operations \term{sum} and \term{product} can be
initially justified by thinking about \emph{finite} combinatorial
classes.  In the case that $F$ and $G$ are finite, one can verify that
\begin{align*}
  ||F + G|| &= ||F|| + ||G|| \qquad \text{and} \\
  ||F \cdot G|| &= ||F|| \cdot ||G||,
\end{align*}
where we use $||F||$ to denote the size (that is, the number of
structures) of a finite combinatorial class.  That is, the size of a
disjoint union $F+G$ is the sum of the sizes $F$ and of $G$, and the
size of a product $F \cdot G$ is the product of the sizes.  Describing
a combinatorial class algebraically (in terms of sums and products of
more primitive classes) gives us a corresponding algebraic handle on
its size; this is one of the key ideas of combinatorics.

However, considering only finite combinatorial classes is much too
restrictive.  For example, we have previously seen the class of binary
trees, which is certainly not finite.  How can we continue to do
meaningful combinatorics with infinite classes?  Combinatorics has
\emph{counting} at its heart, after all, but counting to infinity is
not very informative (nor much fun).

The simple resolution comes from a slight change in perspective:
although there are infinitely many binary trees, there are only a
finite number \emph{of any given size}, where the \term{size} of a
structure is defined (informally, for now) as the number of atoms it
contains.  For example, \pref{fig:trees-by-size} shows all binary
trees with up to 4 atoms, grouped by size.
\begin{figure}
  \centering
  \begin{diagram}[width=200]
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams
import           Diagrams.Prelude hiding (Empty)

allTrees :: Int -> [BTree ()]
allTrees 0 = [Empty]
allTrees n = [0 .. n-1] >>= \k -> BNode () <$> allTrees k <*> allTrees (n-k-1)  -- $

dia :: Diagram B
dia = [0 .. 4]
  # map allTrees
  # map (map (drawBinTreeWide . fmap (const (circle labR # fc black))))
  # map (alignL . hsep 1)
  # vsep 2
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \caption{All nonempty binary trees with up to 4 atoms}
  \label{fig:trees-by-size}
\end{figure}
So it makes good sense to count binary trees if we focus on counting
how many there are of each size.  If $T$ represents the set of all
binary trees we will write $T_n$ to denote the set of binary trees of
size $n$, and $||T_n||$ its cardinality.

Combinatorial classes with this property---having only finitely many
structures of each given size---are called \term{finitary}.  The
cardinality of a finite set is just a single natural number;
generalizing to finitary combinatorial classes, we can say that the
``cardinality'' of a finitary class is a (countably) infinite sequence
of natural numbers \[ ||F_0||, ||F_1||, ||F_2||, \dots, \] describing
the number of structures of each size.  Famously, the generalized
cardinality of the class of binary trees is the sequence of
\term{Catalan numbers} \citep{stanley2015catalan}, which begins
\[ 1, 1, 2, 5, 14, 42, 132 \dots \] (the rows of
\pref{fig:trees-by-size} show the $1$, $2$, $5$, and $14$ trees of
sizes $1$--$4$).

Let's now reconsider the combinatorial operations of sum and product,
and see what operations they correspond to on generalized
cardinalities of finitary classes.
\begin{itemize}
\item Since an $F + G$ structure is either an $F$ structure or a $G$
  structure, the number of $F+G$ structures of size $n$ is just the
  sum of the number of $F$ structures of size $n$ and the number of
  $G$ structures of size $n$.  That is, \[  ||(F+G)_n|| = ||F_n|| +
    ||G_n||. \]

\item What about the number of structures of $F \cdot G$ of size
  $n$?  This turns out to be a bit more interesting.  An $F \cdot G$
  structure is a pair of an $F$ structure and a $G$ structure, whose
  total size is the sum of the sizes of the component structures.
  Thus, to get a structure of size $n$, we need to pair an $F$
  structure of size $k$ and a $G$ structure of size $n-k$.  For each
  $k$, the number of ways to pick an appropriate pair is the product
  of the number of choices for an $F$ structure of size $k$ and the
  number of choices for a $G$ structure of size $n-k$.  That is,
  \[ ||(F \cdot G)_n|| = \sum_{0 \leq k \leq n} ||F_k|| ||G_{n-k}||. \]
\end{itemize}

These generalized cardinalities lead directly to the theory of
\term{(ordinary) generating functions}
\citep{wilf2005generatingfunctionology}.  The observation is that we
can encode sequences $||F_0||, ||F_1||, ||F_2||, \dots$ as the
coefficients of infinite power series,
\[ ||F_0|| + ||F_1|| x + ||F_2|| x^2 + \dots = \sum_{n \geq 0} ||F_n||
  x^n. \] This is much more than a gimmick, because sum and product of
power series corresponds exactly to sum and product of combinatorial
classes:
\begin{itemize}
\item To add two power series, one adds coefficients of like powers,
  that is,
  \[ \left(\sum_{n \geq 0} ||F_n|| x^n \right) + \left(\sum_{n \geq 0}
      ||G_n|| x^n \right) = \sum_{n \geq 0} \left(||F_n|| + ||G_n|| \right)
    x^n. \] On the right-hand side we get $||F_n|| + ||G_n||$ as the
  coefficient of $x^n$, which, as we have previously seen, is in fact
  the number of $F + G$ structures of size $n$.  So adding the
  generating functions for $F$ and $G$ yields the generating function
  for $F+G$.
\item Now consider multiplying two power series.  Each term in the
  output will be the product of two terms, one from each input.
  Powers add when multiplying, so each $x^n$ term in the output will
  arise from the product of some $x^k$ term and some $x^{n-k}$ term.
  In particular, multiplying $ax^k$ and $bx^{n-k}$ results in $abx^n$.
  Once we collect up like terms in the result, the coefficient of
  $x^n$ will therefore be the sum of the products of coefficients of
  every possible pair of terms whose powers add up to $n$.  Symbolically:
  \[ \left(\sum_{n \geq 0} ||F_n|| x^n \right)
    \left(\sum_{n \geq 0} ||G_n|| x^n \right) = \sum_{n \geq 0}
    \left(\sum_{0 \leq k \leq n} ||F_k|| ||G_{n-k}||\right) x^n. \]
  Once again, we see that the coefficient of $x^n$ in the result is
  exactly the expression which we previously argued counts the number
  of $F \cdot G$ structures of size $n$.  Therefore, multiplying the
  generating functions for $F$ and $G$ yields the generating function
  for $F \cdot G$.
\end{itemize}

To make this more concrete, consider the following Agda
code \citep{norell2007towards} which implements these ideas.  We
encode the coefficients of a generating function not as a literally
infinite sequence, but as a function $\N \to \N$, which takes a
natural number $n$ as input and ouputs the coefficient of $x^n$, that
is, the number of structures of size $n$. Computationally, this is a
nicer representation in many ways, and also turns out to be the proper
perspective from which to later generalize to the notion of species.

In Agda, we can define the type of ordinary generating functions \AD{OGF}
literally as the function space $\N \to \N$:

\ExecuteMetaData[latex/SeriesFormelles.tex]{OGF}

We now encode a few primitive generating functions which will come in
handy: the constantly zero generating function $f(x) = 0$ has all
coefficients zero; the constantly $1$ generating function
$f(x) = 1 = 1 + 0x + 0x^2 + \dots$ has an $x^0$ coefficient of
$1$ and all the rest zero; and finally $f(x) = x$ has only a $1$
coefficient for $x^1$.

\ExecuteMetaData[latex/SeriesFormelles.tex]{PrimOGF}

Next we can define the operations of sum and product for generating
functions, according to the discussion above.  Generating function sum
just adds corresponding coefficients:

\ExecuteMetaData[latex/SeriesFormelles.tex]{SumOGF}

We can define generating function product using the standard library
function \AF{applyUpTo}, which applies the given function to each
natural number in the list $[0, 1, \dots, n]$.

\ExecuteMetaData[latex/SeriesFormelles.tex]{ProdOGF}

\todo{somewhere mention terminology ``labelled'' vs ``unlabelled''}

\todo{example application illustrating the power of the generating
  function approach?}

So far, we have focused on combinatorial classes of structures with
indistinguishable atoms.  What about structures with
\emph{distinguishable} atoms?  Taking the sum of two combinatorial
classes with distinguishable atoms is straightforward: once again, the
number of $F + G$ structures of size $n$ is just the sum of the number
of $F$ structures of size $n$ and $G$ structures of size $n$.

Product, on the other hand, is more interesting.  As a concrete
example, consider the combinatorial class $\mathcal{C}$ of
\emph{cycles}, illustrated below.

\todo{illustration}

There is only one cycle structure with $n$ indistinguishable atoms,
but with distinguishable atoms there are $(n-1)!$ distinct cycle
structures (there are $n!$ distinct sequences of $n$ atoms, but this
counts each cycle $n$ times, once for each of the $n$ positions at
which we could ``cut open'' a cycle to make it into a sequence).

The product $\mathcal{C} \cdot \mathcal{C}$ consists of \emph{ordered
  pairs} of cycles.  How many different pairs of cycles are there of a
given size?  The reason the atoms make a difference is that when
forming a pair of cycles with some set of atoms, it matters how we
distribute the atoms between the two cycles---this doesn't matter if
the atoms are indistinguishable.  For example, \todo{show example}

To count the number of $\mathcal{C} \cdot \mathcal{C}$ structures of
size $n$, we can imagine the choices we could make to pick one
particular such structure as follows:

\begin{itemize}
\item We must first choose some $k \in \{1, 2, \dots, n-1\}$; to get a
  structure of size $n$ overall we must pair a $k$-cycle with an
  $(n-k)$-cycle (note there are no size-$0$ cycles which explains why
  we do not choose $k = 0$ or $n$).
\item Next, we decide how to partition the $n$ atoms between the two
  cycles.  There are $\binom n k$ ways to choose $k$ of the $n$ atoms
  to go in the $k$-cycle.
\item Finally, we pick any of the $(k-1)!$ available $k$-cycles on the
  $k$ chosen atoms, and likewise we pick one of the $(n-k-1)!$ cycles
  on the remaining atoms.
\end{itemize}
All told, then, the number of pairs of cycles on $n$ distinguishable
atoms is \[ \sum_{1 \leq k \leq n-1} \binom n k (k-1)!(n-k-1)!. \]

Generalizing, we can see that for arbitrary combinatorial classes $F$
and $G$, the number of $F \cdot G$ structures on $n$ distinguishable
atoms is \[ ||(F \cdot G)_n|| = \sum_{0 \leq k \leq n} \binom n k
  ||F_k|| ||G_{n-k}||, \] assuming that $F_n$ is the set of $F$
structures on $n$ distinguishable atoms, and similarly for $G_n$.
This is the same as the formula for indistinguishable atoms, except
for the extra factor of $\binom n k$.

It turns out counting structures with distinguishable atoms is
captured by a different kind of generating function.  In particular,
we define the \term{exponential generating function} (egf) by
\[ \sum_{n \geq 0} ||F_n|| \frac{x^n}{n!} \] The $n!$ may seem like a
rabbit out of a hat at this point, but hopefully it is at least
plausible: it corresponds to the $n!$ different ways a set of $n$
distinguishable atoms can be permuted.

Let's check that multiplying two such egf's yields the egf for the
product.  Once again, an $x^n$ term in the result will come from the
product of an $x^k$ term from the first egf (with a coefficient of
$||F_k||/k!$) and an $x^{n-k}$ term from the second (with a
coefficient of $||G_{n-k}||/(n-k)!$).  The trick is that to make the
result into another egf, we must massage each term into the form of
some coefficient times $(x^n/n!)$:
\begin{align*}
  \left( \sum_{n \geq 0} ||F_n|| \frac{x^n}{n!} \right)   \left(
  \sum_{n \geq 0} ||G_n|| \frac{x^n}{n!} \right)
  &= \sum_{n \geq 0} \left( \sum_{0 \leq k \leq n} \frac{||F_k||}{k!}
    \frac{||G_{n-k}||}{(n-k)!} x^n\right) \\
  &= \sum_{n \geq 0} \left( \sum_{0 \leq k \leq n} \frac{||F_k||}{k!}
    \frac{||G_{n-k}||}{(n-k)!} \frac{n!}{n!} x^n\right) \\
  &= \sum_{n \geq 0} \left( \sum_{0 \leq k \leq n}
    \frac{n!}{k!(n-k)!}||F_k|| ||G_{n-k}||\right) \frac{x^n}{n!} \\
  &= \sum_{n \geq 0} \left( \sum_{0 \leq k \leq n}
    \binom{n}{k} ||F_k|| ||G_{n-k}||\right) \frac{x^n}{n!}
\end{align*}
Once again, we see that the coefficient of $x^n/n!$ is exactly the
same as the formula we already derived for the number of $(F \cdot G)$
structures with $n$ distinguishable atoms; the binomial coefficient
$\binom n k$ falls out of the extra $n!$ in the denominator of the egf
terms.

\subsection*{Basic group theory}

\todo{explain what algebra background is necessary.}

\subsection*{Basic category theory}

I assume that the reader is already familiar with the basic
definitions of category theory: categories, functors, and ideally
natural transformations (though you may be able to get away with
learning a bit about natural transformations from this document).

The concept of a \emph{groupoid} comes up quite a bit, and in
particular the groupoid $\B$.  A \emph{groupoid} is a category where
all the morphisms are ``invertible'', that is, each $m : A \to B$ has
a corresponding $m^{-1} : B \to A$ such that $m^{-1} \comp m = \id_A$
and $m \comp m^{-1} = \id_B$.  $\B$ is the category whose objects are
\emph{finite} sets and whose morphisms are bijections, that is,
functions which are both injective and surjective.  Since bijections
are invertible, $\B$ is not just a category but a groupoid.

It is always possible to make a bijection between any two finite sets
of the same size; conversely, there are no bijections between finite
sets of different sizes.  Thus, $\B$ can be thought of as the disjoint
union of a number of connected components, one for each natural number
size.

A good check of your understanding of the necessary basic category
theory is to prove that functors preserve isomorphisms: that is, if a
morphism $m : A \to B$ in the category $\mathbb{C}$ is invertible,
then any functor $F : \mathbb{C} \to \mathbb{D}$ must necessarily send
$m$ to an invertible morphism in $\mathbb{D}$.

\subsection*{Categorification}

The process of \emph{categorification} cannot be defined rigorously.
It attempts to take mathematical objects and find a way to see them as
``shadows'' of objects in some richer category, in such a way that
operations and theorems we care about are also ``shadows'' of
(richer/more complex/more informative) operations and theorems on the
category.  As we will see in the case of species, this approach often
yields great insight into the original class of objects.

\todo{Look up/recall how Baez talks about categorification.  I looked
  it up and realized that it is specifically about moving from sets to
  categories.}

As a (particularly germane) example, consider the set of natural
numbers $\N = \{0, 1, 2, \dots\}$, ordered by the usual $\leq$
relation, along with the usual binary operations of addition,
multiplication, and exponentiation.  Our goal is to find a category
such that
\begin{itemize}
\item the natural numbers can be seen as ``shadows'' of the objects of
  the category;
\item the $\leq$ relation can be seen as the shadow of morphisms in
  the category; and
\item addition, multiplication, and exponentiation are shadows of
  suitable categorical constructions.
\end{itemize}

One category that fits the bill is the category whose objects are
\emph{finite sets} with morphisms being arbitrary (total)
\emph{functions} between sets.  \todo{XXX injection vs arbitrary
  function?  Baez \& Dolan mention FinSet with finite sets and functions.}
\begin{itemize}
\item Each natural number $n$ can be seen as the ``shadow'' of all the
  finite sets having cardinality $n$.  The natural number $n$ should
  be thought of as a sort of ``degenerate finite set'' where we have
  forgotten the identity of the set's elements, and remember only its
  size.
\item There is an injection $S \inj T$ if and only if $||S|| \leq
  ||T||$, so the ``shadow'' of a morphism is indeed a $\leq$ relation
  between the cardinalities of the sets.  Put another way, if we have
  an injection $S \inj T$ but then forget the identities of the
  elements of $S$ and $T$, the only thing we can remember about the
  injection is the fact that the cardinality of $S$ must be at most the
  cardinality of $T$.
\item Addition of natural numbers is the shadow of coproducts
  (disjoint unions) of sets.  That is,
\end{itemize}

\todo{Start with arithmetic: natural numbers with addition,
  multiplication, exponentiation.  Turn natural numbers into finite
  sets with functions, get a category with coproducts, products,
  function spaces.  Usual arithmetic laws e.g. $x*(y+z) = x*y + x*z$,
  $x^(y+z) = x^y * x^z$, etc. all turn into theorems expressing
  isomorphisms bewteen sets.  If you take this process and lift it to
  act on generating functions, you get species.}

\subsection*{Contributions}

\mbox{}
\todo{Joyal's paper turns GFs into combinatorial objects in their own
  right via categorification.}

\todoin{  \begin{itemize}
  \item First to apply CT to combinatorics
  \item Unified and generalized known collection of GF techniques
  \item Applied theory to novel results, and very concise derivations
    of celebrated results (Cayley, Lagrange inversion)
  \end{itemize}
}


\newpage

\title{A combinatorial theory of formal series}
\author{Andr\'e Joyal}
\address{Department of Mathematics, University of Quebec Montreal \\
Montreal, Quebec H30 3P8, Canada}
\translator{Brent A. Yorgey}
\address{Department of Mathematics and Computer Science \\ Hendrix
  College \\ 1600 Washington Ave \\ Conway, Arkansas, 72032 \\ United
  States of America}

\begin{abstract}
  This paper presents a combinatorial theory of formal power
  series. The combinatorial interpretation of formal power series is
  based on the concept of species of structures. A categorical
  approach is used to formulate it. A new proof of Cayley's formula
  for the number of labelled trees is given as well as a new
  combinatorial proof (due to G. Labelle) of Lagrange's inversion
  formula. Polya's enumeration theory of isomorphism classes of
  structures is entirely renewed.  Recursive methods for computing
  cycle index polynomials are described. A combinatorial version of
  the implicit function theorem is stated and proved. The paper ends
  with general considerations on the use of coalgebras in
  combinatorics.
\end{abstract}

\maketitle

\section*{Introduction}
\label{sec:introduction}

The aim of this work is simultaneously to explain, clarify, and unify
the subject.  The usefulness of formal series in combinatorial
calculations is well established.  The combinatorial interpretation of
the substitution operation has been the subject of relatively recent
work (\citet{bender1971enumerative, doubilet1975generating,
  foata1970theorie, garsia1976lecture, gessel1977generating}). The
first interpretation (probabilistic) of the substitution of power
series dates back to Watson \citep{kendall1966branching} (in the
theory of branching processes).

The main feature of the theory presented here is its degree of
generality and simplicity. In this theory, the combinatorial objects
corresponding to formal series are the species of structures. The
emphasis is placed on the transport of structures rather than on their
properties. This point of view is reminiscent of that of
\citet{ehresmann1965categories} and contrasts with that of
\citet{bourbaki1968elements}. The combinatorial operations on formal
series correspond to operations on species of structures. Algebraic
identities between formal expressions often correspond to
combinatorial identities.  Intuition and calculation can then play on
two fronts in a dialogue that resembles that between algebra and
geometry. The result is a kind of combinatorial algebra analogous to
the geometric algebra of Grassman (and Leibniz). The simplicity of the
theory is largely due to the use it makes of the concepts of category
theory \citep{mac1971categories} (previous theories mainly use the
theory of ordered sets and partitions). Furthermore, it highlights the
fundamental fact that a very large number of constructed bijections
are \emph{natural}, that is to say, they do not depend on a system of
coordinates introduced by means of an arbitrary enumeration.

The work contains a few combinatorial innovations, like the concept of
the \term{vertebrate} and a new proof of Cayley's result on the number
of trees. There is also a new combinatorial proof of Lagrange's
inversion theorem. Polya theory is entirely redone and results in a
method for calculating by \emph{recurrence} the coefficients of cycle
index polynomials.

The theory presented here is partial. Several fundamental aspects have
not been addressed. For example, there is a very general categorical
theory of the substitution operation \citep{kelly1974clubs}. Further
developments probably require such a level of generality. We limit
ourselves to those aspects most suitable (in the opinion of the
author) for capturing the attention of a reader unfamiliar with the
concepts of category theory.

I am especially grateful to G. Labelle for expressing interest in the
author's questions, and to whom I owe the proof of Lagrange's
inversion theorem presented here. I am also indebted to J. Beck, F. W.
Lawvere, P. Leroux, J. Labelle and S. Schanuel for stimulating
conversations on the possible relationship between combinatorics and
category theory. I thank G. C. Rota for encouraging me to write this
work. I also thank Cathy Kicinski for her work of typing.  Finally,
this work, written in the sunny Australian spring, would never have
been possible without the hospitality of Max Kelly of the University
of Sydney.

\section{Species of Structures}
\label{sec:species-of-structures}

There already exists a precise concept of species of structures
\citep[Chap.\ 4]{bourbaki1968elements}.
\begin{commentary}
  Bourbaki's definition of species of structure is really quite a
  horrible mess.  If one squints at it, one can make out the general
  idea; Joyal's definition really encapsulates all of its essence in a
  much more elegant and economical way.  This seems to be due in large
  part to Bourbaki's insistence on using formal set theory as a
  foundation; as we will see, Joyal's definition benefits tremendously
  from using category theory instead.  Interested and/or masochistic
  readers can read Bourbaki's definition in
  Appendix~\ref{sec:appendix-bourbaki}.
\end{commentary}
Describing a particular species is often done by specifying the
conditions which a structure must satisfy to belong to the species.
This description may take the form of an axiomatic theory of the
species being considered. A key part of the concept is the transport
of structures.  We will abstract the concept of species so that the
transport of structures is the main aspect. Moreover, as we only deal
with the problems of counting and finite enumeration, we will confine
ourselves to finitary species, unless otherwise noted.

\begin{commentary}
  See \citet[pp. 6--7]{bll} for some examples of ``axiomatic
  theories'' for particular species. The main point here is
  \emph{transport} of species, that is, the ability to ``swap out''
  the labels in a structure for some other labels.  Joyal's insight is
  to make transport itself the central defining feature of species;
  there will be much more to say on this later.
\end{commentary}

\subsection{Species and cardinality}
\label{sec:species-and-cardinality}

\begin{defn}
  A (finitary) species is an endofunctor $M: \B \to \B$ on the
  groupoid $\B$ of finite sets and bijections.
\end{defn}

\begin{commentary}
  It's worth pointing out that most subsequent publications (even by
  Joyal himself) actually define species as functors $\B \to \E$,
  where $\E$ is the category of (finite) sets and total functions,
  rather than bijections \citep{joyal86, bll}.  In fact, Joyal
  introduces functors $\B \to \E$ later in this very paper, in
  \pref{sec:category-of-species}.  It may not seem to make much
  difference when considering an individual species, but the
  \emph{category} of species ends up being much richer if we define
  species as functors $\B \to \E$.  For more details, see the
  commentary on \pref{sec:category-of-species}.
\end{commentary}

If $E$ is a finite set, $M [E]$ is the set of all \term{structures} of
the species $M$ on $E$. We say that $E$ is the \term{underlying} set
of $s \in M[E]$, or also that it is \term{supported} by $E$. We say
also, in an abuse of language, that $s$ is an \term{element} of $M$
($s \in M$) and that it is an \term{$M$-structure}.

\begin{commentary}
  Recall that a functor has two components, a mapping from objects to
  objects and a mapping from morphisms to morphisms.  This first
  paragraph after the definition sets up some terminology related to
  the object mapping, which sends finite sets (that is, objects of
  $\B$) to finite sets.  In particular, one should think of $M$ as
  sending finite sets of \emph{labels} to finite sets of
  \emph{labelled structures}.

  For example, consider the functor $L : \B \to \B$ which sends each
  finite set of labels to the set of all linear orderings on the
  labels, as illustrated below for a particular set of labels:
    \begin{center}
    \begin{diagram}[width=300]
import SpeciesDiagrams
import Data.List
import Data.List.Split

dia =
  hcat' (with & sep .~ 0.5)
  [ unord (map labT [0..2]) === txt' 8 "E"
  , mkArrow 2 (txt "L")
  , enRect listStructures === txt' 8 "L[E]"
  ]
  # centerXY
  # pad 1.1
  # lwO 0.7

listStructures
  = centerXY
  . hcat' (with & sep .~ 0.7)
  . map (vcat' (with & sep .~ 0.5))
  . chunksOf 2
  . map (drawList' labT)
  . permutations
  $ [0..2]
    \end{diagram}
    %$
  \end{center}
  If the set on the left is $E = \{0,1,2\}$, then the set on the right
  is $L[E]$, the set of all $L$-structures supported by $E$.  Each
  linear order $s \in L[E]$ is called an $L$-structure, or element of
  $L$, and has $E = \{0,1,2\}$ as its underlying set.
\end{commentary}

If $u: E \to F$ is a bijection, the element $t = M [u] (s)$ is the
structure on $F$ obtained by \term{transport along $u$}. The bijection
$u$ is an \term{isomorphism} between $s$ and $t$:
\[ u : s \to t. \]
\begin{commentary}
  These next sentences have to do with the second component of the
  functor, the mapping from morphisms to morphisms.  In this case,
  morphisms in the source category are bijections between label sets
  (which one can think of as \term{relabellings}), which are sent to
  bijections between sets of labelled structures (with each structure
  corresponding to its relabelled version).

  The diagram below shows an example, where $M$ is some species of
  labelled binary trees. $u$ is a bijection between the label sets
  $E = \{0, \dots, 4\}$ and $F = \{a, \dots, e\}$; so $M[u]$ is a
  bijection between the set $M[E]$ of trees labelled by $E$ and $M[F]$
  of trees labelled by $F$.  The diagram shows a particular
  $s \in M[E]$ and its image $t = M[u](s)$ under the relabelling $M[u]$;
  notice how each label in the structure has simply been replaced by
  its corresponding label under $u$.

  \begin{center}
  \begin{diagram}[width=300]
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

import           Data.Char                      (ord)
import           Data.Maybe                     (fromMaybe)
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams

t :: BTree Int
t = BNode 1 (leaf 0) (BNode 2 (leaf 3) (leaf 4))

sig :: Int -> Char
sig = ("acebd"!!)

t1 = drawBinTreeWide . mkNamedTree id show $ t
t2 = drawBinTreeWide . mkNamedTree sig ((:[]) . sig) $ t

linkedTrees s t = hsep 0.8 [t1 === txt' 8 s, t2 === txt' 8 t]
  # applyAll (map conn [0..4 :: Int])
  where
    conn i = connectOutside'
      (with & arrowShaft .~ selectShaft i
            & shaftStyle %~ dashingG [0.05,0.05] 0
            & arrowHead .~ noHead
      )
      i (sig i)
    selectShaft i || i `elem` [0,3] = theArc # reverseTrail
                  || i `elem` [2,4] = theArc
    selectShaft _ = hrule 1
    theArc = arc xDir (65 @@@@ deg)

drawSig :: String -> String -> String -> Int -> (Int -> Char) -> Diagram B
drawSig e f u n sig = vcat
  [ hsep 0.2 [txt' 8 e, unord (map (mkNamedNode id show) [0..n-1])]
  , txt' 8 u # translateX 0.5
  , hsep 0.2 [txt' 8 f, unord (map (mkNamedNode sig ((:[]).sig)) [0..n-1])]
  ]
  # applyAll [ connectOutside' opts x y || x <- [0 :: Int .. n-1], let y = sig x ]
  where
    opts = with & arrowHead .~ noHead & shaftStyle %~ dashingG [0.05,0.05] 0

-- XXX eventually this should be moved into diagrams-lib
connectEnvelope'
  :: (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2)
  => ArrowOpts n -> QDiagram b V2 n Any -> n1 -> n2 -> QDiagram b V2 n Any -> QDiagram b V2 n Any
connectEnvelope' opts label n1 n2 =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
    let v = location b2 .-. location b1
        midpoint = location b1 .+^ (v ^/ 2)
        s' = fromMaybe (location b1) $ envelopePMay v b1
        e' = fromMaybe (location b2) $ envelopePMay (negated v) b2
    in
      atop (arrowBetween' opts s' e' <> label # moveTo midpoint)

dia = hcat' (with & sep .~ 3)
  [ drawSig "E" "F" "u" 5 sig     # centerXY # named "sig"
  , linkedTrees "s" "t = M[u](s)" # centerXY # named "trees"
  ]
  # connectEnvelope' (with & gap .~ local 0.5)
      (txt' 8 "M[u]" # translateY 0.4) "sig" "trees"
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \end{center}

  Joyal writes $u : s \to t$, which is a mild abuse of notation; $u$
  is in fact a bijection between label sets $E$ and $F$, and $s$ and
  $t$ are elements of $M[E]$ and $M[F]$ respectively, not sets.  The
  point is that $u : E \to F$ induces a bijection $M[u]$ between the
  sets of structures $M[E]$ and $M[F]$; if this bijection relates
  $s \in M[E]$ and $t \in M[F]$, then we say that $s$ and $t$ are
  isomorphic, as witnessed by $u$, and write $u : s \to t$.  That is,
  whereas $u : E \to F$ is pronounced ``$u$ is a function from $E$ to
  $F$'', $u : s \to t$ is pronounced ``$u$ relabels $s$ to $t$'', that
  is, $u$ is a relabelling which transports the structure $s$ to the
  structure $t$.

  A concrete example should help to clarify the idea. The trees $s$
  and $t$ shown in the example above are isomorphic, as witnessed by
  the relabelling $u$.  Any other tree $t'$ of the ``same shape''
  would also be isomorphic to $s$, as long as there exists some
  bijection $u'$ which swaps out the labels of $s$ for the
  corresponding labels in $t'$.  As an example of non-isomorphic tree
  structures, however, consider the diagram below: there is no way to
  turn $s$ into $t$ merely by permuting labels.  These two tree
  structures are fundamentally different.

  \begin{center}
  \begin{diagram}[width=150]
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams

s, t :: BTree Int
s = BNode 1 (leaf 0) (BNode 2 (leaf 3) (leaf 4))
t = BNode 1 (BNode 2 (leaf 3) (leaf 4)) (leaf 0)

dia = hsep 0.8
  [ (drawBinTreeWide . mkNamedTree id show) s === txt' 8 "s"
  , (drawBinTreeWide . mkNamedTree id show) t === txt' 8 "t"
  ]
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \end{center}

  The fact that a species is a \emph{functor} (as opposed to just a
  mapping from sets of labels to sets of structures) corresponds to
  the idea of ``baking in'' the notion of \emph{transport}, mentioned
  earlier.  Here we see that the functorial mapping from a bijection
  on label sets to a bijection on sets of structures is exactly what
  allows us to ``transport'' structures along a bijection of
  labels to get new, relabelled structures.  The fact that we can
  always do this means that the precise nature of the labels does not
  matter: there will always be \emph{some} set of labels to allow us
  to talk about the atoms in a structure, but we get the same
  structures no matter what labels we use.

  This is, it seems to me, the crux of Joyal's insight: replacing the
  complicated \emph{intensional} definition of Bourbaki (defined as a
  very explicit construction of sets and relations) with a simple,
  \emph{extensional} definition.  Species are defined by their
  behavior, that is, what we can \emph{do} with them, not by what they
  \emph{are}: species are precisely those things which generate a set
  of structures from a set of labels and which can be functorially
  relabelled.  This is what Joyal is referring to in the introduction
  when he writes ``The emphasis is placed on the transport of
  structures rather than on their properties.''

  % \todo{Insert here commentary from section 3.2.1 of thesis?}
\end{commentary}
We denote by $\el (M)$ the category whose objects are the
$M$-structures and whose morphisms are isomorphisms of $M$-structures;
it is the groupoid of \term{elements} of $M$.  There is a
\term{forgetful} functor $U: \el (M) \to \B$ whose value on $s \in M$
is the \term{underlying} set of the structure $s$. The concept of
isomorphism of structures defines an equivalence relation whose
classes are the \term{types} of structures of the species M; these
classes are the \term{connected components} of the groupoid $\el
(M)$. We use the notation $\pi_0 (M)$ to denote the set of types (of
structures) of the species $M$. If $s \in M$, we denote the
\term{type} of $s$ by the notation $||s|| \in \pi_0 (M)$.

\begin{commentary}
  This definition of structure \emph{types} is an important one, and
  worth unpacking a bit.  A \emph{type} of structures is formally
  defined as an equivalence class of structures under isomorphism,
  that is, under relabelling.  For example, all the labelled trees
  shown below are isomorphic, since any two of them are related by some
  relabelling.
  \begin{center}
  \begin{diagram}[width=200]
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams

s :: BTree Int
s = BNode 1 (leaf 0) (BNode 2 (leaf 3) (leaf 4))

applyPerm p n = p !! n

dia = hsep 0.8
  [ (drawBinTreeWide . mkNamedTree id show) s
  , (drawBinTreeWide . mkNamedTree id show) (s # fmap (applyPerm [0,1,2,4,3]))
  , (drawBinTreeWide . mkNamedTree id show) (s # fmap (applyPerm [2,4,0,1,3]))
  , (drawBinTreeWide . mkNamedTree id show) (s # fmap (applyPerm [1,2,3,4,0]))
  ]
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \end{center}
  The collection of all such trees forms a \emph{type}. I also tend to
  use the word ``shape'' to mean the same thing. We can visually
  represent this type/shape like so:
  \begin{center}
  \begin{diagram}[width=50]
import           Diagrams.TwoD.Layout.Tree
import           SpeciesDiagrams

s :: BTree Int
s = BNode 1 (leaf 0) (BNode 2 (leaf 3) (leaf 4))

dia = (drawBinTreeWide . fmap (const (circle labR # fc black))) s
  # frame 0.5
  # lwO 0.7
  \end{diagram}
  \end{center}
  That is, intuitively, we can often think of types in terms of these
  pictures of ``unlabelled shapes'' with indistinguishable dots in
  place of labels.  However, thinking of shapes as structures with
  indistinguishable labels isn't always good enough---we'll see some
  examples later where it breaks down---so it's good to be able to
  return to the formal definition in terms of equivalence classes of
  labelled structures.

  It's worth pointing out that this definition of structure types,
  while simple, is actually rather profligate: an equivalence class of
  structures under relabelling contains structures labelled by
  \emph{every possible finite set}.  Hence a type is at least as big
  as the collection of all finite sets.  A more manageable way to get
  at the same concept is defined in \pref{sec:orbits}.

  $\el (M)$ is a \term{groupoid} because all its morphisms are
  invertible: if $u : s \to t$, that is, if $u$ is a relabelling
  witnessing the isomorphism of $s$ and $t$, then we necessarily have
  $u^{-1} : t \to s$.
\end{commentary}

\begin{ex}
  Recall that a \emph{simplicial scheme} structure on $E$ is a set
  $\Simp$ of non-empty subsets of $E$ such that (i) every non-empty
  subset contained in an element of $\Simp$ belongs to $\Simp$, (ii)
  the singletons $\{x\}$ for $x \in E$ belong to $\Simp$. The elements
  of $\Simp$ are \emph{simplices}. The dimension of a simplex is one
  less than its cardinality. A \emph{graph} is a simplicial scheme
  whose simplices have dimension $\leq 1$. If $u : E \to F$ is a
  bijection, it is clear that $u (\Simp) = \{u(S) \mid S \in \Simp\}$
  is also a simplicial scheme structure on $F$. We can therefore
  consider the \emph{species} of simplicial schemes. It is also clear
  that if $\Simp$ is a graph then $u(\Simp)$ is one too; we obtain the
  species of graphs, which is a \emph{subspecies} of the species of
  simplicial schemes. More generally, any property $P$ which applies
  to simplicial schemes, and which is invariant under isomorphism,
  determines a subspecies of the species of simplicial schemes. For
  example, connectedness is such an invariant property. The species of
  \emph{forests} is that of graphs \emph{without cycles}, the species
  of \emph{trees} is that of connected forests, etc.

\begin{commentary}
  This whole business with \emph{simplicial schemes} is unnecessarily
  general in some sense, but it allows Joyal to deal in one fell swoop
  with a large collection of common species (graphs, forests,
  trees\dots) which can all be seen as special cases of the species of
  simplicial schemes. It also incidentally provides a relevant
  motivation for introducing the concept of subspecies.  It would be
  tedious to go through each slight variant on the concept of a graph
  and show that it defines a valid species, but at the same time, it
  would be unsatisfying to just wave our hands and say things like
  ``this case is similar\dots''. By moving up a notch in abstraction,
  we get a rigorous yet economical definition that handles many
  similar cases at once.

  Geometrically, \term{simplices} are what we get when we start with a
  single point (a ``$0$-simplex''), and then repeatedly add points,
  each time connecting the new point to all the previous points (and
  also ``filling in'' the interior).  For example, a $1$-simplex is a
  line segment between two points; a $2$-simplex is a (filled in)
  triangle; a $3$-simplex is a solid tetrahedron; and after that they
  get harder to visualize, but conceptually are just the natural
  generalization of triangles and tetrahedra to $n$ dimensions.
  \begin{center}
  \begin{diagram}[width=200]
dia =
  [ pt
  , (pt <> hrule 1 # alignL) # alignR <> pt
  , mconcat
    [ foldMap (place pt) (triangle 1 :: [P2 Double])
    , triangle 1 # fc lightblue
    ]
  ]
  # map alignB
  # hsep 1
  # frame 0.5
  # lwO 0.7
  where
    pt = circle 0.08 # fc black
  \end{diagram}
\end{center}
Notice that every $n$-simplex has a ``boundary'' which consists of
simplices one dimension smaller: a line segment has two endpoints; a
triangle has three edges; a tetrahedron has four triangular faces, and
so on (yes, a $4$-simplex has five tetrahedral ``faces''.)  In fact,
if we label the vertices of an $n$-simplex with elements from some
finite set of size $n+1$, \emph{every} size $n$ subset corresponds to
the vertices of one of the $(n-1)$-dimensional faces.  Of course the
same is true of the faces in turn, that is, every size $n-1$ subset of
the vertices is a simplex which is the face of a face, and so on.

This explains what the geometric idea of simplices has to do with the
abstract set-based definition given above.  In the present context we
do not actually care about the geometry of simplices, but only their
combinatorial structure.  In this case, we just identify a simplex
with its set of vertices.  A \term{simplicial complex} $\Simp$ is a
collection of sets which we think of as a collection of simplices; the
requirement that every non-empty subset of an element of $\Simp$ also
belongs to $\Simp$ just means that whenever we have a simplex in our
collection, all its faces, faces of faces, and so on, are also
simplices in our collection.

The fact that the simplices in $\Simp$ are all built from some
underlying set of labels $E$ means that they can share vertices, so
$\Simp$ is not just a collection of independent, disjoint simplices,
but in general they can be glued together in various ways along their
faces. For example, below is a graphical representation of the
simplicial scheme
\[
  \{\{0\},\{1\},\{2\},\{3\},\{4\},\{1,3\},\{0,1\},\{0,2\},\{1,2\},\{0,1,2\}\}. \]
  \begin{center}
  \begin{diagram}[width=150]
dia = mconcat
  [ mconcat
    [ mconcat (zipWith (\(n,l) -> place (pt n l)) [(2,2),(3,0),(4,1)] (pathVertices t # concat))
    , strokeP t # fc lightblue
    ]
  , (hrule 1 # alignL <> pt 4 3) # alignR
  , pt 2 4 # translate (2 ^& 0.5)
  ]
  # frame 0.5
  # lwO 0.7
  where
    t  = triangle 1 # alignL # alignB :: Path V2 Double
    pt n l = mconcat
      [ circle 0.08 # fc black
      , text ("$" ++ show l ++ "$")
        # translate (unitX # scale (1/3) # rotateBy (fromIntegral n / 12))
        # fontSizeO 10
      ]
  \end{diagram}
  \end{center}

  Simplicial schemes, seeing as they consist simply of collections of
  subsets of the underlying label type
  $E$, can be relabelled just by applying the relabelling to every
  subset.  We can check that this is functorial: the identity
  relabelling acts as the identity on a simplicial scheme, and if we
  do one relabelling followed by another, it is the same as applying
  the composition of the two relabellings.  Therefore simplicial
  schemes form a valid species.

  Joyal then defines \term{graphs} as simplicial schemes with
  simplices of dimension $\leq 1$.  Hence graphs consist only of
  vertices (dimension 0 simplices, that is, singleton sets) and edges
  (dimension 1 simplices, that is, sets of size $2$), so this is an
  abstract way of defining what should be a familiar concept.  Note
  that this definition corresponds to graphs in which
  \begin{itemize}
  \item the edges are undirected, since edges are represented by
    \emph{sets} of vertices, which have no inherent order;
  \item there are no self-loops, since there cannot be a set
    containing two copies of the same vertex; and
  \item there is at most one edge between any two given vertices.
  \end{itemize}
  Other types of graphs relaxing some or all of these restrictions can
  also be defined as species.

  An example graph structure is shown in the figure below.
  \begin{center}
    \begin{diagram}[width=100]
      import           SpeciesDiagrams
      import qualified Data.Map as M
      dia = drawGraph labT
         ( M.fromList
           [ (0, 3 ^& (-1))
           , (1, 8 ^& 0)
           , (2, origin)
           , (3, 8 ^& 2)
           , (4, 4 ^& 2)
           , (5, 3 ^& (-3))
           ] # scale 0.6
         , [(2,0), (2,4), (0,4), (3,1), (0,5)]
         )
         # frame 0.3
    \end{diagram}
  \end{center}
  A \term{subspecies} of a species $S$ is then defined as a subset of
  $S$ (thought of as the collection of its structures) which is also a
  species.  Joyal points out that a valid subspecies results from
  restricting to any subset defined by a property which is not
  affected by relabelling (an \term{equivariant} property), since in
  that case relabelling a structure in the subset will always result
  in another structure in the subset.  The property of
  \term{dimension} is not affected by relabelling (since relabellings
  are bijections, they cannot change the size of a set), and hence
  graphs are a subspecies of simplicial schemes.

  As a non-example of a subspecies, consider the subset ``simplicial
  schemes containing the simplex $\{0,1,2\}$''.  This property is not
  preserved by relabelling (for example, consider the relabelling that
  sends $0 \mapsto a$, $1 \mapsto b$, and $2 \mapsto c$), and hence
  this subset is not a valid subspecies of simplicial schemes.

  Using this same technique of carving out a subspecies via an
  equivariant property, Joyal defines the species of forests (graphs
  with no cycles) and trees (connected forests).  For example,
  removing the 2---4 edge from the example shown above would make it a
  forest; then adding another edge, say, 4---3, would make it a tree.

  It is important to point out that the term \term{tree} is used here
  in the common mathematical sense of a connected graph without
  cycles, rather than in the common computer science sense of an
  inductive data structure with a root and children.  The difference
  is that a mathematical ``tree'' has undirected edges, and no
  privileged root node; adjacent nodes in the tree are just
  ``neighbors'', rather than parent and child.  A computational
  ``tree'' is what mathematicians would call a \term{rooted} tree,
  since one can obtain an inductive parent-child tree from a
  mathematical tree just by picking a distinguished root node; this
  will be explored in more detail later.
\end{commentary}
\end{ex}


\begin{ex} \label{ex:endofunctions} The transport of an endofunction
  $\phi : E \to E$ along a bijection $u : E \bij F$ is by
  \emph{conjugation}: $\phi \mapsto u \phi u^{-1}$. The species of
  \emph{permutations} is a subspecies of the species of
  endofunctions. If we require that the graph of an endofunction is
  connected, we obtain the subspecies of \emph{connected
    endofunctions} and, likewise, that of \emph{circular
    permutations}. An important concept is that of \emph{contraction}:
  an endofunction $\phi : E \to E$ is a contraction if there exists
  $x_0 \in E$ such that for every $x \in E$ we have $\phi^n(x) = x_0$
  when $n$ is large enough. That is, a contraction is an endofunction
  which is ultimately constant.

  \begin{commentary}
    The first sentence of \pref{ex:endofunctions} both introduces the
    class of \term{endofunctions} $\phi : E \to E$ over some set $E$,
    and explains why it is a valid species.  To show that it is a
    valid species, we are required to say how to functorially relabel
    an endofunction $\phi : E \to E$ given a bijection (relabelling)
    $u : E \bij F$. The situation can be pictured as below:
    \[ \xymatrix{E
        \ar@@{<->}[r]^{u} \ar[d]_{\phi} & F \ar@@{.>}[d]^{u \phi u^{-1}} \\
        E \ar@@{<->}[r]^{u} & F } \] ``Transporting $\phi$ along $u$''
    means using the bijection $u$ between $E$ and $F$ to turn $\phi$
    from an endofunction on $E$ into an endofunction on $F$.  The way
    to do this can be read off from the picture: the $\phi$ on the
    left side of the square turns into the dotted arrow on the right
    side of the square, formed by following three sides of the square
    around.  That is, first run $u^{-1}$, to turn $F$ back into $E$;
    then perform $\phi$; then run $u$ to get back to $F$ again.  Note
    that $u \cdot \id \cdot u^{-1} = u \cdot u^{-1} = \id$, and
    $(u \cdot \phi \cdot u^{-1}) \cdot (u \cdot \psi \cdot u^{-1}) = u
    \cdot (\phi \cdot \psi) \cdot u^{-1}$, so this defines a valid
    functor.

    Functional programmers are not used to thinking of a definition
    like $\mathbf{End}(E) = E \to E$ as a functor, since $E$ occurs
    both positively and negatively.  And indeed, given a
    \emph{function} $u : E \to F$ it is not possible to turn
    $\phi : E \to E$ into an endofunction $F \to F$. But since $u$ is
    a \emph{bijection}, we can deal just as easily with negative
    occurrences as positive, by making use of the inverse $u^{-1}$.
    Intuitively, almost \emph{any} type built out of products,
    coproducts, and functions will define a valid species. (The word
    ``almost'' is necessary since the type must be \term{finitary},
    that is, have only finitely many structures of any given
    size.)

    For those who are familiar with homotopy type theory, note that
    this situation matches up nicely with the notion of transport
    along a path.  Indeed, one can define species in HoTT in such a
    way that transport of species is literally given by transport
    along paths \citep{yorgey2014combinatorial}.

    \term{Permutations} are endofunctions which are invertible;
    invertibility is invariant under relabelling, so permutations are
    a subspecies of endofunctions; likewise connected endofunctions,
    connected permutations (which must consist of one single cycle),
    and contractions.  Typical structures of these species are
    illustrated in \todo{illustration}.

    Joyal claims that \term{contractions} are important, but note that
    this is not because they were already important (as far as I
    know), but simply because they will play an important role later
    in the paper.
  \end{commentary}
\end{ex}

\begin{ex} \label{ex:monomial}
  Let $S$ be the species of permutations. Consider the groupoid $\el
  (S)$ of elements of $S$. The objects of $\el (S)$ are the sets $E
  \in \B$ equipped with a permutation $\sigma_E \in S [E]$. The
  morphisms $(E, \sigma_E) \to (F, \sigma_F)$ are the bijections $u: E
  \to F$ such that $u\sigma_E = \sigma_F u$. Let $x_1, x_2, x_3,
  \dots$ be an infinite sequence of variables.  Let $I (\sigma_E) =
  x_1^{d_1} \dots x_n^{d_n}$, where $n = \Card E$ and where $d_i$ is
  the number of cycles of length $i$ in $\sigma_E$. Two objects $(E,
  \sigma_E)$ and $(F, \sigma_E)$ in $\el (S)$ are isomorphic if and
  only if $I (\sigma_E) = I (\sigma_F)$. The set $\pi_0(S)$ of
  connected components of the groupoid $\el (S)$ is therefore
  naturally identified with the set $\Mon(x)$ of all monomials in the
  variables $x_1, x_2, x_3, \dots$

  \begin{commentary}
    The objects of $\el(S)$ are $S$-structures, that is, permutations
    on specific sets, and the morphisms are isomorphisms between
    $S$-structures.  Unpacking definitions a bit, recall that two
    structures are isomorphic if one can be relabelled (\ie
    ``transported'') into the other.  So an isomorphism between a
    permutation $\sigma_E$ on the set $E$ and a permutation $\sigma_F$
    on the set $F$ is a relabelling $u : E \bij F$ such that
    relabelling $\sigma_E$ by $u$ results in $\sigma_F$; from the
    previous example we know that the relabelling of $\sigma_E$ by $u$
    is defined as $u \sigma_E u^{-1}$.  If
    $u \sigma_E u^{-1} = \sigma_F$, we can compose both sides by $u$
    on the right to obtain $u \sigma_E = \sigma_F u$.

    The rest of the example simply observes that relabelling can't
    change the structure of a permutation at all: it necessarily
    preserves the number and size of the cycles.  So $I$, which maps
    each permutation to a monomial representing the number of cycles
    of each given size, is invariant under isomorphism.
  \end{commentary}
\end{ex}

\subsubsection{} \label{sec:orbits}
The group $E!$ of permutations of $E$ acts on $M [E]$ by transport
of structures. The set $\pi_0 (M [E])$ of \emph{orbits} is identified
with the set of types of $M$-structures supported by sets equipotent
with $E$. We identify the orbit of $s \in M[E]$ with its type
$||s||$. The \emph{stabilizer} subgroup of an element $s \in M [E]$ is
the group $\Aut (s)$ of \emph{automorphisms} of $s$. We have the
well-known formula \[ \Card ||s|| = \frac{n!}{\Card \Aut(s)}. \]

\begin{commentary}
  There is quite a lot to unpack in this paragraph!  Recall that a
  \emph{type} is an equivalence class of labelled structures under
  isomorphism. As noted previously, however, such an equivalence class
  is unmanageably large, since it contains structures labelled by
  every possible finite set.  One of the main observations of the
  above paragraph is that we still have all the relevant information
  if we consider just \emph{one particular} set of a given size, and
  permutations on that set, rather than considering \emph{all} sets of
  a given size and bijections between them.

  \todo{Permutations, group actions, orbits, etc.}
\end{commentary}

One of the fundamental problems of enumerative combinatorics is to
evaluate the two infinite sequences of numbers
\begin{gather}
  \Card M[n], \quad \text{$n \geq 0$ ($[n] = \{1, 2, \dots, n\}$)}, \\
  \Card \pi_0(M[n]), \quad n \geq 0.
\end{gather}

\begin{commentary}
  In English: how many labelled $M$-structures are there of each size
  $n$?  And how many \emph{unlabelled} $M$-structures (that is,
  equivalence classes of labelled $M$-structures under relabelling)?
\end{commentary}

We define two generating functions. The first is a series of Hurwitz
\citep{comtet1974combinatorics}:

\begin{equation}
  M(x) = \sum_{n \geq 0} \Card M[n] \frac{x^n}{n!}.
\end{equation}

\begin{commentary}
  As mentioned in the introduction, this is also known as an
  \emph{exponential} generating function (EGF).  Again, this type of
  generating function (with an $n!$ in the denominator) turns out to
  be exactly the right thing to count \emph{labelled} structures.
  Intuitively, dividing by $n!$ accounts for the $n!$ different ways
  to permute a set of $n$ labels.
\end{commentary}

The second is a power series with integer coefficients (without
factorial):

\begin{equation}
  \unl M(x) = \sum_{n \geq 0} \Card \pi_0(M[n]) x^n.
\end{equation}

\begin{commentary}
  This is known as an \emph{ordinary} generating function (OGF), in
  contrast to an exponential one.
\end{commentary}

We say that $M (x)$ is the \emph{cardinality} of $M$. Let us see
immediately that the calculation of $\unl M (x)$ boils down to
computing the cardinality of the \emph{associated} species $\unl M$.

\begin{defn} \label{defn:unl}
  A structure of the species $\unl M$ is a pair $(\sigma, s)$ where
  $\sigma$ is an automorphism of $s \in M$.
\end{defn}

\begin{prop} \label{prop:unl}
  We have
  \[ \unl M (x) = \Card {\unl M}. \]
\end{prop}

\begin{proof}
  We use Burnside's lemma: if a finite group $G$ acts on a finite set
  $X$, then the cardinality of the set $\pi_0(X)$ of orbits of $X$ is
  equal to that of the set $\{(\sigma, x) \mid \sigma \in G, x \in X,
  \sigma \cdot x = x\}$ \emph{divided by} $\Card G$. (See
  \citet[p. 191, Theorem VII]{burnside1955groups}.)
\end{proof}

\subsection{The category of species}
\label{sec:category-of-species}

Species form a category: they are functors, and one can take natural
transformations as morphisms. As it is desirable to have a larger
class of morphisms than that of isomorphisms, it is best to consider a
species as a functor $M: \B \to \E$ to the category $\E$ of finite
sets and \emph{functions} (by composing with the inclusion $\B \inj
\E$).

\begin{commentary}
  When considering an individual species, the distinction between a
  functor $\B \to \B$ and a functor $\B \to \E$ does not seem
  important: of course any functor $\B \to \B$ can easily be turned
  into a functor $\B \to \E$ (by composing with the canonical
  inclusion $\B \inj \E$), but we can actually convert the other way
  too.  Since functors necessarily preserve isomorphisms, every
  bijection in $\B$ must actually map to a \emph{bijection} in $\E$
  under the action of any functor, so the image of a functor
  $\B \to \E$ actually lies in $\B$.

  However, considering the whole collection of species as a category,
  the distinction is an important one, since the functor categories
  $\B^\B$ and $\E^\B$ are not equivalent.  Although we have seen above
  that these two categories have essentially the same collection of
  \emph{objects} (namely, functors $\B \to \B$ or $\B \to \E$), they
  do not have the same \emph{morphisms} (namely, natural
  transformations).  If $\mathcal{C}$ and $\mathcal{D}$ are
  categories, a natural transformation between two functors from
  $\mathcal{C}$ to $\mathcal{D}$ consists of a collection of morphisms
  in $\mathcal{D}$; if all the morphisms of $\mathcal{D}$ are
  isomorphisms, then every natural transformation between functors
  $\mathcal{C} \to \mathcal{D}$ is necessarily an isomorphism as well.
  But this won't do: we certainly want to be able to talk about
  morphisms between species which are \emph{not} isomorphisms, for
  example, the morphism that sends the species of nonempty lists to
  the species of cycles by ``forgetting'' which element comes first.
  Categorically, having only isomorphisms also means the category
  can't have things like products and coproducts.  This, then, is what
  Joyal means by saying it is ``desirable to have a larger class of
  morphisms than that of isomorphisms'' (which turns out to be rather
  an understatement).  Hence $\B \to \E$ really seems to be the
  ``right'' notion of species to use.  (I am grateful to Ian Price for
  pointing out this fact to me, which I had somehow missed even after
  many years of thinking about such things!  And of course, it was in
  Joyal's original paper all along.)
\end{commentary}

\begin{defn}
  A \emph{morphism} $\alpha : M \to N$ is a natural transformation
  from $M$ to $N$, considered as functors from $\B$ to $\E$.
\end{defn}

One can interpret $\alpha$ as follows: one has a \emph{construction}
$\alpha$ allowing one to produce a structure of the species $N$
(output) from a structure of the species $M$ (input), and for every
bijection $u : E \to F$ the rectangle \[ \xymatrix{M[E]
  \ar[r]^{\alpha_E} \ar[d]_{M[u]} & N[E] \ar[d]^{N[u]} \\ M[F]
  \ar[r]^{\alpha_F} & N[F] } \] commutes; this means that the
construction is \emph{equivariant} (or invariant): it does not change
if one \emph{simultaneously} transports the input and the output along
the same bijection; the vast majority of mathematical constructions
have this property.

If $\sigma_E$ is invertible regardless of $E$, the morphism $\alpha$
is an \emph{isomorphism} between $M$ and $N$. In this case, we write
$M \stackrel{\alpha}{=} N$, or more simply (in an abuse of notation) $M
= N$. If $M$ and $N$ satisfy the weaker condition $\Card M = \Card N$,
we say that $M$ and $N$ are \emph{equipotent} species, and we write $M
\equiv N$.

\begin{ex}
  The construction of the transitive closure of a graph determines a
  morphism from the species of graphs to the species of partitions.
\end{ex}

\begin{ex} \label{ex:rooted-tree}
  A \emph{rooted tree} is a tree equipped with a \emph{root} (which is
  an arbitrary vertex of the underlying set). We usually orient the
  edges of a rooted tree in the direction of the root. If we adjoin a
  loop to the root, we obtain the graph of a \emph{contraction}
  (\pref{ex:endofunctions}). There is an \emph{isomorphism} between
  the species of rooted trees and the species of contractions.
\end{ex}

\begin{ex}
  The species of linear orders, of permutations, of permutations
  equipped with a fixed point, and of circular permutations equipped
  with an automorphism are all \emph{equipotent}, without being
  isomorphic.
\end{ex}

\subsubsection{} A morphism of species $M \to N$ determines a functor
$\el (M) \to \el (N)$ between the corresponding groupoids. Note that
this functor commutes with the forgetful functors \[ \xymatrix@@C=0.7em{\el(M)
  \ar[rr] \ar[dr]_U & & \el(N) \ar[dl]^U \\ & B.} \]
It is not true that a functor $\el (M) \to \el (N)$ is always induced
by a morphism of species $M \to N$. For example, if $M$ is the species
of preorders and $N$ the species of the orders, the usual construction
of an order relation to from a pre-order (on a quotient of the
pre-order's underlying set) determines a functor $\el (M) \to \el (N)$
that \emph{does not come} from a morphism of species $M \to
N$. However, it is easily checked that every functor $\el (M) \to \el
(N)$ which commutes with the forgetful functors $U$ is induced by one
and only one species morphism $M \to N$.

\subsection{Relative species}
\label{sec:relative-species}

We want to examine the concept of a \emph{relative} species. We begin
with an example. Let $G$ be the species of \emph{graphs}. The concept
of \emph{orientation} gives us a functor $O : \el (G) \to \E$, because
one can transport a graph orientation along a graph isomorphism. The
species of \emph{orientations} (of a graph) is \emph{relative} to that
of graphs. On the other hand, the species $GO$ of \emph{oriented
  graphs} is equipped with a projection $GO \to G$.

\begin{defn}
  Let $M$ be a species. A species \emph{relative} to $M$ is a functor
  $T_M : \el (M) \to \E$.
\end{defn}

\todo{This paragraph will need a lot of commentary and/or some
  nice pictures.}
Given $T_M$, one can construct a species $T$
equipped with a morphism $T \to^p M$: set $T [E] = \{(s, \alpha) \mid
s \in M [E], \alpha \in T_M[s]\}$. To transport $(s, \alpha) \in T[E]$
along a bijection $u : E \to F$ we begin by transporting $s$ to obtain
$t = M [u] (s)$, which gives first an isomorphism $s \to^u t \in
\el(M)$ and then $\beta = T_M [u] (\alpha)$; we set $T [u] (s, \alpha)
= (t, \beta)$. The morphism $p : T \to M$ is the projection $p (s,
\alpha) = s$. Conversely, given a morphism $T \to^p M$, we can
construct $T_M$: if $s \in M [E]$, we have $p_E: T [E] \to M [E]$ and
set $T_M [s] = p_E^{-1}\{s\} \subseteq T [E]$. Naturality of $p$
allows us to verify that if $u : E \to F$ is an isomorphism between $s
\in M [E]$ and $T \in M [F]$ then $T [u]$ turns $p_E^{-1} \{s\}$ into
$p_E^{-1} \{t\}$, which gives $T_M [u] : T_M [s] \to T_M [t]$. We
have, in fact, a precise proposition: a \term{species over $M$} is a
species $T$ equipped with a morphism $T \to^p M$. A morphism $(T, p)
\to (T', p')$ between species above $M$ is an arrow $T \to^u T'$ such
that $p'\ u = p$.

\begin{prop}
  The constructions described above define a equivalence between the
  category of species relative to $M$ and the category $E \||X\||/_M$ of
  species over $M$. \emph{(See \pref{sec:combinatorial-operations} for the
  notation $E\||X\||$.)}
\end{prop}

Suppose $T_M: \el (M) \to E$ is given. We often say that $(s, \alpha)
\in T [E]$ is an $M$-structure $s$ \term{equipped} with an element
$\alpha \in T_M[s]$. For example, a directed graph is a graph
\term{equipped} with an orientation. A structure of the species
$\tilde M$ (\pref{defn:unl}) is an $M$-structure \term{equipped} with an
automorphism.  \emph{etc.}

We sometimes use the term ``enriched'' rather than ``equipped''. Thus,
if $R$ is any species, we will say that endofunction $\phi : E \to E$
is \term{$R$-enriched} if each of its \term{fibers} $\phi^{-1}\{x\}$,
$x \in E$ is equipped with an $R$-structure.  Similarly, let $a$ be an
rooted tree on $E$. The \term{fiber} $a^{-1}\{x\}$ of a vertex $x \in
E$ is the set of vertices of $a$ connected to $x$ by an edge adjacent
to $x$ (for the orientation of a tree as described in
\pref{ex:rooted-tree}). We say that $a$ is \term{$R$-enriched} if each
of its fibers is equipped with an $R$-structure. (Keeping in mind the
empty fibers.)

In graphical representations of endofunctions or $R$-enriched trees it
is often convenient to assume that $R$-structures on the fibers are
placed on the set of \emph{edges} of the fibers.  For example, an
$R$-enriched tree can be represented as in \pref{fig:enriched-tree}
where an arc cutting through the edges of a fiber denotes an
$R$-structure. Don't forget the leaves.
\begin{figure}
  \centering
  \begin{diagram}[width=300]
import Diagrams
dia = drawEnrichedTree (layoutEnrichedTree figure0) # frame 0.5
  \end{diagram}
  \caption{An $R$-enriched tree} \label{fig:enriched-tree}
\end{figure}

\section{The Combinational Operations}
\label{sec:combinatorial-operations}

The category of species is rich in various operations. In this
section, we describe several operations of which three are binary.
The first two are the sum (disjoint) and the product. With these two
operations, the category of species becomes a kind of semi-ring.  More
precisely, let $R$ be a commutative ring, and denote by $\Poly R x$
the ring of Hurwitz series with coefficients in $R$: these are the
formal series
\[ \sum_{n \geq 0} a_n \frac{x^n}{n!}, \quad \text{where $n \geq 0,
  a_n \in R$.} \] The continued analogy here is that the category of
species would be the semiring $\Poly{\E}{X}$ of Hurwitz series, but
\emph{with coefficients from the category $\E$ of finite sets}. The
concept of cardinality induces a \emph{homomorphism}
\[ \Card : \Poly{\E}{X} \to \Poly{\Z}{x}. \]

In addition, the evaluation $M \mapsto M [0]$ is a functor preserving sum and
product
\[ \Poly \E X \to \E \]
whose kernel $J$ is an ideal on which we will describe the operations
of \term{divided powers} \citep{cartan1954seminaire}
\[ \gamma_n : J \to \Poly \E X \quad \text{($n \geq 0$)} \]
so that we have
\[ \Card \gamma_n(M) = \frac{M(x)^n}{n!} \quad \text{($n \geq 0$)}. \]

Using these operations of divided powers we then describe
the operation of \term{substitution} of one species into another. We end this
chapter with an introduction to the \term{differential calculus} of species
and a \emph{combinatorial} proof of the Lagrange inversion formula.

\subsection{Sum and product}

The disjoint sum of two species $M$ and $N$ is the \emph{coproduct}
in the category of species:
\[ (M + N) [E] = M [E] + N [E]. \]

More generally, an arbitrary family of species $(M_i)_{i \in I}$ is
summable if for any finite set $E$, the set of indices $i \in I$ for
which $M_i[E] \neq \varnothing$ is finite.  We set
\[ \left( \sum_{i \in I} M_i \right) [ E ] = \sum_{i \in I} M_i[E]. \]
It is clear that $\Card$ preserves sum. We now turn to the definition
of the \term{product} $M \cdot N$ of two species $M$ and $N$. Define
first a \term{partition} of a set $E$ into two \term{parts} as a pair
$(E_1, E_2)$ such that $E_1 \union E_2 = E$ and $E_1 \intersect E_2 =
\varnothing$. One defines in the same way the concept of a partition
of $E$ into $n$ pieces ($n \in \N$): we write $E = E_ + \dots + E_n$
to indicate that $(E_1, \dots, E_n)$ is a partition of $D$ into $n$
pieces.

\begin{defn}
  A structure of the species $M \cdot N$ on $E \in \B$ is a quadruplet
  $(E_1, E_2, s, t)$, where $E = E_1 + E_2$ and $(s, t) \in M[E_1]
  \times N [E_2]$.
\end{defn}

\begin{prop}
We have
\[ \Card (M \cdot N) = \Card (M) \cdot \Card (N). \]
\end{prop}
\begin{proof}
By definition,
\[ (M \cdot N)[E] = \sum_{E_1 + E_2 = E} M[E_1] \times N[E_2]. \]
If $\Card E = n$ and $0 \leq k \leq n$ there are $\binom n k$
partitions $E = E_1 + E_2$ with $\Card E_1 = k$.  As a result,
\[ \Card(M \cdot N) [n] = \sum_{k=0}^n \binom n k \Card M [k] \Card N
[n - k]. \]
\end{proof}

The \term{uniform} species is a species with only one structure on
each set; one can give it various representations: the structures of
complete graphs, chaotic topologies, and \trans{identity
  functions}{applications identiques}
determine the uniform species.

\begin{ex}
  Let $S$ be the species of \term{permutations}, and $S_0$ that of
  permutations without \emph{fixed points}, and $U$ the uniform
  species. We have $S = S_0 \cdot U$ (\pref{fig:permutation}); taking
  cardinalities, we get:
\[ \frac{1}{1-x} = S_0 (x) e^x \]
and therefore
\[ S_0 (x) = \frac{e^{-x}}{1 - x}. \]
\end{ex}

\begin{figure}
  \centering
  \begin{diagram}[width=250]
    import Diagrams

    dia = figure1 # frame 0.1
  \end{diagram}
  \caption{$S = S_0 \cdot U$}
  \label{fig:permutation}
\end{figure}
\todo{add 2-cycle to \pref{fig:permutation}.  Function for placing arrowhead @@
  midpoint of arbitrary path?}

\begin{ex}
  Let $D$ be the species of endofunctions, $D_0$ the species of
  endofunctions equipped with a fixed point, and $A$ and the species
  of rooted trees. Then
  \[ D_0 = A \cdot D. \] Indeed, one can partition the domain $E$ of
  an endofunction $\phi$ equipped with a fixed point $x_0$ into two
  parts $E = E_1 + E_2$. The first, $E_1$, consists of all the points
  ultimately transformed into $x_0$ by $\phi$. On $E_1$, $\phi$
  induces a contraction (\pref{ex:endofunctions}), which is equivalent
  to a rooted tree. On the second part $E_2$, $\phi$ induces an
  arbitrary endofunction (\pref{fig:endofunction}).
\end{ex}

\begin{figure}
  \centering
  \missingfigure{Endofunction}
  \caption{XXX}
  \label{fig:endofunction}
\end{figure}

The \emph{product} $M = \prod_{i=1}^n M_i$ of a finite sequence of
species can be explicitly defined as follows: a structure of the
species $M$ on $E$ is a \emph{partition} $E = E_1 + \dots + E_n$ where
each part $E_i$ (possibly empty) is \emph{equipped} with a structure
of the species $M_i$.

One can also describe the \term{power} $N^S$ of a species $N$ by a
finite set $S$: a structure of the species $N^S$ on $E$ is a function
$\chi : E \to S$ where each fiber is \emph{equipped} with a structure
of the species $N$; in other words, it is an \emph{$N$-enriched}
function.

\begin{ex}[Joyal] \label{ex:vertebrate}
  A \emph{vertebrate} is a tree \term{bipointed} by a pair $(p_0,
  p_1)$ of vertices. We say that $p_0$ is the \term{tail vertex} and
  $p_1$ the \term{head vertex}. The shortest path from the tail vertex
  to the head vertex is the \term{spine}. The vertices along the spine
  are \term{vertebrae}. For each vertex $p$, let $v(p)$ be the
  vertebra closest to $p$. The function $v$ is idempotent, and there
  is a rooted tree structure on each fiber of $v$.  The roots of these
  trees are the vertebrae. Thus, a vertebrate on $E$ determines a
  variable length partition $E = E_1 + \dots + E_n$, in which each
  part $E_i$ is equipped with a rooted tree structure, and vice
  versa. So we have the identity:
  \[ V = A + A^2 + A^3 + \dots, \] where $V$ is the species of
  vertebrates and $A$ that of rooted trees (\pref{fig:vertebrate}).
\end{ex}

\begin{figure}
  \centering
  \missingfigure{vertebrate}
  \caption{XXX}
  \label{fig:vertebrate}
\end{figure}

\begin{ex}
  Let $L$ be the species of linear orders and $S$ a finite set.  A
  structure of the species $L^S$ on $E$ is a function $E \to S$ where
  each fiber is equipped with a linear order. The number $l(n, s)$ of
  such objects (if $n = \Card E$ and $s = \Card S$) is the coefficient
  of $x^n/n!$ in the series $(1-x)^{-s}$. This shows that
  \[ l(n, s) = s (s + 1) \dots (s + n-1). \]
\end{ex}

Hereafter, we will often consider that a finite set $A$
determines a species by setting
\[ A [E] = \begin{cases} A \quad \text{if $E = \varnothing$} \\ \varnothing \quad \text{otherwise}. \end{cases} \]
With this convention, the category $\E$ acts as a ring of coefficients
for $\Poly \E X$, because the disjoint sum and Cartesian product of sets
can be conflated with the sum and product of species as described
earlier.

\subsection{Divided powers and substitution}

\begin{defn}
  Let $N$ be a species such that $N [0] = \varnothing$ and let $E$ be
  a finite set. An \term{assembly} of structures of the species $N$ on
  $E$ is a partition of $E$ where each class is equipped with a
  structure of the species $N$.  A \term{member} of the assembly is a
  class equipped with the corresponding $N$-structure. The
  \term{divided power} $\gamma_n(N)$ is the species of assemblies of
  $N$-structures with exactly $n$ members. The \term{exponential}
  $\exp (N)$ is the species of all assemblies of $N$-structures.
\end{defn}

\begin{prop}
We have
\begin{align*}
\Card \gamma_n(N) &= \frac{N(x)^n}{n!}, \\
\Card \exp (N) &= \exp (N (x)).
\end{align*}
\end{prop}

\begin{proof}
It obviously suffices to prove the first identity.  Note first
that a structure of the species $N^n$ on $E$ determines a partition $E
= E_1 + \dots + E_n$ into \emph{non-empty} (and disjoint) parts:
indeed, $E_i$ is equipped with an $N$-structure and by hypothesis $N
[0] = \varnothing$. If we forget about the linear order on the parts,
we have a partition $\{E_1, \dots, E_n\}$ where each class is equipped
with an $N$-structure. We have shown that an $N^n$-structure is none
other than an $N$-assembly whose members have been placed in a
\emph{linear order}:
\[ \Card N^n = n! \Card \gamma_n(N). \]
\end{proof}

For many species, one may indicate a concept of \term{connectedness}
and demonstrate that any structure consists of a partition
where each class is equipped with a connected structure. Under these conditions, a
species $M$ is isomorphic to the species of assemblies of connected structures:
$M = \exp (M_C)$.

\begin{ex}
  \term{Forests} are assemblies of trees. Forests of rooted trees are
  assemblies of rooted trees. Permutations are assemblies of circular
  permutations. Partitions are assemblies of partitions with a single
  class, \etc
\end{ex}

The operation of \term{substitution} of one species into another is
the richest in possibilities.

\begin{defn}
Let $R$ and $N$ be species and assume that $N [0] = \varnothing$.
The species $R (N)$ is that of pairs $(a, \rho)$, where $a$ is an assembly of $N$-
structures and $\rho$ is an $R$-structure on the set of members of $a$.

We say that $R (N)$ is the result of \term{substituting} $N$ into $R$. We
say that an element of $R (N)$ is an $R$-assembly (of $N$-structures). Note
immediately that $\exp (N)$ is the result of substituting $N$ in
the \emph{uniform} species (Ex. 7).
\end{defn}

\begin{thm} \label{thm:card-subst}
  Assuming $N [0] = \varnothing$, we have
  \[ \Card R (N) = R (N (x)). \]
\end{thm}

\begin{proof}
For each integer $n \geq 0$, let $R_n$ be the species of $R$-structures
where the underlying set has cardinality $n$. It has a decomposition
as a disjoint sum
\[ R = \sum_{n \geq 0} R_n, \]
inducing a decomposition of $R$-assemblies according to the number of
members:
\[ R (N) = \sum_{n \geq 0} R_n(N). \]
An element of $R_n(N)$ is an assembly of $n$ members \emph{equipped} with an $R$-structure.
We have therefore
\[ \Card R_n(N) = \Card \gamma_n(N) \times \Card R [n], \]
resulting in
\begin{align*} \Card R(N) &= \sum_n \geq 0 \Card R[n]
  \frac{N(x)^n}{n!} \\
  &= R(N(x)).
\end{align*}
\end{proof}

\begin{rem}
  To think combinatorially it is necessary to give visual
  representations. To grasp the nature of a species is to be capable
  of representing the general shape of its structures. The
  \emph{shape} of a structure is invariant under
  isomorphism. \trans{What is needed to represent first is the general
    type of structures of a given species}{Ce qu'il faut arriver \`a
    se repr\'esenter d'abord c'est le type g\'en\'eral des structures
    d'une esp\`ece donn\`ee}.  This type is independent of a labelling
  or an enumeration of the vertices of the underlying set. For
  example, suppose that we want to represent the general type of
  structures of the species $R(N)$ knowing that we already have a
  representation for $R$ and $N$. What we can do is to literally
  substitute arbitrarily selected $N$-structures in place of each
  vertex of an $R$-structure, For this, one can imagine that the
  vertices of the $R$-structure blow up into cells containing the
  $N$-structures.  The underlying set of a cellular configuration is
  the sum of the underlying sets of the structures contained in the
  cells. Thus, if we substitute the species of circular permutations
  into the species of trees, we obtain a species whose general type
  can be represented as in \pref{fig:tree-of-cycles}.

  \begin{figure}
    \centering
    \missingfigure{tree of cycles}
    \caption{XXX}
    \label{fig:tree-of-cycles}
  \end{figure}

This representation is not the only one, and it is convenient to adapt to
the particularities of a species. For example, suppose that the species $N$
is \emph{pointed}, that is, equipped with a morphism $N \to^p B$ where $B$ is the species
of ``vertices'' ($B [E] = E$ for $E \in B$). Each structure $s \in N[E]$ then has
a \emph{base point} $p (s) \in E$. One can used the base points to
give another representation of $R (N)$-structures: for each vertex
of an $R$-structure one chooses an $N$-structure and makes the vertex \emph{coincide}
with the base point of the selected $N$-structure. For example, the base point
a rooted tree is the root; if we substitute the species $A$ of rooted trees
into that of (nonempty) linear orders, we obtain the species of vertebrates
(Ex. 9). When $N$ is pointed we can define substitution as follows: an
$R (N)$-structure on $E$ is a triplet $(v, \alpha, \beta)$, where
\begin{enumerate}
\item $v$ is an idempotent function $E \to E$,
\item $\alpha$ is a function that selects for each $x \in \Im (v)$ a
  structure $\alpha (x) \in N [v^{-1}\{x\}]$ such that the base point
  of $\alpha (x)$ \emph{coincides} with $x \in v^{-1}\{x\}$,
\item $\beta$ is an $R$-structure on $\Im (v)$.
\end{enumerate}
\end{rem}

\begin{ex} \label{ex:endo-perm-of-rooted}
Let $D$ be the species of endofunctions, $S$ that of
permutations, and $A$ that of rooted trees. We have the decomposition
\[ D = S(A). \] Indeed, let $\phi \in D [E]$. A point $x \in E$ is
\term{periodic} if there exists an integer $n \geq 1$ such that
$\phi^n(x) = x$. The function $\phi$ \term{permutes} the periodic
points.  For each $x \in E$ let $v (x)$ be the first periodic point in
the sequence $x, \phi(x), \phi^2(x) \dots$. The function $v$ is
idempotent and its image is the set of periodic points. For each $x
\in \Im (v)$ the fiber $v^{-1}\{x\}$ is equipped with a rooted tree
structure whose root is $x$. \emph{Conversely}, if one has an assembly
of rooted trees and a permutation of the set of roots, it is clear
that one can construct a corresponding endofunction $\phi$
(\pref{fig:perm-of-rooted-trees}).
\end{ex}

\begin{figure}
  \centering
  \missingfigure{Permutation of rooted trees}
  \caption{XXX}
  \label{fig:perm-of-rooted-trees}
\end{figure}

Examples 9 and 12 give a simple proof of Cayley's theorem: the number
$a_n$ of trees on a set of cardinality $n \geq 1$ is
$n^{n-2}$. Indeed, the number of vertebrates (bipointed trees) is
equal to $n^2a_n$.  Example 9 shows that the vertebrates are
\emph{linear} assembies of rooted trees.  Example 10 shows that
endofunctions are \emph{permuted} assemblies of rooted trees.  As the
number of linear orders coincides with the number of permutations, one
obtains $n^2 a_n = n^n$ $(n \geq 1)$.

\pref{thm:card-subst} suggests adopting the notation $M (X)$ to
designate a species $M$. The variable $X$ is interpreted as the
\emph{singleton} species: there is only a single structure of the
species $X$ (up to isomorphism) and it is \trans{supported by the
  singletons}{port\'ee par les singletons}.  The result $M (X)$ of the
substitution of $X$ in $M$ is isomorphic to $M$. For some species we
adopt a \trans{frankly}{franchement} algebraic notation if it does not
create ambiguity. Thus, $\exp (X)$ or $e^X$ designate the uniform
species, $X e^X$ the species of pointed sets, $e^X - 1$ the uniform
nonempty species, $\cosh (X)$ and $\sinh (X)$ the uniform even and odd
species, $1/(1 - X)$ the species of linear orders, \etc However, we
retain the notation $S (X)$ to designate the species of permutations
in order to avoid confusion with $1/(1-X)$. Similarly, we will use the
notation $C (X)$ rather than $\log 1/(1-X)$ to designate the species
of circular permutations, \etc

\begin{ex}
  A preorder relation $\leq$ on $E$ determines an equivalence
  relation: $x \equiv y$ if and only if one has $x \leq y$ and $y \leq
  x$.  The preorder relation induced on the quotient $E / \equiv$ is
  an \trans{order relation}{relation d'ordre}, and conversely.  This
  shows that the species of preorders is obtained by substituting the
  species $e^X - 1$ in the species of order relations. In particular,
  the species of total preorders is
  \[ \frac{1}{1 - (e^X - 1)} = \frac{1}{2 - e^X}. \]
\end{ex}

\begin{ex}
  In a graph, two vertices are \term{doubly connected} if we can
  connect them with a path avoiding any edge selected beforehand. The
  vertices of a graph can be partitioned into doubly connected
  components.  Let $\Gc$ be the species of connected pointed graphs
  and $\Gcc$ that of doubly connected pointed graphs. We have the
  relation
  \[ \Gc = \Gcc(Xe^{\Gc(X)}). \] Indeed, in a connected pointed graph,
  consider the doubly connected component $H$ of the base point; for
  each vertex $x$ let $v(x)$ be the closest vertex located in the
  component $H$. We can easily verify that $v$ is well defined. It is
  an idempotent function whose fibers are equipped with a structure of
  the species $X \cdot e^{\Gc(X)}$; the image of $v$ coincides with
  the doubly connected pointed graph $H$ (\pref{fig:doubly-connected}).
\end{ex}

\begin{figure}
  \centering
  \missingfigure{doubly connected}
  \caption{XXX}
  \label{fig:doubly-connected}
\end{figure}

\begin{ex}[\citet{polya1937kombinatorische}] \label{ex:rooted-trees-eqn}
  Consider the species $A$ of rooted trees.  We have the identity
  (\pref{fig:root-plus-forest})
\[ A = X \cdot \exp (A). \]
More generally, the species $A_R$ of $R$-enriched trees satisfies
the equation (see \pref{fig:enriched-tree})
\[ A_R = X \cdot R(A_R). \]
\end{ex}

\begin{figure}
  \centering
  \missingfigure{root plus forest of rooted trees}
  \caption{XXX}
  \label{fig:root-plus-forest}
\end{figure}

\subsection{Differential calculus}

For any finite set $E$, let $E^+$ be the set obtained by adjoining to $E$
an additional item $*$:
\[ E^+ = E + \{ * \}. \]

\begin{defn}
The \term{derivative} species $M'$ of a species $M$ is defined as
follows:
\[ M' [E] = M [E^+]. \]
\end{defn}

\begin{ex}
  The derivative of the species $C (X)$ of circular permutations is
  the species of linear orders (\pref{fig:diff-cycle}):
  \[ C'(X) = \frac{1}{1 - X}. \]
\end{ex}

\begin{figure}
  \centering
  \missingfigure{derivative of a cycle}
  \caption{XXX}
  \label{fig:diff-cycle}
\end{figure}

\begin{ex}
  The derivative of the species of trees is that of forests of pointed
  trees (\pref{fig:diff-tree}).
\end{ex}

\begin{figure}
  \centering
  \missingfigure{derivative of a tree}
  \caption{XXX}
  \label{fig:diff-tree}
\end{figure}

\begin{ex}
  Recall that a graph is \term{even} if the number of edges adjacent
  to each vertex is even. The derivative of the species of even graphs
  is the species of graphs
  (\pref{fig:diff-even-graph}). \citep{harary1973graphical}.
\end{ex}

\begin{figure}
  \centering
  \missingfigure{derivative of an even graph}
  \caption{XXX}
  \label{fig:diff-even-graph}
\end{figure}

\begin{ex}
  The derivative of the species of linear orders $1 / (1-X)$ is equal
  to $(1 / (1-X)) \cdot (1 / (1-X))$.
\end{ex}

Recall that a \term{pointed} structure of the species $M$ on $E$ is an
element of $E \times M [E]$. We denote by $\pointed M$ the species of
pointed $M$-structures.

\begin{prop}
We have the relations
\begin{align*}
\pointed M &= X \cdot M', \\
(M + N)' &= M' + N', \\
(M \cdot N)' &= M' \cdot N + M \cdot N', \\
M(N)' &= M'(N) \cdot N'.
\end{align*}
\end{prop}

These identities are not only relationships between numeric quantities
but real \emph{combinatorial} identities. We leave to
the reader the pleasure of demonstrating this.

\begin{ex} \label{ex:trees-vertebrates}
To illustrate the combinatorial differential calculus,
consider the equation satisfied by the species $A$ of rooted trees:
\[ A = X \cdot e^A. \] The operation of ``pointing'' \trans{is a
  derivation}{est une d\'erivation}:
\[ \pointed A = \pointed X \cdot e^A + X \cdot e^A \pointed A. \]
Pointed, rooted trees are vertebrates:
\begin{align*}
V = \pointed A &= X \cdot e^A + X \cdot e^A V \\
&= A + A \cdot V \\
&= A + A^2 + A^3 + \dots .
\end{align*}
It has been shown that vertebrates are (non-empty) linear assemblies
of rooted trees. (See \pref{ex:vertebrate}.)
\end{ex}

\subsection{The Lagrange inversion formula}

\subsubsection{}
The methods of analysis sometimes leave traces in
the restricted world of formal series. An example is the theory of the \term{residue}
(at the point $0$) of a formal meromorphic series. The invariance property
of the residue with respect to an invertible change of parameter $x = w (t)$ is
a fundamental \emph{algebraic identity}:
\[ \Res f (x) dx = \Res f(u (t)) u' (t) dt. \]
This identity is equivalent to the \term{Lagrange inversion formula}. This
formula gives the coefficient $c_n$ of $x_n$ in the series $g (v (x))$ when $v (x)$
satisfies the equation $v (x) = xR(v (x))$:
\[ c_n = \frac{1}{n} \times \text{coefficient of $t^{n-1}$ in
  $g'(t)R(t)^n$}. \] To demonstrate this, it is sufficient to note
that this coefficient is equal to the residue at the origin of $g (v
(x))/x^{n+1}$. Note further that $u (t) = t / R (t)$ is the inverse
series of $v$. Performing the change of the parameter $x = u (t)$ and
using the invariance property yields
\begin{align*}
c_n &= \Res g(t) \frac{u'(t)}{u(t)^{n+1}} dt \\
&= \Res \left[ \frac{g'(t)}{nu(t)^n} - \left( \frac{g(t)}{nu(t)^n}\right)' \right] dt \\
&= \Res \frac{g'(t)}{nu(t)^n} dt \\
&= \frac{1}{n} \Res \frac{g'(t) R(t)^n}{t^n} dt \\
&= \frac{1}{n} \times \text{coefficient of $t^{n-1}$ in $g'(t) R(t)^n$}.
\end{align*}
Since the calculations are reversible, the equivalence between the inversion formula
and the invariance property is clear.

\subsubsection{} Since \citet{polya1937kombinatorische}, the inversion
formula is often used in combinatorics to calculate the coefficients
of certain generating series (the canonical example is the species $A$
of rooted trees that satisfy the equation $A = X \cdot \exp (A)$; see
\citet{moon1970counting}).

There is already a purely combinatorial proof of the inversion formula
\citep{raney1960functional}. That proof is based on different concepts
than those used in the following proof. In Chapter 4 XXX, we will give
another proof of the inversion formula.

\begin{thm}[Lagrange Inversion]
  Let $R$ and $F$ be species and let $A_R$ be the species of
  $R$-enriched rooted trees. For $n \geq 1$, we have:
\[ F(A_R)[n] \equiv F' R^n[n-1] \]
(the $\equiv$ denotes equipotence).
\end{thm}

\begin{proof}
  Recall (\pref{ex:rooted-trees-eqn}) that $A_R$ satisfies the
  equation
\[ A_R = X \cdot R (A_R). \]
This leads to the derivation
\begin{align*}
A_R' &= R(A_R) + X \cdot R'(A_R) A_R' \\
&= R (A_R) + C_R A_R'
\end{align*}
and by iteration ($C_R = X \cdot R'(A_R)$)
\begin{align*}
&= R (A_R) (1 + C_R + C_R^2 + \dots) \\
&= R(A_R) \frac{1}{1 - C_R}.
\end{align*}
At this point, we want to replace $1 / (1 - C_R)$ by $S(C_R)$, where
$S$ is the species of permutations. We need to interpret the result of
such replacement combinatorially.

\begin{lem}[\citet{labelle1981nouvelle}]
  The species $C_R = X \cdot R'(A_R)$ coincides with that of
  $R$-enriched contractions. (See \pref{sec:relative-species} and
  \pref{ex:rooted-tree}.)
\end{lem}

\begin{proof}
  Indeed, consider an $R$-enriched contraction $\phi : E \to E$. Let
  $x_0$ be the point of convergence of $\phi$. We can partition $E$
  into two parts: $\{x_0\} + E - \{x_0\}$. On the second part, there
  is the structure of an $R'$-assembly of $A_R$-structures as seen in
  \pref{fig:Rprime-AR} (the loop explains the presence of the
  derivative in the formula $C_R = X \cdot R'(A_R)$). \emph{And
    conversely.}
\end{proof}

\begin{figure}
  \centering
  \missingfigure{$R'$-assembly of $A_R$-structures}
  \caption{XXX}
  \label{fig:Rprime-AR}
\end{figure}

\begin{lem}[Labelle] The result of the substitution of $C_R$ in the
  species $S$ of permutations coincides with the species $D_R$ of
  $R$-enriched endofunctions.
\end{lem}

\begin{figure}
  \centering
  \missingfigure{Correspondence of $S(C_R)$ and $D_R$}
  \caption{XXX}
  \label{fig:SCR-DR}
\end{figure}

\begin{proof}
  The lemma states that $S(C_R) = D_R$. We use the decomposition of an
  endofunction into permutations and rooted trees as described in
  \pref{ex:endo-perm-of-rooted}. Replace the rooted trees in this
  decomposition by contractions. If we compare, at each point, the
  fiber of the endofunction with the fiber of the contraction
  ``containing'' that point, we realize that they are in bijection
  (they coincide if the point is not periodic). One can then carry the
  $R$-structures along these bijections. This shows that the
  $R$-enriched endofunctions are in canonical bijection with the
  \trans{permutation assemblies}{assembl\'ees permut\'ees} of
  $R$-enriched contractions.
\end{proof}

The proof of the theorem continues by taking the \emph{derivative} of
$F(A_R)$:
\begin{align*}
F(A_R)' &= F'(A_R) A_R' \\
&= F'(A_R) R(A_R) \frac{1}{1 - C_R} \\
&\equiv F'(A_R) R(A_R) D_R.
\end{align*}

\begin{lem}[Labelle, \trans{Repartitioning}{repartage} lemma]
  Let $G$ be any species.  A structure of the species $G(A_R) D_R$ on
  a set $E$ can be interpreted as giving a partition $E = E_1 + E_2$
  whose first part $E_1$ is equipped with a $G$-structure $\gamma$ and
  the second part $E_2$ is equipped with an $R$-enriched function
  $\lambda : E_2 \to E$.
\end{lem}

\begin{proof}
  Let $(F_1, F_2, g, f)$ be a structure of the species $G(A_R) \cdot
  D_R$ on $E$.  We have $E = F_1 + F_2$. We can describe $g$ as a
  forest of $R$-enriched rooted trees where the set of roots is
  equipped with a $G$-structure $\gamma$. Let $E_1$ be the set of
  these roots, and let $E_2$ be its complement. This forest of rooted
  trees, together with the endofunction $f$, defines an R-enriched
  function $\lambda : E_2 \to E$. (See
  \pref{fig:endo-plus-forest-rooted}.) Conversely, starting from $(E_1,
  E_2, \gamma, \lambda)$, we first recover $F_1$ as the set of all
  points in $E$ that are ultimately transformed into $E_1$ by
  $\lambda$. One may complete the proof by meditating on
  \pref{fig:endo-plus-forest-rooted}.
\end{proof}

\begin{figure}
  \centering
  \missingfigure{Endofunction + forest of rooted trees}
  \caption{XXX}
  \label{fig:endo-plus-forest-rooted}
\end{figure}

We now use this lemma to compute the cardinality of $(G(A_R) D_R)
[n]$. \todo{note typo $R_R$} Indeed, the $R$-enriched functions
$\lambda : E_2 \to E$ \emph{coincide} with the $R$-enriched functions
$\lambda : E_2 \to [n]$ in the case where $E = [n]$ (see the note
preceding \pref{ex:vertebrate}). We have therefore
\[ (G(A_R) D_R) [n] = (G \cdot R^n)[n]. \]
Finally, note that we want to calculate $F(A_R) [n]$, and
if $n \geq 1$, we have
\todo{should there be a prime on the first line?}
\begin{align*}
F(A_R)[n] &= F(A_R)[n-1] \\
&\equiv F'(A_R) R(A_R) D_R[n-1] \\
&\equiv (F' R) R^{n-1} [n-1] \\
&= F' R^n [n-1].
\end{align*}
\end{proof}

\section{Enumeration of Types of Structures}
\label{sec:types}

\subsection{} In this chapter, we will try to solve the problem of enumeration of
\emph{types} of structures of a given species $M$. It will suffice to
calculate, for each $n \in \N$, the cardinality of the set
$\pi_0(M[n])$ of the orbits of $M [n]$ under the action of $[n]!$. In
other words, we want to identify the generating series

\[ \unl M (x) = \sum_{n \geq 0} \Card \pi_0(M[n]) x^n. \]

It is often impossible to describe $\unl M (x)$ explicitly. However,
if we have a functional equation, we can calculate the coefficients of
$\unl M (x)$ by induction. The current technique for calculating the
number of isomorphism classes is due to P\'olya. It makes use of a
certain polynomial indicator of \emph{cycles}
\citep{polya1937kombinatorische}. Instead, we calculate an \term{index
  series} $Z_M$.  We obtain a substitution theorem for index
series. This result often allows us to calculate the coefficients of
$Z_M$ by \emph{induction}. We will follow a path less algebraic and
more combinatorial than P\'olya. In this way, we hope to show the
direct link between the problem of enumeration of structures and that
of enumeration of \emph{types} of structures.

Recall that calculating $\unl M(x)$ amounts to calculating the
cardinalty of an \emph{associated species} $\unl M$ (\pref{prop:unl}):
\[ \unl M(x) = \Card \unl M. \]
The elements of $\unl M[E]$ are the pairs $(\sigma, s)$, where $s \in
M[E]$ and $\sigma \in E!$ is an automorphism of $s$.  One notes easily
that the relation $P = M + N$ implies $\unl P = \unl M + \unl N$ and
also that $P = M \cdot N$ implies $\unl P = \unl M \unl N$.  Now
suppose that $P = M(N)$; what can we say about $\unl P$?  For example,
we have the following obvious result:
\begin{prop}
  If $P = 1/(1 - N)$ then \[ \unl P = \frac{1}{1 - \unl N}. \]
\end{prop}
We also have the well known result \citep{harary1973graphical}:
\begin{prop} \label{prop:exp}
  If $P = \exp(N)$ then \[ \unl P = \exp \left[ \sum_{n \geq 1}
      \frac{\unl N(x^n)}{n} \right]. \]
\end{prop}
We will demonstrate:
\begin{prop}
  Let $S$ be the species of permutations.  If $P = S(N)$ then \[ \unl
    P(x) = \prod_{n \geq 1} \frac{1}{1 - \unl N(x^n)}. \]
\end{prop}

The difficulty rests in the fact that the relation
$\widetilde{M(N)}(x) = \unl M(\unl N(x))$ is false.  Examples show
that the nature of the answer depends strongly on the interal structure
of the thing being substituted and very little on its cardinality. We
will develop a concept finer than cardinality:
\[ Z : \Poly \E X \to \MPoly \Z x \]
having a value in the ring $\MPoly \Z x$ of formal series in an
infinite number of variables $\{x_1, x_2, \dots\}$.  This ring is
equipped with a substitution operation: if $f = f(x_1, \dots)$ and $g
= g(x_1, \dots)$ then \[ f(g) = f(g_1, g_2, \dots), \] where \[ g_i =
  g(x_i, x_{2i}, \dots). \] We will demonstrate a substitution
theorem: \[ Z_{M(N)} = Z_M(Z_N). \]  Moreover, we have
\begin{align*}
  M(x) &=& Z_M(x, 0, 0, \dots), \\ \unl M(x) = Z_M(x, x^2, x^3, \dots)
\end{align*}
and we obtain the following result:
\begin{prop}
  If $P = M(N)$ then \[ \unl P(x) = Z_M(\unl N(x), \unl N(x^2),
    \dots). \]
\end{prop}

We could give completely combinatorial demonstrations of the results
in this chapter, but at the cost of heavy categorical artillery.  We
have chosen an intermediate approach, using a bit of algebra such as
the principle of extension of algebraic identities.  This results in a
text that is easier to read.

\subsection{} The problem is to calculate the cardinality of
$\widetilde{M(N)}$.  For this, we seek to understand its combinatorial
structure.  An element of $\widetilde{M(N)}[E]$ is a pair $(\sigma, h)$, where $h$
is an element of $M(N)[E]$ and $\sigma$ is an automorphism of $h$.
The assembly $h$ determines an equivalence relation $R$ on $E$.
$\sigma$ is an automorphism of $E$, which means that $\sigma$ is
\emph{compatible} with $R$.  The quotient $E/R$ is therefore equipped
with an induced permutation $\sigma/R$.  This permutation decomposes
into cycles.  We will begin by supposing that $\sigma/R$ is circular.

\todo{Should ``couronne'' be ``crown''? Or perhaps ``wreath''?}

\begin{defn}
  A \trans{crown}{couronne} of $N$-structures is an assembly
  $h \in \exp(N)$ \emph{equipped with} an automorphism $\sigma$ that
  circularly permutes its members.  The \term{length} of a crown is
  the number of members of the assembly.  We use $C_n(N)$ to denote
  the species of crowns (of $N$-structures) whose length is $n$.
\end{defn}

\begin{prop}
  \[\Card C_n(N) = \unl N(x^n)/n. \]
\end{prop}

\begin{proof}
  We first show how, from an element of $\unl N(X^n)$, we can
  construct a crown.  An element $l$ of $\unl N(X^n)[E]$ is an
  $\unl N$-assembly of linear orders with cardinality $n$.  This
  element $l$ defines a partition $P$ on $E$; the set $E/P$ of classes
  is equipped with an $\unl N$-structure $(\tau, t)$ and each class is
  a linear order of cardinality $n$.  For $1 \leq i \leq n$ let $C_i$
  be the set of all the elements of rank $i$ in these linear orders.
  If we send each element to its sucessor, we obtain bijections
  $\sigma_i : C_i \to C_{i+1}$ for $1 \leq i \leq n-1$.  We have a
  chain \[ C_1 \to C_2 \to C_3 \dots \to C_n. \] \trans{Just
    as}{Comme} every $C_i$ is a system of representatives of the
  partition $P$, we have a structure $t_i$ on $C_i$ by transporting
  the $N$-structure $t$ of the assembly. We also have an automorphism
  $\tau_1$ of $t_1$ by transporting $\tau$ on $C_1$. We will
  circularly close the chain of isomorphisms of $N$-structures
  \[ (C_1,t_1) \xrightarrow{\sigma_1} (C_2, t_2) \dots
    \xrightarrow{\sigma_{n-1}} (C_n, t_n) \] by defining an
  isomorphism $\sigma_n: (C_n,t_n) \to (C_1, t_1)$. Just take
  $\sigma_n = \tau_1(\sigma_{n-1} \dots \sigma_1)^{-1}$. We obtain a
  \term{crown} $(\sigma, h)$ of length $n$ on $E$.  Moreover, in this
  crown, there is an \emph{initial} $N$-structure $(C_1,
  t_1)$. Conversely, suppose we have a crown
  $(\sigma, h) \in C_n(N)[E]$ \emph{marked} with an initial
  $N$-structure $(C_1, t_1)$. Taking successive images of $(C_1, t_1)$
  by $\sigma$ we obtain a chain
  \[ (C_1,t_1) \xrightarrow{\sigma_1} (C_2,t_2) \dots
    \xrightarrow{\sigma_{n-1}} (C_n, t_n). \] Each element
  $x_1 \in C_1$ is the first element of a linear order
  $\{x_1 < \sigma x_1 < \dots < \sigma^{n-1}x_1\}$.  This gives us a
  partition $P$ whose classes are linear orders of size $n$. The set
  $E/P$ of classes is equipped with an $N$-structure $t$ isomorphic to
  $t_1$ (or $t_i$). To obtain an automorphism $\tau$ of $t$, it
  suffices to transport on $E / P$ the automorphism $\tau_1$ of $C_1$
  obtained by making a complete tour of the crown:
  \[ \tau_1 = \sigma^n \mid C_1. \] It has therefore been shown that
  an $\unl N (X^n)$ structure is equivalent to a crown of length $n$
  \emph{marked} with an initial member. Since there are $n$ possible
  choices for this initial member, we have
  \[ n \times \Card C_n(N) = \unl N(x^n). \]
\end{proof}

\begin{cor}
  Suppose that $N[0] = \varnothing$.  The species of crowns of
  $N$-structures has as its cardinality \[ \sum_{n \geq 1} \frac{\unl
      N(x^n)}{n}. \]
\end{cor}

Consider now an element $(\sigma, h) \in \widetilde{\exp(N)}[E]$.  The
element $h$ is an assembly of $N$-structures; it induces an
equivalence relation $R$ on $E$.  The automorphism $\sigma$ induces a
permutation $\sigma/R$ of $E/R$.  This permutation $\sigma/R$
decomposes $E$ into an \emph{assembly} of crowns of $N$-structures.
Thus we have

\begin{cor}
  Suppose that $N[0] = \varnothing$.  We have \[ \Card
    \widetilde{\exp(N)} = \exp \left[ \sum_{n \geq 1} \frac{\unl
        N(x^n)}{n} \right]. \]
\end{cor}

Note that the corollary implies Proposition~\ref{prop:exp}.  Now let
$S$ be the species of permutations.  Consider the category $\el(S)$ of
elements of $S$.  The objects of $\el(S)$ are the pairs $(E,
\sigma_E)$, where $\sigma_E \in S[E]$.  We have already described
(Example~\ref{ex:monomial}) the cycle indicator monomial $I(\sigma_E)
= x_1^{d_1} \dots x_n^{d_n}$.  The set $\pi_0(S)$ of connected
components of the groupoid $\el(S)$ is identified with the set
$\Mon(s)$ of monomials in the variables $x_1, x_2, x_3,
\dots$.  Moreover, if $I(\sigma_E) = \xx = x_1^{d_1} \dots
x_n^{d_n}$, the cardinality of the group of automorphisms of the
object $(E, \sigma_E)$ is equal to \[ \aut(\xx) = 1^{d_1}
  2^{d_2} \dots n^{d_n} d_1! \dots d_n!. \]  Consider now an element
$(\sigma,h) \in \widetilde{\exp(N)}[E]$.  We know that $h$ determines
an equivalence relation $R$ on $E$ compatible with $\sigma$.  We say
that $(\sigma, h)$ has \term{class} $\xx = x_1^{d_1} \dots
x_n^{d_n}$ of we have $I(\sigma/R) = \xx$, where $\sigma/R$ is
the permutation induced by $\sigma$ on the quotient $E/R$. In
particular $(\sigma, h)$ has class $x_n$ if and only if it is a crown
of length $n$.  With these conventions, we have:

\begin{prop}
  The species of \emph{assembies of crowns} (of $N$-structures) having
  class $\xx = x_1^{d_1} \dots x_n^{d_n}$ has as its
  cardinality \[ \frac{1}{\aut(\xx)}\unl N(x)^{d_1} \unl N(x^2)^{d_2}
    \dots \unl N(x^n)^{d_n}. \]
\end{prop}

\begin{proof}
  Indeed, this species is expressed as the product of \trans{divided
    powers}{puissances divis\'ees} \[ \gamma_{d_1}(C_1(N))
    \gamma_{d_2}(C_2(N)) \dots \gamma_{d_n}(C_n(N)). \] Its
  cardinality is therefore \[ \frac{1}{d_1!} \unl N(x)^{d_1}
    \frac{1}{d_2!}\left( \frac{\unl N(x^2)}{2}\right)^{d_1} \dots
    \frac{1}{d_n!} \left( \frac{\unl N(x^n)}{n}\right)^{d_n}. \]
  \todo{Is the $d_1$ exponent on the second term a typo for $d_2$?}
\end{proof}

\bibliographystyle{plainnat}
\bibliography{series-formelles}

\appendix
\section{Bourbaki's definition of species}
\label{sec:appendix-bourbaki}

This appears on page 6 of \emph{\'El\'ements de math\'ematique},
Book 1 (Theory of Sets), Chapter 4, available from
\url{http://sites.mathdoc.fr/archives-bourbaki/PDF/180_nbr_083.pdf}.
\todo{cite}

\begingroup \itshape
We say that we have defined, in a theory $\mathcal{C}$ at least as
strong as set theory, a \emph{species of structure} when we have:

\begin{enumerate}
\item A certain number of distinct variables $x_1, \dots, x_n, s_1,
  \dots, s_p$ besides the constants of $\mathcal{C}$.
\item The \emph{echelons} $T_1, T_2, \dots, T_p$ of an \emph{echelon
    construction} on $n$ letters $x_1, \dots, x_n$, equal in number to
  the letters $s_j$, distinct or not.
\item A relation $R\{x_1, \dots, x_n, s_1, \dots, s_p\}$ of the theory
  $\mathcal{C}$, of the form
  \begin{multline*}
    s_1 \subset T_1(x_1, \dots, x_n) \text{ and } s_2 \subset T_2(x_1,
    \dots, x_n) \text{ and \dots and } \\
    s_p \subset T_p(x_1, \dots, x_n) \text{ and } R'\{x_1, \dots, x_n,
    s_1, \dots, s_p\}
  \end{multline*}
  so that the following relationship is a theorem of $\mathcal{C}$:

  (IS) $(R\{x_1, \dots, x_n, s_1, \dots, s_p\}$ and $f_1$ is a
  bijection of $x_1$ onto $y_1$ and \dots and $f_n$ is a bijection of
  $x_n$ onto $y_n) \implies R\{y_1, \dots, y_n, s_1', \dots s_p'\}$,
  where $y_1, \dots, y_n$ are variables distinct from the constants of
  $\mathcal{C}$ and from all the variables appearing in $R\{x_1,
  \dots, x_n, s_1, \dots, s_p\}$, and where we set \[ s_j' =
  T_j\langle f_1, \dots, f_n\rangle \langle s_j \rangle \] for $1 \leq
  j \leq p$.
\end{enumerate}
\endgroup

The basic idea seems to be that the variables $x_1, \dots, x_n$
represent sets containing labels, and the variables $s_1, \dots, s_p$
represent structures.  The relation $R\{x_1, \dots, x_n, s_1, \dots,
s_p\}$ holds precisely when the $s_j$ are all the (multi-sorted)
structures that can be built out of the labels in the $x_i$.
Condition (IS) ensures that we can bijectively swap out the label sets
$x_i$ for different ones.

Functoriality is embedded in the definition of an \emph{echelon
  construction} on page 2:
\todo{Translate echelon construction definition with commentary.}

\end{document}
