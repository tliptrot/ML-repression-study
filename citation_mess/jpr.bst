%$$$ jpr.bst (02.27.2013)
%$$$ update (07.05.2013)
%$$$ update (11.17.2014) 
%$$$ update (05.20.2015) 
% // Original version by Steven Miller
% // Tweaks made by Baobao Zhang and Allan Dafoe (Yale University).
%
% This *.bst file will allow the following JPR citations.
% 1) ARTICLE
% 2) BOOK
% 3) INCOLLECTION
% 4) UNPUBLISHED (please use techreport)
% 5) WEB RESOURCES (please use misc)
% 6) NEWS ARTICLES (please use articlenews)
%
% Original GitHub repo by Steven Miller: https://github.com/svmiller/jpr-bst-file
% New GitHub fork by Baobao Zhang: https://github.com/13bzhang/jpr-bst-file


ENTRY
  { address
    author
    booktitle
    chapter
    edition
    editor
    howpublished
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    URL
    volume
    year
  }
  {}
  { label extra.label sort.label }

INTEGERS { output.state before.all mid.sentence after.sentence after.block }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}

STRINGS {ss tt fm}
FUNCTION {format.grepmi}
{
    'ss :=   %% Make a copy of the name in s
    %% Extract the First (and possible Medium) names
    %% and store it in variable fm
    ss #1 "{ff}" format.name$ 'fm :=

    %% Note that now fm could contain:
    %% * An empty string ("") if the author has no first name (only Last name was provided)
    %% * A single word (like "First") if the author has no medium name
    %% * A sequence of words (like "First Medium")
    %% For the last case we want to abbreviate "Medium", without dot

    %% Test if we are in the first case
    fm empty$ {
       % If empty (no first name), use the standard formatting
       ss #1 "{vv~}{ll}{, jj}{, ff}" format.name$
    }{ % Otherwise, attempt the trick
        %% Now the trick. Interpret "First Medium"
        %% as if "Medium" were a last name, and abbreviate it
        fm #1 "{ff }{ll}" format.name$ 'tt :=
           %% And store the result in tt
        %% Consider the particular case in which no Medium name is present
        %% In this case, "First" will be interpreted as a last name, and
        %% thus abbreviated. This can be detected because the resulting
        %% string has length 1
        tt text.length$ #1 > { %% If there was a medium name
            tt  %% Store the abbreviated version
            }{  %% Else store the original version of the name
            fm
            } if$

        %% After the above, the top of the stack will contain
        %% either "First" unabbreviated (if the author has not middle name)
        %% or "First M", as required
        'tt :=  %% Copy that value to tt

        %% Now complete the standard formatting of the author, omitting
        %% the first name part, which is stored in tt
        ss #1 "{vv~}{ll}{, jj}" format.name$
        %% And concatenate to it the value of tt
        ", " *
        tt *
  }if$   %% If the trick has to be done
}

STRINGS { s t u f }

FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { " " * write$ }
    { output.state after.block =
  { add.period$ write$
    newline$
    "\newblock " write$
  }
  { output.state before.all =
      'write$
      { add.period$ " " * write$ }
    if$
  }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {write.url}
{ url empty$
    { skip$ }
    { "({" url * "})" * write$}
  if$
}



FUNCTION {special.output.nonnull}
{ 's :=
  output.state mid.sentence =
    { "  " * write$ }
    { output.state after.block =
        { ": " write$
          newline$
          "\newblock " write$
        }
        { output.state before.all =
            'write$
            { ": " * write$
        }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output.nonnull.colon}
{ 's :=
  output.state mid.sentence =
    { ": " * write$ }
    { output.state after.block =
    { add.period$ write$
      newline$
      "\newblock " write$
    }
    { output.state before.all =
        'write$
        { add.period$ " " * write$ }
      if$
    }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output.nonnull.space}
{ 's :=
  output.state mid.sentence =
    { "\ " * write$ }
    { output.state after.block =
    { add.period$ write$
      newline$
      "\newblock " write$
    }
    { output.state before.all =
        'write$
        { add.period$ " " * write$ }
      if$
    }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {special.output}
{ duplicate$ empty$
    'pop$
    'special.output.nonnull
  if$
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

FUNCTION {output.check.colon}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull.colon
  if$
}

FUNCTION {output.check.space}
{ 't :=
  duplicate$ empty$
    { pop$ "empty " t * " in " * cite$ * warning$ }
    'output.nonnull.space
  if$
}

FUNCTION {output.year.check}
{ year empty$
     { "empty year in " cite$ * warning$ }
     { write$
        " (" year * extra.label * ")" *
%        'output.nonnull.space
       mid.sentence 'output.state :=
     }
  if$
}


FUNCTION {output.bibitem}
{ newline$

  "\bibitem[" write$
  label write$
  "]{" write$

  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}

FUNCTION {fin.entry}
{ add.period$
  write$
  newline$
}

FUNCTION {new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}

FUNCTION {new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
    'skip$
    { after.sentence 'output.state := }
      if$
    }
  if$
}

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION {new.block.checka}
{ empty$
    'skip$
    'new.block
  if$
}

FUNCTION {new.block.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.block
  if$
}

FUNCTION {new.sentence.checka}
{ empty$
    'skip$
    'new.sentence
  if$
}

FUNCTION {new.sentence.checkb}
{ empty$
  swap$ empty$
  and
    'skip$
    'new.sentence
  if$
}

FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {underline}
{ duplicate$ empty$
  { pop$ "" }
  { "\underline{" swap$ * "}" * }
  if$
}

FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "{\em " swap$ * "}" * }
  if$
}


FUNCTION {emphasize.space}
{ duplicate$ empty$
    { pop$ "" }
    { "{ " swap$ * "\/}" * }
  if$
}

INTEGERS { nameptr namesleft numnames }



FUNCTION {format.names}
{ 's :=
  #1 'nameptr :=               % nameptr = 1;
  s num.names$ 'numnames :=    % numnames = num.name$(s);
  numnames 'namesleft :=
    { namesleft #0 > }

    { s nameptr "{vv~}{ll}{, jj}{, f}" format.name$
      % Postprocess result
      format.grepmi 't :=
      nameptr #1 >
        { namesleft #1 >
              { "; " * t * }
               { numnames #2 >
                    { "" * }
                    'skip$
                  if$
                  t "others" =
                        { " et~al." * }
%                       { " and " * t * }
%                       { ", \& " * t * }
                        { " \& " * t * }

                      if$
                }
               if$
             }
            't
        if$

        nameptr #1 + 'nameptr :=          % nameptr += 1;
        namesleft #1 - 'namesleft :=      % namesleft =- 1;
    }
  while$
}

FUNCTION {format.names.fml}
{ 's :=
  #1 'nameptr :=               % nameptr = 1;
  s num.names$ 'numnames :=    % numnames = num.name$(s);
  numnames 'namesleft :=
    { namesleft #0 > }

%   { s nameptr "{vv~}{ll}{, jj}{, ff}" format.name$ 't :=
    { s nameptr "{ff }{vv~}{ll}{, jj}" format.name$ 't :=
      nameptr #1 >
        { namesleft #1 >
              { "; " * t * }
               { numnames #2 >
                    { "  " * }
                    'skip$
                  if$
                  t "others" =
                        { " et~al." * }
%                        { " and " * t * }
                        { " \& " * t * }
                      if$
                }
               if$
             }
            't
        if$
        nameptr #1 + 'nameptr :=          % nameptr += 1;
        namesleft #1 - 'namesleft :=      % namesleft =- 1;
    }
  while$
}


FUNCTION {format.rem.names}
{ 's :=
  'f :=
  #2 'nameptr :=
  s num.names$ 'numnames :=
  numnames #1 - 'namesleft :=
    { namesleft #0 > }
    { s nameptr f format.name$   % <-----
      format.grepmi               % <-----
      #1 f format.name$ 't :=    
      nameptr #1 >
      {numnames #3 <
        {namesleft #1 >
        { ", " * t * }
        { t "others" =
        { " et~al." * }
        { "  \& " * t * }
          if$
        }
      if$
    }
      { nameptr #3 <
              { "; " * t * }
        { namesleft #1 >
              { ", " * t * }
               { numnames #2 >
                    { "  " * }
                    'skip$
                  if$
                  t "others" =
                        { " et~al." * }
%                        { " and " * t * }
                        { " \& " * t * }
                      if$
                }
               if$
             }
          if$
        }
        if$
        }
            't
        if$
        nameptr #1 + 'nameptr :=          % nameptr += 1;
        namesleft #1 - 'namesleft :=      % namesleft =- 1;
    }
  while$
}


FUNCTION {format.authors}
{ author empty$
    { "" }
    { author #1 "{vv~}{ll}{, jj}{, ff}" format.name$
      format.grepmi                                    
      #1 "{vv~}{ll}{, jj}{, ff}" format.name$          
      "{ff }{vv~}{ll}{, jj}" author format.rem.names
    }
  if$
}


FUNCTION {format.authors.2}
{ author empty$
    { "" }
    { author #1 "{ff }{ll}" format.name$
      format.grepmi                                    
      #1 "{ff }{ll}" format.name$          
      "{ff }{vv~}{ll}{, jj}" author format.rem.names
    }
  if$
}

FUNCTION {format.key}
{ empty$
    { key field.or.null }
    { "" }
  if$
}

FUNCTION {format.editors.fml}
{ editor empty$
    { "" }
    { editor format.names.fml
      editor num.names$ #1 >
    { " (eds)" * }
    { " (ed.)" * }
      if$
    }
  if$
}

FUNCTION {format.editors}
{ editor empty$
    { "" }
    { editor format.names
      editor num.names$ #1 >
    { " (eds)" * }
    { " (ed.)" * }
      if$
    }
  if$
}


FUNCTION {format.editors.dot}
{ editor empty$
    { "" }
    { editor #1 "{vv~}{ll}{, jj}{, ff}" format.name$
      format.grepmi                                    
      #1 "{vv~}{ll}{, jj}{, ff}" format.name$          
      "{ff }{vv~}{ll}{, jj}" editor format.rem.names
      editor num.names$ #1 >
    { ", eds" * }
    { ", ed." * }
      if$
    }
  if$
}



FUNCTION {format.title}
{ title empty$
    { "" }
    { title "t" change.case$ }
%    { title add.period$ }
  if$
}

% Note that the APA style requres case changes
% in article titles. The following does not
% change cases. If you perfer it, uncomment the
% following and comment out the above.

%FUNCTION {format.title}
%{ title empty$
%    { "" }
%    { title }
%  if$
%}

FUNCTION {n.dashify}
{ 't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
    { t #1 #2 substring$ "--" = not
        { "--" *
          t #2 global.max$ substring$ 't :=
        }
        {   { t #1 #1 substring$ "-" = }
        { "-" *
          t #2 global.max$ substring$ 't :=
        }
          while$
        }
      if$
    }
    { t #1 #1 substring$ *
      t #2 global.max$ substring$ 't :=
    }
      if$
    }
  while$
}

FUNCTION {format.btitle}
{ edition empty$
  { title  }
  { title empty$
    { title  }
    { "{ " title * "\/} (" * edition * " ed.)" * "." * }
    if$
  }
  if$
}

FUNCTION {format.emphasize.booktitle}
{ edition empty$
  { booktitle  }
  { booktitle empty$
    { booktitle  }
    { "{ "  booktitle * "\/} (" * edition * " ed.)" * "." * }
    if$
  }
  if$
}

FUNCTION {tie.or.space.connect}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}

FUNCTION {either.or.check}
{ empty$
    'pop$
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}

FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { "volume" volume tie.or.space.connect
      series empty$
        'skip$
        { " of " * series  * }
      if$
      "volume and number" number either.or.check
    }
  if$
}

FUNCTION {format.number.series}
{ volume empty$
    { number empty$
    { series field.or.null }
    { output.state mid.sentence =
        { "number" }
        { "Number" }
      if$
      number tie.or.space.connect
      series empty$
        { "there's a number but no series in " cite$ * warning$ }
        { " in " * series * }
      if$
    }
      if$
    }
    { "" }
  if$
}

FUNCTION {format.edition}
{ edition empty$
    { "" }
    { output.state mid.sentence =
        { edition "l" change.case$ " edition" * }
        { edition "t" change.case$ " edition" * }
      if$
    }
  if$
}

INTEGERS { multiresult }

FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
    { #1 'multiresult := }
    { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {format.org.or.pub}
{ 't :=
  ""
  address empty$ t empty$ and
    'skip$
    {
      address "address" *
      t empty$
        'skip$
        { address empty$
            'skip$
            { ": " * }
          if$
          t *
        }
      if$
    }
  if$
}

FUNCTION {format.url}
{
  url
  duplicate$ empty$
    { pop$ "" }
    { "({" swap$ * "})" * }
  if$
}

FUNCTION {format.publisher.address}
{ publisher "publisher" 
 format.org.or.pub
}

FUNCTION {format.pages}
{ pages empty$
  { "" }
  { pages multi.page.check
%    { "pages" pages n.dashify tie.or.space.connect }
%    { "page" pages tie.or.space.connect }
% { "(pp.\" pages n.dashify tie.or.space.connect }
% { "(pp.\" pages tie.or.space.connect }
  { ", " pages n.dashify tie.or.space.connect }
  { ", " pages tie.or.space.connect }
    if$
  "." *
  }
  if$
}

% By Young (and Spencer)
FUNCTION {format.vol.num.pages}
{ number empty$
    { volume empty$
       'skip$
       { "{\em " volume * "}" * }
%    { "\underline{" volume * "}" * }
      if$
    }
    { volume emphasize.space
      number empty$
       {"there's a number but no volume in " cite$ * warning$ }
       { "(" number * ")" * * }
      if$
    }
  if$
  pages empty$
    'skip$
    { duplicate$ empty$
      { pop$ format.pages }
      { ": " * pages n.dashify * }
      if$
    }
  if$
}

FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
        { "chapter" }
        { type "l" change.case$ }
      if$
      chapter tie.or.space.connect
      pages empty$
        'skip$
        { ", " * format.pages * }
      if$
    }
  if$
}

FUNCTION {format.chapter.pages.incoll}
{ chapter empty$
    'format.pages
    { type empty$
        { "chapter" }
        { type "l" change.case$ }
      if$
      chapter tie.or.space.connect
      pages empty$
        'skip$
        { " " * format.pages * }
      if$
    }
  if$
}

FUNCTION {format.in.ed.booktitle}
{ booktitle empty$
  { "" }
  { editor empty$
    { "In: " format.emphasize.booktitle * }
    { "In: " format.editors * " " * format.emphasize.booktitle * }
    if$
  }
  if$
}

FUNCTION {format.in.ed.booktitle.incoll}
{ booktitle empty$
  { "" }
  { editor empty$
    { "In: " format.emphasize.booktitle  * }
    { "In: " format.editors.fml * " " *  format.emphasize.booktitle emphasize * }
    if$
  }
  if$
}

FUNCTION {format.thesis.type}
{ type empty$
    'skip$
    { pop$
      type "t" change.case$
    }
  if$
}

FUNCTION {format.tr.number}
{ type empty$
    { "Technical Report" }
    'type
  if$
  number empty$
    { "t" change.case$ }
    { number tie.or.space.connect }
  if$
}

FUNCTION {format.article.crossref}
{ "In"
  "\cite{" * crossref * "}" *
}

FUNCTION {format.crossref.editor}
{ editor #1 "{vv~}{ll}" format.name$
  editor num.names$ duplicate$
  #2 >
    { pop$ " et~al." * }
    { #2 <
    'skip$
    { editor #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
        { " et~al." * }
        { " and " * editor #2 "{vv~}{ll}" format.name$ * }
      if$
    }
      if$
    }
  if$
}

FUNCTION {format.book.crossref}
{ volume empty$
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      "In "
    }
    { "Volume" volume tie.or.space.connect
      " of " *
    }
  if$
  editor empty$
  editor field.or.null author field.or.null =
  or
    { key empty$
    { series empty$
        { "need editor, key, or series for " cite$ * " to crossref " *
          crossref * warning$
          "" *
        }
        { "{\em " * series * "\/}" * }
      if$
    }
    { key * }
      if$
    }
    { format.crossref.editor * }
  if$
  " \cite{" * crossref * "}" *
}

FUNCTION {format.incoll.inproc.crossref}
{ "In"
  " \cite{" * crossref * "}" *
}

FUNCTION {article}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output          % added
  output.year.check              % added
%  new.block
  format.title 
  "title" output.check
  new.block
  crossref missing$
    { journal emphasize "journal" output.check 
      format.vol.num.pages output
    }
    { format.article.crossref output.nonnull 
      format.pages output
    }
  if$
  %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {articlenews}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output          % added
  output.year.check              % added
%  new.block
  format.title 
  "title" output.check
  new.block
  journal emphasize "journal" output.check 
  %new.block
  month output
  note output
  format.url output
  fin.entry
}

FUNCTION {book}
{ output.bibitem
  author empty$
    { format.editors.dot 
    "author and editor" output.check }
    { format.authors 
    output.nonnull
      crossref missing$
      { "author and editor" editor either.or.check }
      'skip$
      if$
    }
  if$
  output.year.check       % added
%  new.block
  format.btitle 
  emphasize "title" output.check
  crossref missing$
    { format.bvolume output
      new.block
      format.number.series output
      new.sentence
      address output
      publisher "publisher" output.check.colon

    }
    { new.block
      format.book.crossref output.nonnull
    }
  if$
%  format.edition output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {booklet}
{ output.bibitem
  format.authors output
  author format.key output          % added
  output.year.check                 % added
  new.block
  format.title 
  "title" output.check
  new.block
  howpublished output
  address output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  author empty$
    { format.editors.dot 
      "author and editor" output.check 
    }
    { format.authors output.nonnull
      crossref missing$
    { "author and editor" editor either.or.check }
    'skip$
      if$
    }
  if$
  output.year.check                 % added
%  new.block
  format.btitle 
  "title" output.check
  crossref missing$
    { format.bvolume output
      format.chapter.pages 
      "chapter and pages" output.check
      new.block
      format.number.series output
      new.sentence
      address output
      publisher 
      "publisher" output.check.colon
    }
    { format.chapter.pages "chapter and pages" output.check
      new.block
      format.book.crossref output.nonnull
    }
  if$
%  format.edition output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  format.authors
  "author" output.check
  author format.key output       % added
  output.year.check              % added
%  new.block
  format.title 
  "title" output.check
  new.block
  crossref missing$
  { format.in.ed.booktitle.incoll 
    "booktitle" output.check.colon

    format.bvolume output
    format.number.series output

 %   format.chapter.pages special.output
    new.sentence
    address output
    publisher "publisher" output.check.colon
    before.all 'output.state := %% added
format.chapter.pages special.output
  }
  { format.incoll.inproc.crossref 
  output.nonnull
    format.chapter.pages output
  }
  if$
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {inproceedings}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output            % added
  output.year.check                   % added
  new.block
  format.title 
  "title" output.check
  new.block
  crossref missing$
    { format.in.ed.booktitle 
    "booktitle" output.check
      format.bvolume output
      format.number.series output
      format.pages output
      address output
      new.sentence
      organization output
      publisher output
    }
    { format.incoll.inproc.crossref output.nonnull
      format.pages output
    }
  if$
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {conference} { inproceedings }

FUNCTION {manual}
{ output.bibitem
  format.authors output
  author format.key output          % added
  output.year.check                 % added
  new.block
  format.btitle 
  "title" output.check
  organization address new.block.checkb
% Reversed the order of "address" and "organization", added the ":".
  address output
  organization "organization" output.check.colon
%  address output
%  ":" output
%  organization output
%  format.edition output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {mastersthesis}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output          % added
  output.year.check                 % added
  new.block
  format.title 
  "title" output.check
  new.block
  "Master's thesis" format.thesis.type output.nonnull
  school "school" output.check
  address output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  format.authors output
  author format.key output            % added
   output.year.check
  %title howpublished new.block.checkb
  format.title output
  %new.block
  %howpublished output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {phdthesis}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output            % added
  output.year.check                   % added
  new.block
  format.btitle 
  "title" output.check
  new.block
  "PhD thesis" format.thesis.type output.nonnull
  school "school" output.check
  address output
  %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {proceedings}
{ output.bibitem
  editor empty$
    { organization output }
    { format.editors.dot output.nonnull }
  if$
  author format.key output             % added
  output.year.check                    % added
  new.block
  format.btitle 
  "title" output.check
  format.bvolume output
  format.number.series output
  address output
  new.sentence
  organization output
  publisher output
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output             % added
  output.year.check                    % added
  format.title 
  "title" output.check
  new.block
  format.tr.number output.nonnull
  new.block
  institution 
  "institution" output.check
  note output
  format.url output
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors 
  "author" output.check
  author format.key output              % added
  output.year.check                      % added
  %new.block
  format.title 
  "title" output.check
 %new.block
  note output
  format.url output
  fin.entry
}

FUNCTION {default.type} { misc }

MACRO {jan} {"January"}

MACRO {feb} {"February"}

MACRO {mar} {"March"}

MACRO {apr} {"April"}

MACRO {may} {"May"}

MACRO {jun} {"June"}

MACRO {jul} {"July"}

MACRO {aug} {"August"}

MACRO {sep} {"September"}

MACRO {oct} {"October"}

MACRO {nov} {"November"}

MACRO {dec} {"December"}

MACRO {acmcs} {"ACM Computing Surveys"}

MACRO {acta} {"Acta Informatica"}

MACRO {ai} {"Artificial Intelligence"}

MACRO {cacm} {"Communications of the ACM"}

MACRO {ibmjrd} {"IBM Journal of Research and Development"}

MACRO {ibmsj} {"IBM Systems Journal"}

MACRO {ieeese} {"IEEE Transactions on Software Engineering"}

MACRO {ieeetc} {"IEEE Transactions on Computers"}

MACRO {ieeetcad}
 {"IEEE Transactions on Computer-Aided Design of Integrated Circuits"}

MACRO {ipl} {"Information Processing Letters"}

MACRO {jacm} {"Journal of the ACM"}

MACRO {jcss} {"Journal of Computer and System Sciences"}

MACRO {scp} {"Science of Computer Programming"}

MACRO {sicomp} {"SIAM Journal on Computing"}

MACRO {tocs} {"ACM Transactions on Computer Systems"}

MACRO {tods} {"ACM Transactions on Database Systems"}

MACRO {tog} {"ACM Transactions on Graphics"}

MACRO {toms} {"ACM Transactions on Mathematical Software"}

MACRO {toois} {"ACM Transactions on Office Information Systems"}

MACRO {toplas} {"ACM Transactions on Programming Languages and Systems"}

MACRO {tcs} {"Theoretical Computer Science"}

READ

FUNCTION {sortify}
{ purify$
  "l" change.case$
}

INTEGERS { len }

FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}

INTEGERS { fullptr numfull fullsleft }

STRINGS { u1 u2 }

FUNCTION {my.full.label}
{ 
% Initialize 'u1','u2','s'.
  "" 'u1 :=                                       
  "" 'u2 :=                     
  's :=                      

% Initialize 'fullptr','numfull','fullsleft'.
  #1 'fullptr :=                         
  s num.names$ 'numfull :=   
  numfull 'fullsleft :=          

% enter the while loop which generates the first-citation information.
% while we have names left, 
%     format the next name
%   if this is the next-to-last name, tack the ampersand on the end
%   else if this isn't the last name, tack the comma on the end.
%   concatenate the next name onto the first-citation string.
%   update the counters.

  { fullsleft #0 > }
  { s fullptr "{vv~}{ll}" format.name$ 'u1 :=  
     fullsleft #2 =
       { u1 " \& " * 'u1 := }
      { fullsleft #2 > 
           { u1 ", " * 'u1 := }
           'skip$
         if$
        }
    if$
     u2 u1 * 'u2 :=
     fullptr #1 + 'fullptr :=         
     fullsleft #1 - 'fullsleft :=  
  }
  while$

% push 'u2' onto the stack -- our first-citation information.
  u2        
}

FUNCTION {space.word}
{ " " swap$ * " " * }

FUNCTION {bbl.and}
{ "and"}


FUNCTION {bbl.etal}
{ "et~al." }

FUNCTION {format.lab.names}
{'s :=
 "" 't :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv~}{ll}" format.name$
      't :=
      nameptr #1 >
        {
          nameptr #2 =
          numnames #3 > and
            { "others" 't :=
              #1 'namesleft := }
            'skip$
          if$
          namesleft #1 >
            { ", " * t * }
            {
              s nameptr "{ll}" format.name$ duplicate$ "others" =
                { 't := }
                { pop$ }
              if$
              t "others" =
                {
                  " " * bbl.etal *
                }
                {
                  "\&"
                  space.word * t *
                }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {author.key.label}
{ author empty$
    { key empty$
          { cite$ #1 #3 substring$ }
         'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {editor.key.label}
{ editor empty$
    { key empty$
          { cite$ #1 #3 substring$ }
          'key
        if$
     }
     { editor format.lab.names }
  if$
}

FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
          { key empty$
               { cite$ #1 #3 substring$ }
             'key
           if$
         }
          { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {calc.label}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
          'editor.key.label
          'author.key.label
        if$
    }
  if$
  duplicate$    

  author my.full.label  % generate the first-citation information.

  "\protect\citeauthoryear{" swap$ * "}{" * swap$ * "}{" *
  year field.or.null purify$ #-1 #11 substring$ *  
  'label :=
  year field.or.null purify$ #-1 #4 substring$ *
  sortify 'sort.label :=
}

FUNCTION {sort.format.names}
{ 's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { nameptr #1 >
          { "   " * }
         'skip$
      if$
      s nameptr "{vv{ } }{ll{ }}{  f{ }}{  jj{ }}" format.name$ 't :=
      nameptr numnames = t "others" = and
          { "et al" * }
          { t sortify * }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {sort.format.title}
{ 't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}

FUNCTION {author.sort}
{ author empty$
    { key empty$
          { "to sort, need author or key in " cite$ * warning$
              ""
         }
         { key sortify }
      if$
    }
    { author sort.format.names }
  if$
}

FUNCTION {editor.sort}
{ editor empty$
    { key empty$
         { "to sort, need editor or key in " cite$ * warning$
           ""
         }
         { key sortify }
      if$
    }
    { editor sort.format.names }
  if$
}

FUNCTION {author.editor.sort}
{ author empty$
    { editor empty$
         { key empty$
             { "to sort, need author, editor, or key in " cite$ * warning$
               ""
             }
             { key sortify }
           if$
         }
         { editor sort.format.names }
      if$
   }
   { author sort.format.names }
  if$
}

FUNCTION {presort}
{ calc.label
  label sortify
  "    "
  *
  type$ "book" =
  type$ "inbook" =
  or
    'author.editor.sort
    { type$ "proceedings" =
          'editor.sort
          'author.sort
      if$
    }
  if$
  #1 entry.max$ substring$        % added for newapa
  'sort.label :=                  % added for newapa
  sort.label                      % added for newapa
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}

ITERATE {presort}

SORT             % by label, sort.label, title --- for final label calculation

STRINGS { last.label next.extra }

INTEGERS { last.extra.num }

FUNCTION {initialize.extra.label.stuff}
{ #0 int.to.chr$ 'last.label :=
  "" 'next.extra :=
  #0 'last.extra.num :=
}

FUNCTION {forward.pass}
{ last.label label =
     { last.extra.num #1 + 'last.extra.num :=
       last.extra.num int.to.chr$ 'extra.label :=
     }
     { "a" chr.to.int$ 'last.extra.num :=
       "" 'extra.label :=
       label 'last.label :=
    }
  if$
}

FUNCTION {reverse.pass}
{ next.extra "b" =
    { "a" 'extra.label := }
     'skip$
  if$
  label extra.label * "}" * 'label :=   
  extra.label 'next.extra :=
}

EXECUTE {initialize.extra.label.stuff}

ITERATE {forward.pass}

REVERSE {reverse.pass}

FUNCTION {bib.sort.order}
{ sort.label
  "    "
  *
  year field.or.null sortify
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}

ITERATE {bib.sort.order}

SORT             % by sort.label, year, title --- giving final bib. order.

FUNCTION {begin.bib}

{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thebibliography}{}" write$ newline$
}


EXECUTE {begin.bib}

EXECUTE {init.state.consts}

ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
  "\end{thebibliography}" write$ newline$
}

EXECUTE {end.bib}
