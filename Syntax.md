Email address syntax
====================

Since you need to consult a bunch of RFCs to figure this out,
here is a compilation of what it means to be an (internationalized) email address.

    [RFC5322]
    addr-spec       =   local-part "@" domain

An email address is a *local-part*, the `@` symbol, and then a *domain*.

Local-part
----------

    [RFC5322]
    local-part      =   dot-atom / quoted-string / obs-local-part

Dotted atoms:

    [RFC5322]
    dot-atom        =   [CFWS] dot-atom-text [CFWS]


    CFWS            =   (1*([FWS] comment) [FWS]) / FWS

    comment         =   "(" *([FWS] ccontent) [FWS] ")"

    ccontent        =   ctext / quoted-pair / comment

    ctext           =   %d33-39 /          ; Printable US-ASCII
                        %d42-91 /          ;  characters not including
                        %d93-126 /         ;  "(", ")", or "\"
                        obs-ctext

    [RFC6532]
                    =/  UTF8-non-ascii


    [RFC5322]
    dot-atom-text   =   1*atext *("." 1*atext)

    atext           =  ALPHA / DIGIT /    ; Printable US-ASCII
                       "!" / "#" /        ;  characters not including
                       "$" / "%" /        ;  specials.  Used for atoms.
                       "&" / "'" /
                       "*" / "+" /
                       "-" / "/" /
                       "=" / "?" /
                       "^" / "_" /
                       "`" / "{" /
                       "|" / "}" /
                       "~"
    [RFC6532]
                    =/ UTF8-non-ascii

    [RFC5234]
    ALPHA           =  %x41-5A / %x61-7A   ; A-Z / a-z
    DIGIT           =  %x30-39
                           ; 0-9

    [RFC6532]
    UTF8-non-ascii  =   UTF8-2 / UTF8-3 / UTF8-4

    [RFC3629]
    UTF8-2          = %xC2-DF UTF8-tail
    UTF8-3          = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
                      %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
    UTF8-4          = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
                      %xF4 %x80-8F 2( UTF8-tail )
    UTF8-tail       = %x80-BF

Quoted strings:

    [RFC5322]
    quoted-string   =   [CFWS]
                        DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                        [CFWS]

    FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                          ; Folding white space

    qcontent        =   qtext / quoted-pair

    qtext           =  %d33 /             ; Printable US-ASCII
                       %d35-91 /          ;  characters not including
                       %d93-126 /         ;  "\" or the quote character
                       obs-qtext

    [RFC6532]
                    =/  UTF8-non-ascii


    [RFC5322]
    quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp


    [RFC5234]
    DQUOTE          =  %x22
                             ; " (Double Quote)
    
    WSP             =  SP / HTAB
                             ; white space

    SP              =  %x20

    HTAB            =  %x09
                             ; horizontal tab

    CRLF            =  CR LF
                             ; Internet standard newline

    CR              =  %x0D
                             ; carriage return
    
    LF              =  %x0A
                             ; linefeed
    
    VCHAR           =  %x21-7E
                             ; visible (printing) characters

    [RFC6532]
                    =/  UTF8-non-ascii


Domain-part
-----------

A *domain* is a dot-separated list of *sub-domain*s.

    [RFC5322]
    domain          =   dot-atom / domain-literal / obs-domain

    domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]

    dtext           =   %d33-90 /          ; Printable US-ASCII
                        %d94-126 /         ;  characters not including
                        obs-dtext          ;  "[", "]", or "\"

    [RFC6532]
                    =/  UTF8-non-ascii


Obsolete syntax
===============

    [RFC5322]
    obs-local-part  =   word *("." word)
 
    atom            =   [CFWS] 1*atext [CFWS]

    word            =   atom / quoted-string
 
    obs-FWS         =   1*WSP *(CRLF 1*WSP)
 
    obs-ctext       =   obs-NO-WS-CTL
 
    obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                        %d11 /             ;  characters that do not
                        %d12 /             ;  include the carriage
                        %d14-31 /          ;  return, line feed, and
                        %d127              ;  white space characters
    
    obs-qtext       =   obs-NO-WS-CTL
 
    obs-qp          =   "\" (%d0 / obs-NO-WS-CTL / LF / CR)
  
    obs-dtext       =   obs-NO-WS-CTL / quoted-pair
