-- Configuration for top xmobar   -*- Haskell -*-
-- Time-stamp: <2017-08-29 21:30:05 PDT xoddf2>

Config { font         = "xft:Terminus-11"
       , bgColor      = "#242424"
       , fgColor      = "#E5E5E5"
       , position     = TopW L 100
       , lowerOnStart = True
       , commands     = [ Run Date "%a %b %d %H:%M:%S %Z %Y" "date" 1
                        , Run StdinReader
                        ]
       , sepChar      = "%"
       , alignSep     = "}{"
       , template     = "%StdinReader% }{ <fc=#515151>|</fc> <fc=#7FE0DC>%date%</fc>"
       }
