-- Configuration for top xmobar   -*- Haskell -*-
-- Time-stamp: <2018-03-06 19:32:19 PST xoddf2>

Config { font         = "xft:Terminus-11"
       , bgColor      = "#3F3F3F"
       , fgColor      = "#DCDCCC"
       , position     = TopW L 100
       , lowerOnStart = True
       , commands     = [ Run Date "%a %b %d %H:%M:%S %Z %Y" "date" 1
                        , Run StdinReader
                        ]
       , sepChar      = "%"
       , alignSep     = "}{"
       , template     = "%StdinReader% }{ <fc=#709080>|</fc> <fc=#93E0E3>%date%</fc>"
       }
