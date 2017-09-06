-- Configuration for bottom xmobar   -*- Haskell -*-
-- Time-stamp: <2017-08-29 21:30:27 PDT xoddf2>

Config { font         = "xft:Terminus-11"
       , bgColor      = "#242424"
       , fgColor      = "#E5E5E5"
       , position     = BottomW R 90
       , lowerOnStart = True
       , commands     = [ Run Battery [] 10
                        , Run Cpu ["-t","CPU: <total>%","-L","3","-H","50","--normal","#8BDE58","--high","#DD424C"] 50
                        , Run Memory ["-t","Mem: <used>M (<usedratio>%)"] 50
                        , Run Swap ["-t","Swap: <used>M (<usedratio>%)"] 50
                        , Run CoreTemp ["-t","Temp: <core0>C/<core1>C","-L","40","-H","75","--high","#DD424C","--low","#7FE0DC"] 50
                        , Run Mail [(" <fc=#515151>|</fc> Mail: ", "~/Mail/Gmail/INBOX")] "mail"
                        , Run Weather "KRDD" ["-t","<station>: <tempC>C <skyCondition>","-L","15","-H","30","--normal","#8BDE58","--high","#DD424C","--low","lightblue"] 18000
                        , Run StdinReader
                        ]
       , sepChar      = "%"
       , alignSep     = "}{"
       , template     = "%cpu% <fc=#515151>|</fc> %memory% * %swap% <fc=#515151>|</fc> %coretemp% }{ %KRDD%%mail% <fc=#515151>|</fc> %battery%"
       }
