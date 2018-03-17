-- Configuration for bottom xmobar   -*- Haskell -*-
-- Time-stamp: <2018-03-17 02:27:05 PDT xoddf2>

Config { font         = "xft:Terminus-11"
       , bgColor      = "#3F3F3F"
       , fgColor      = "#DCDCCC"
       , position     = BottomW R 90
       , lowerOnStart = True
       , commands     = [ Run Battery [] 10
                        , Run Cpu ["-t","CPU: <total>%","-L","3","-H","50","--normal","#C3BF9F","--high","#DCA3A3"] 50
                        , Run Memory ["-t","Mem: <used>M (<usedratio>%)"] 50
                        , Run Swap ["-t","Swap: <used>M (<usedratio>%)"] 50
                        , Run CoreTemp ["-t","Temp: <core0>C/<core1>C","-L","40","-H","75","--high","#DCA3A3","--low","#93E0E3"] 50
                        , Run Mail [(" <fc=#709080>|</fc> Mail: ", "~/local/var/mail/Gmail/INBOX")] "mail"
                        , Run Weather "KRDD" ["-t","<station>: <tempC>C <skyCondition>","-L","15","-H","30","--normal","#C3BF9F","--high","#DCA3A3","--low","#93E0E3"] 18000
                        , Run StdinReader
                        ]
       , sepChar      = "%"
       , alignSep     = "}{"
       , template     = "%cpu% <fc=#709080>|</fc> %memory% * %swap% <fc=#709080>|</fc> %coretemp% }{ %KRDD%%mail% <fc=#709080>|</fc> %battery%"
       }
