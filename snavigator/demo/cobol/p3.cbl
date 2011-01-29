00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID.
00003      Demo 1.
00004  AUTHOR.
00005      Bob Noweck.
00006  INSTALLATION.
00007      IBM PC COMPATABLES.
00008  DATE-WRITTEN.
00009      APRIL 22 1990.
00010  DATE-COMPILED.
00011      APRIL 22 1990.
00012  SECURITY.
00013      NONE.
00014  ENVIRONMENT DIVISION.
00015  CONFIGURATION SECTION.
00016  SOURCE-COMPUTER.
00017      WYSE-386 WITH DEBUGGING MODE.
00018  OBJECT-COMPUTER.
00019      WYSE-386 MEMORY SIZE 384000 WORDS.
00020  SPECIAL-NAMES.
00121  INPUT-OUTPUT SECTION.
00122  FILE-CONTROL.
00133  DATA DIVISION.
00134  FILE SECTION.
00180  WORKING-STORAGE SECTION.
00181  01 TESTe PIC X.
00188  SCREEN SECTION.
00189  01 CLRSCR.
          03 LINE 1 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 1 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 2 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 2 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 3 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 3 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 4 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 4 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 5 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 5 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 6 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 6 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 7 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 7 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 8 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 8 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 9 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 9 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 10 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 10 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 11 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 11 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 12 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 12 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 13 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 13 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 14 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 14 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 15 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 15 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 16 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 16 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 17 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 17 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 18 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 18 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 19 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 19 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 20 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 20 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 21 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 21 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 22 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 22 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
          03 LINE 23 COLUMN 1
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2 
             VALUE '                                        '.
          03 LINE 23 COLUMN 40
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '                                        '.
       01 CBL-HDR.
          03 LINE 4 COLUMN 20
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'CCC'.     
          03 LINE 4 COLUMN 28
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'OOO'.     
          03 LINE 4 COLUMN 35
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'BBBB'.    
          03 LINE 4 COLUMN 44
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'OOO'.     
          03 LINE 4 COLUMN 51
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'L'.       
          03 LINE 5 COLUMN 19
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.       
          03 LINE 5 COLUMN 23
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.        
          03 LINE 5 COLUMN 27
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.       
          03 LINE 5 COLUMN 31
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.        
          03 LINE 5 COLUMN 35
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'B'.       
          03 LINE 5 COLUMN 39
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'B'.        
          03 LINE 5 COLUMN 43
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.       
          03 LINE 5 COLUMN 47 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.       
          03 LINE 5 COLUMN 51 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'L'.        
          03 LINE 6 COLUMN 19 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.        
          03 LINE 6 COLUMN 27 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.       
          03 LINE 6 COLUMN 31 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.       
          03 LINE 6 COLUMN 35 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'BBBB'.    
          03 LINE 6 COLUMN 43 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.        
          03 LINE 6 COLUMN 47 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.         
          03 LINE 6 COLUMN 51 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'L'.          
          03 LINE 7 COLUMN 19 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.          
          03 LINE 7 COLUMN 27 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.           
          03 LINE 7 COLUMN 31 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.          
          03 LINE 7 COLUMN 35 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'BBBB'.          
          03 LINE 7 COLUMN 43 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.         
          03 LINE 7 COLUMN 47 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.         
          03 LINE 7 COLUMN 51 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'L'.          
          03 LINE 8 COLUMN 19 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.           
          03 LINE 8 COLUMN 23 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'C'.          
          03 LINE 8 COLUMN 27 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.          
          03 LINE 8 COLUMN 31 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.          
          03 LINE 8 COLUMN 35 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'B'.         
          03 LINE 8 COLUMN 39 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'B'.         
          03 LINE 8 COLUMN 43 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.        
          03 LINE 8 COLUMN 47 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'O'.         
          03 LINE 8 COLUMN 51 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'L'.          
          03 LINE 9 COLUMN 20 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'CCC'.          
          03 LINE 9 COLUMN 28 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'OOO'.       
          03 LINE 9 COLUMN 35 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'BBBB'.        
          03 LINE 9 COLUMN 44 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'OOO'.        
          03 LINE 9 COLUMN 51 
             FOREGROUND-COLOR 4
             BACKGROUND-COLOR 1
             VALUE 'LLLLL'.    
       01 MORE-INFO.
          03 LINE 12 COLUMN 1
             FOREGROUND-COLOR 7
             BACKGROUND-COLOR 2
             VALUE '________________________________________'.
          03 LINE 12 COLUMN 40
             FOREGROUND-COLOR 7
             BACKGROUND-COLOR 2
             VALUE '________________________________________'.
      
       01 MORE-INFO-1.
          03 LINE 14 COLUMN 7
             HIGHLIGHT
             UNDERLINE
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'Now with COBOL version 5.30 you have the'.
          03 LINE 14 COLUMN 48
             highlight
             UNDERLINE
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             value 'ability use to screen IO.'.
       01 box-1.
          03 line 18 column 20
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '                    '.
          03 line 18 column 40
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '                  '.


          03 LINE 19 COLUMN 20
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '      User Supported Shareware        '.
          03 LINE 20 COLUMN 20
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '                                      '.
          03 LINE 21 COLUMN 20
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '   Copywrite: R. E. Noweck 1990       '.
          03 LINE 22 COLUMN 20
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '                    '.
          03 line 22 column 40
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 7
             VALUE '                  '.



       01 cpywrite.
          03 LINE 20 COLUMN 31
             BLINK             
             FOREGROUND-COLOR 6
             BACKGROUND-COLOR 7
             VALUE 'COBOL VER. 5.30'.
    
       01 NEXT-SCREEN.
          03 LINE 1 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'ANSI STANDARD COBOL has gone thru many '.
          03 LINE 3 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'changes in the years since it was first'.
          03 LINE 5 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'written.  Now I have added fixed ALL   '.
          03 LINE 7 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'the BUGS that seemed to plague me.  In '.
          03 LINE 9 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'keeping with the shareware concept this'.
          03 LINE 11 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'compiler is offered on a TRY and BUY   '.
          03 LINE 13 COLUMN 20
             highlight
             FOREGROUND-COLOR 9
             BACKGROUND-COLOR 2
             VALUE 'basis.                                 '.
          03 LINE 16 COLUMN 20
             highlight
             FOREGROUND-COLOR 6
             BACKGROUND-COLOR 2
             VALUE 'If you like this product then PLEASE   '.
          03 LINE 17 COLUMN 20
             highlight
             FOREGROUND-COLOR 6
             BACKGROUND-COLOR 2
             VALUE 'send your donation of $25.00 to :      '.
          03 LINE 18 COLUMN 20
             highlight
             FOREGROUND-COLOR 7
             BACKGROUND-COLOR 2
             VALUE '      Coastal Area Support Team        '.
          03 LINE 19 COLUMN 20
             highlight
             FOREGROUND-COLOR 7
             BACKGROUND-COLOR 2
             VALUE '           115 Lynton Rd.              '.
          03 LINE 20 COLUMN 20
             highlight
             FOREGROUND-COLOR 7
             BACKGROUND-COLOR 2
             VALUE '          Jesup, Ga. 31545             '.
          03 LINE 22 COLUMN 20
             highlight
             FOREGROUND-COLOR 5
             BACKGROUND-COLOR 2
             VALUE '  <c> 1990 R.E. Noweck   912-427-0756  '.

       01 PRESS.
          03 LINE 23 COLUMN 5
             BLINK
             FOREGROUND-COLOR 6
             BACKGROUND-COLOR 2
             VALUE 'PRESS RETURN TO CONTINUE'.

20200  PROCEDURE DIVISION.
20300  MAIN.
20310      DISPLAY CLRSCR.
20320      DISPLAY CBL-HDR.
20321      DISPLAY MORE-INFO.
20322      display more-info-1.
20330      DISPLAY BOX-1.
20333      DISPLAY CPYWRITE.
20340      DISPLAY PRESS.
20341      ACCEPT ( 23 , 30 ) TESTe.
20350      DISPLAY CLRSCR.
20360      DISPLAY NEXT-SCREEN.
20400      STOP RUN.

