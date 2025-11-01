@ECHO OFF

REM  OS/2 command file to create miscellaneous files
REM  needed while building the GNU C compiler for OS/2 2.x.
REM    Copyright (C) 1993 Free Software Foundation, Inc.
REM    Contributed by Samuel Figueroa (figueroa@cs.nyu.edu)

REM This file is part of GNU CC.

REM GNU CC is free software; you can redistribute it and/or modify
REM it under the terms of the GNU General Public License as published by
REM the Free Software Foundation; either version 2, or (at your option)
REM any later version.

REM GNU CC is distributed in the hope that it will be useful,
REM but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
REM GNU General Public License for more details.

REM You should have received a copy of the GNU General Public License
REM along with GNU CC; see the file COPYING.  If not, write to
REM the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

REM Create the command file addtolib1.cmd, which is used in building gcc1.lib.
REM (Actually, gcc1.lib is not needed in the OS/2 version of gcc.)
IF EXIST addtolib1.cmd ATTRIB -H -S -R addtolib1.cmd
ECHO @ECHO %%1 >addtolib1.cmd
ECHO @CALL rm -f %%1.obj ^|^| GOTO END >>addtolib1.cmd
ECHO @%%3 -c %%4 %%5 %%6 %%7 %%8 %%9 -DL%%1 %%2 ^|^| GOTO END >>addtolib1.cmd
ECHO @MOVE libgcc1.obj %%1.* ^|^| GOTO END >>addtolib1.cmd
ECHO @LIB tmpgcc1 %%1; ^|^| GOTO END >>addtolib1.cmd
ECHO @CALL rm -f %%1.obj ^|^| GOTO END >>addtolib1.cmd
ECHO @ECHO %%1: done >>addtolib1.cmd
ECHO :END >>addtolib1.cmd

REM Create the command file addtolib2.cmd, which is used in building gcc2.lib.
IF EXIST addtolib2.cmd ATTRIB -H -S -R addtolib2.cmd
ECHO @ECHO %%1 >addtolib2.cmd
ECHO @%%3 -c -I. -Iconfig %%4 %%5 %%6 %%7 %%8 %%9 -g1 -DL%%1 -o %%1.obj %%2 ^|^| GOTO END >>addtolib2.cmd
ECHO @LIB tmpgcc2 -+%%1; ^|^| GOTO END >>addtolib2.cmd
ECHO @CALL rm -f %%1.obj ^|^| GOTO END >>addtolib2.cmd
ECHO @ECHO %%1: done >>addtolib2.cmd
ECHO :END >>addtolib2.cmd

REM Create a file called config.status to satisfy the makefile.
IF EXIST config.status ATTRIB -H -S -R config.status
ECHO. >config.status

REM Create the command file move-if-change.cmd, whose purpose is to simulate the
REM gcc shell script called move-if-change.
IF EXIST move-if-change.cmd ATTRIB -H -S -R move-if-change.cmd
ECHO @IF NOT EXIST %%2 GOTO MOVE >move-if-change.cmd
ECHO @COMP %%1 %%2 ^<no ^>NUL 2^>^&1 >>move-if-change.cmd
ECHO @IF ERRORLEVEL 4 GOTO DELETE >>move-if-change.cmd
ECHO @IF ERRORLEVEL 2 ECHO ERROR COMPARING "%%1" TO "%%2" ^& GOTO END >>move-if-change.cmd
ECHO @IF ERRORLEVEL 1 GOTO DELETE >>move-if-change.cmd
ECHO @ECHO %%2 is unchanged >>move-if-change.cmd
ECHO @rm -f %%1 >>move-if-change.cmd
ECHO @REM GOTO END IMPLICIT >>move-if-change.cmd
ECHO :DELETE >>move-if-change.cmd
ECHO @CALL rm -f %%2 >>move-if-change.cmd
ECHO :MOVE >>move-if-change.cmd
ECHO @MOVE %%1 %%2 >>move-if-change.cmd
ECHO :END >>move-if-change.cmd

REM Create the command file mv.cmd, whose purpose is to emulate the UNIX mv
REM command.
IF EXIST mv.cmd ATTRIB -H -S -R mv.cmd
ECHO @ECHO OFF >mv.cmd
ECHO IF EXIST %%2 DEL %%2 ^<no >>mv.cmd
ECHO MOVE %%1 %%2 >>mv.cmd

REM Create a file called no containing a number of n's, to be used as input to
REM the OS/2 COMP command.
IF EXIST no ATTRIB -H -S -R no & DEL no
FOR %%i IN (0 1 2 3 4 5 6 7 8 9) DO ECHO n>>no

REM Create the command file rm.cmd, whose purpose is to emulate the UNIX rm
REM command.
IF EXIST rm.cmd ATTRIB -H -S -R rm.cmd
ECHO @IF %%1.==-f. (IF EXIST %%2 ATTRIB -H -S -R %%2) ^& DEL %%2 ^<no ^& GOTO END >rm.cmd
ECHO @IF %%1.==-r. (IF EXIST %%2\* DEL %%2 ^<yes) ^& RD %%2 ^& GOTO END >>rm.cmd
ECHO @IF %%1.==-fr. (IF EXIST %%2\* ATTRIB -H -S -R %%2\* ^& DEL %%2 ^<yes) ^& RD %%2 ^& GOTO END >>rm.cmd
ECHO @IF %%1.==-rf. (IF EXIST %%2\* ATTRIB -H -S -R %%2\* ^& DEL %%2 ^<yes) ^& RD %%2 ^& GOTO END >>rm.cmd
ECHO @DEL %%1 ^<no >>rm.cmd
ECHO :END >>rm.cmd

REM Create the command file touch.cmd, whose purpose is to emulate the UNIX
REM touch command.
IF EXIST touch.cmd ATTRIB -H -S -R touch.cmd
ECHO @IF EXIST %%1 COPY %%1 /B + ,, >touch.cmd
ECHO @IF NOT EXIST %%1 ECHO. ^>%%1 >>touch.cmd

REM Create a file called yes containing a number of y's, to be used as input to
REM the OS/2 COMP command.
IF EXIST yes ATTRIB -H -S -R yes & DEL yes
FOR %%i IN (0 1 2 3 4 5 6 7 8 9) DO ECHO y>>yes
