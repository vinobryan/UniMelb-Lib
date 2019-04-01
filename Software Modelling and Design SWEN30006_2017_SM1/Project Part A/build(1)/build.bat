@echo off

set "JAVA_HOME=C:\Program Files\Java\jdk1.8.0_91"

set zip_path="C:\Program Files\7-Zip\7z"

%zip_path% x -y "Part A.zip" | find /I "Files:" > src_check.tmp

set /p src_file_count=<src_check.tmp

%zip_path% e -y "submission.zip" -osubmission *.java -r | find /I "Files:" > submission_check.tmp

set /p submission_file_count=<submission_check.tmp

del *.tmp

IF NOT EXIST swen30006 (
	ECHO ERROR: MISSING FILE: Part A.zip!
	exit /b
)

IF NoT EXIST submission (
	ECHO ERROR: MISSING FILE: submission.zip!
	exit /b
)


IF "%src_file_count%" == "Files: 91" (
	ECHO Correct number of files in Part A.zip!
) ELSE (
	ECHO ERROR: INCORRECT NUMBER OF JAVA FILES FOR PART A [Please re-download Part A.zip from the LMS!]
	exit /b
)

IF "%submission_file_count%" == "Files: 3" (
	ECHO Correct number of files submitted!
) ELSE (
	ECHO ERROR: INCORRECT NUMBER OF JAVA FILES SUBMITTED [We expect EXACTLY 3 Java files!]
	exit /b
)

set src_file_count=
set submission_file_count=

echo Copying your files to build...

del /q swen30006\strategies\*
xcopy /s /y submission\Automail.java swen30006\automail\
if errorlevel 1 (
	echo Automail.java not found!
	exit /b
)
del submission\Automail.java
xcopy /s /y submission swen30006\strategies
rmdir /s /q submission

echo Starting Build...

SET ant_path=apache-ant\bin\ant
%ant_path% && %ant_path% clean && java -cp simulation.jar automail.Simulation 30006 && del simulation.jar	



