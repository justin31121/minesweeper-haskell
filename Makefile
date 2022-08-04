build:
	ghc -package vector -i.\src\hs -o bin\mine.exe src\hs\Main.hs src\c\ansi.c
run:
	bin\mine.exe
