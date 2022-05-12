About scads3:
 The scads3 decompiler for TADS3 games was originally created by Ben Rudiak-Gould.
 This update by "truemechasonic" works on more games, and compiles on current software.
 Ben's release was originally hosted at http://www.darkweb.com/~benrg/if-decompilers/
 Ben's release can still be found through use of the Internet Archive (Wayback Machine).
 Ben's release was last updated on January 31st, 2004.
 The scads3 decompiler supposedly used the GPL license, but...
 Ben did not include the actual GPL license, which is required for its use.

Running scads3:
 To decompile a TADS3 game, open up a command prompt in this directory, and run:
	scads3.exe storyfile.gam
 (Replace "storyfile.gam" with the path and filename for the game file.)
 If you want to write the decompiled text to a file, instead run:
	scads3.exe storyfile.gam > output.txt
 (Replace "output.txt" with whatever.)
 If you are not using Windows, you will have to compile scads3 before use.

Compiling scads3:
 Compiling scads3 requires that you have GHC (Glasgow Haskell Compiler) installed.
 To compile, simply run make inside this directory.
