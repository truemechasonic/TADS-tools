About scads2:
 The scads2 decompiler for TADS2 games was originally created by Ben Rudiak-Gould.
 This update by "truemechasonic" works on more games, and compiles on current software.
 Ben's release was originally hosted at http://www.darkweb.com/~benrg/if-decompilers/
 Ben's release can still be found through use of the Internet Archive (Wayback Machine).
 Ben's release was last updated on January 31st, 2004.
 The scads2 decompiler supposedly used the GPL license, but...
 Ben did not include the actual GPL license, which is required for its use.

Running scads2:
 To decompile a TADS2 game, open up a command prompt in this directory, and run:
	scads2.exe storyfile.gam
 (Replace "storyfile.gam" with the path and filename for the game file.)
 If you want to write the decompiled text to a file, instead run:
	scads2.exe storyfile.gam > output.txt
 (Replace "output.txt" with whatever.)
 If you are not using Windows, you will have to compile scads2 before use.

Compiling scads2:
 Compiling scads2 requires that you have GHC (Glasgow Haskell Compiler) installed.
 To compile, simply run make inside this directory.
