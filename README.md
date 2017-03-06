
[ORIGINALLY SOURCED FROM HERB'S STUFF](http://www.retrotechnology.com/herbs_stuff/float.html)


#Lawrence Livermore Floating point packages for the 8008 and 8080

Following two years of use, in **1975**, Lawrence Livermore (National) Labs released an 8008 and 8080 floating point package, based on 8008 code they had obtained in **1973**. Versions became available in the CP/M community and in Dr Dobb's Journal and in an IEEE publication; but they were incomplete. These programs as corrected are now available on [Herb's site](http://www.retrotechnology.com/herbs_stuff/float.html). He also added discussion and information about another 8080 floating point package from the late 1970's, and discussion of early floating point chips, now often called *math coprocessors*, such as the AMD 9511A.

# Lawrence Livermore (National) Labs Floating Point

**1975**: Here is the [PDF of the original LLL document](http://www.llnl.gov/tid/lof/documents/pdf/171286.pdf):

>   UCRL-51940, [Floating-Point Package for Intel 8008 and 8080 Microprocessors](http://www.llnl.gov/tid/lof/documents/pdf/171286.pdf) by Michael D. Maples, Lawrence Livermore Laboratory, University of California/Livermore, California 94550, October 24, 1975

Other LLL publications about LLL BASIC are:

>[User's Guide to LLL BASIC](http://www.osti.gov/scitech/biblio/7342209), April 1976 and [Users Guide to the LLL Basic Interpreter](http://www.osti.gov/scitech/biblio/7303688/), June 1977.

These are freely available (US Govt publications are not copyrighted) and are online as of Nov 2014. The June version is very similar to the April version.

In **1977**, a version was published in Dr. Dobb's Journal in Jan. 1977. There was a brief article in IEEE Computer magazine in Sept. The reference at the IEEE Web site is: *Real-Time Microcomputer Applications Using LLL Basic,* Computer , vol.10, no.9, pp.14,21, Sept. 1977. IEEE charges for copies of its articles and actively prohibits online distribution. The article lists three programs, one in assembly language; better examples are available in the user's guides cited above.

(Thanks to Neil McNeight for the osti, and IEEE references.)

**1980's**: A version of the code was offered on CPMUG disks #2 and disk #10; as part of LLL *floating point BASIC*. Files and folders with the contents of those disks, can be obtained from the retroarchive Web site as CPMUG010.ARK and CPMUG002.ARK. They should also be on archives or images of the Walnut Creek CP/M CD-ROM, found on many Web CP/M archive sites. Of course *010* is code from disk #10, and *002* from disk #2. These archives DO NOT INCLUDE THE SQUARE ROOT ROUTINE which is in the original document and PDF. The code submitted for disk #2 includes simple CP/M I/O code; for disk #10 that additional code includes CP/M file system support. There's few changes to the LLL code.

**Sept 2006**: Emmanuel Roche, AKA *French Luser*, led a discussion in com.os.cpm (an old Usenet CP/M email discussion group) about 8080 floating point code. Roche posted part of the LLL document text but not the code of the document. Later, Roche privately provided his version of the source code to Herb, with square root code included, and more commentary.

For some time, Herb's Web page carried multiple versions of the source and discussion about differences. In **2015**, Herb simply took the CPMUG #2 code and produced a version which 1) is verified against the LLL documented version and 2) includes the simple CP/M code from disk #2 as cited above. Herb thanks Bill Beech, as they took turns editing and assembling the source, and comparing the octal produced against the octal-based listing in the LLL document.

[Here's the 8080 ASM source for the Lawrence Livermore 1975 floating point package](http://www.retrotechnology.com/herbs_stuff/lll_float_8080_clean.asm). Additional code added in the CPMUG disk #2 is noted. [Here's an octal-based 8080 listing of that code](http://www.retrotechnology.com/herbs_stuff/lll_float_8080_beech.lst), which can be compared to the octal listing. The assembly is courtesy of Bill Beech and his 8080 cross assembler.

Note the title of the LLL document is *8008 and 8080....*. The 8080 code they provided is code compatible with Intel's 8008 mnemonics as revised by them around 1975. Here's an [8008 assembly listing](http://www.retrotechnology.com/herbs_stuff/lll_float_8008_beech.lst) of almost the same LLL code. The differences are that decimal constants in the 8080 source were appended with *D*, so that the assembler would not assume they were octal. Assembly errors in this listing are due to the added CP/M code. As of this date, the resulting 8008 code has not been tested on an 8008 simulator or physical computer. Thanks again to Bill Beech for some of this work and his commentary.

# This Project for RC2014 & YAZ180

**2017**: Building this code to work on the [RC2014](http://rc2014.co.uk) and on the [YAZ180](https://feilipu.me/2016/05/23/another-z80-project/) platforms. Mainly to enable me to test against the [Am9511A-1 APU](https://feilipu.me/2017/02/22/characterising-am9511a-1-apu/) device.

To enable the code to be integrated into Z80 targets, sadly, the library has been translated into Z80 mnemonics.
The tool used was the [z88dk 8080->Z80 awk tool](https://github.com/z88dk/z88dk/tree/master/support/8080). The translation was [almost perfect](https://github.com/feilipu/LLL-Floating-Point/commit/b30d59f84afe3667187d0e9c5634b93ae3c00ed0).

