SKS Keyserver
=============

The following is an incomplete guide to compiling, setting up and using SKS.
Hopefully this is enough to get you started, in addition there is a wiki available, 
where in particular <https://bitbucket.org/skskeyserver/sks-keyserver/wiki/Peering> 
should help getting a working installation. 

Prerequisites
-------------

There are a few prerequisites to building this code.  You need:

* OCaml-3.11.0 or later.  Get it from <http://ocaml.org>
* Berkeley DB version 4.6.* or later.  You can find the
  appropriate versions at
  <http://www.oracle.com/technetwork/database/berkeleydb/downloads/index.html>
* GNU Make and a C compiler (e.g gcc) 

  Verifying the integrity of the download
----------------------------
Releases of SKS are signed using the SKS Keyserver Signing Key
available on public keyservers with the KeyID

    0x41259773973A612A
	
and has a fingerprint of

    C90E F143 0B3A C0DF D00E 6EA5 4125 9773 973A 612A.
	
Using GnuPG, verification can be accomplished by, first, retrieving the signing key using

    gpg --keyserver pool.sks-keyservers.net --recv-key 0x41259773973A612A
	
followed by verifying that you have the correct key

    gpg --keyid-format long --fingerprint 0x41259773973A612A

should produce:

    pub   4096R/41259773973A612A 2012-06-27
    Key fingerprint = C90E F143 0B3A C0DF D00E 6EA5 4125 9773 973A 612A
		
A check should also be made that the key is signed by
trustworthy other keys;

    gpg --list-sigs 0x41259773973A612A

and the fingerprint should be verified through other trustworthy sources.
			
Once you are certain that you have the correct key downloaded, you can create
a local signature, in order to remember that you have verified the key.

     gpg --lsign-key 0x41259773973A612A

Finally; verifying the downloaded file can be done using

    gpg --keyid-format long --verify sks-x.y.z.tgz.asc

The resulting output should be similar to
	
    gpg: Signature made Wed Jun 27 12:52:39 2012 CEST
    gpg:                using RSA key 41259773973A612A
    gpg: Good signature from "SKS Keyserver Signing Key"

  Compilation and Installation
----------------------------

  * Install OCaml and Berkeley DB

    When installing ocaml, make sure you do both the `make world` and
    the `make opt` steps before installing.  The later makes sure you
    get the optimizing compilers.  (do make opt.opt if you want faster
    compilation.  You can then set the environment variables `OCAMLC`,
    `OCAMLOPT` and `CALMP4O` to `ocamlc.opt`, `ocamlopt.opt` and
    `camlp4o.opt` respectively.)

    If your vendor or porting project supplies prebuilt binaries and
    libraries for Berkeley DB, make sure to get the development
    package as you will need the correct version include files.

  * Copy `Makefile.local.unused` to `Makefile.local`, and edit to
    match your installation.

  * Compile

        make dep
        make all
        make all.bc # if you want the bytecode versions
        make install # puts executables in $PREFIX/bin, as defined
                     # in Makefile.local

    There are some other useful compilation targets, mostly useful for
    development.

      - `make doc`

        creates a doc directory with ocamldoc-generated documentation
        of the individual modules.  These are mostly useful as
        documentation to the source code, not a user's guide.

      - `make modules.ps`

        Creates a ps-file that shows the dependencies between
        different modules, and gives you a sense of the overall
        structure of the system.  For this to work you need to have
        AT&T's graphviz installed, as well as python2.  The python
        script that's used actually requires that python2 be called
        python2, rather than python.  You can of course edit that
        script.

Setup and Configuration
-----------------------

You need to set up a directory for the SKS installation.  It will
contain the database files along with configuration and log files.

Configuration options can be passed in on the command-line or put in
the `sksconf` file in the SKS directory.  the `-basedir` option
specifies the SKS directory itself, which defaults to the current
working directory.

### Sksconf and commandline options

The format of the sksconf file is simply a bunch of lines of the
form:

    keyword: value

The `#` character is used for comments, and blank lines are
ignored.  The keywords are just the command-line flags, minus the
initial `-`.

The one thing you probably want no matter what is a line that says

    logfile: log

which ensures that sks will output messages to `recon.log` and
`db.log` respectively.

### Membership file

If you want your server to gossip with others, you will need a
membership file which tells the `sks recon` who else to gossip with.
The membership file should look something like:

    epidemic.cs.cornell.edu 11370
    athos.rutgers.edu 11370
    ...

This file should be called `membership`, and should be stored in the
SKS directory.  Note that in order for synchronization to work, both
hosts have to have each other in their membership lists.  Send mail to
<sks-devel@nongnu.org> to get other SKS administrators to add you to
their membership lists.

**IMPORTANT NOTE**: if you include the server itself in the membership
file, you should make sure that you also specify the `hostname`
option, and that the selected hostname is exactly the same string
listed in the membership file.  Otherwise, the `sks recon` will try to
synchronize with itself and will deadlock.

### Outgoing PKS synchronization: mailsync file

The mailsync file contains a list of email addresses of PKS
keyservers.  This file is important, because it ensures that keys
submitted directly to an SKS keyserver are also forwarded to PKS
keyservers.

**IMPORTANT**: don't add someone to your mailsync file without getting
their permission first!

In order for outgoing email sync's to work, you need to specify a
command to actually send the email out.  The default is `sendmail -t
-oi`, but you may need something different.

### Incoming PKS synchronization

Incoming PKS synchronization is less critical than outgoing,
since as long as some SKS server gets the new data, it will be
distributed to all.  Having more hosts receive the incoming PKS
syncs does, however, increase the fault-tolerance of the
connection between the two systems.

In order to get incoming mail working, you should pipe the appropriate
incoming mail to the following command via procmail:

    sks_add_mail sks_directory_name

Here's an example procmail entry:

    PATH=/path/of/sks/exectuables

    :0
    * ^Subject: incremental
    | sks_add_mail sks_directory_name


### Built-in webserver

You can server up a simple index page directly from the port
you're using for HKP.  This is done by creating a subdirectory in
your SKS directory called `web`.  There, you can put an index file
named `index.html`, `index.htm`, `index.xhtm`, or `index.xhtml`,
supporting files with extensions .css, .es, or .js, and some image
files with extensions jpg, jpeg, png or gif. Subdirectories will
be ignored, as will filenames with anything other than
alphanumeric characters and the '.'  character.  This is
particularly useful if you want to run your webserver off of port
80.  This can be done by using the -hkp_port command-line option.


Building up the databases
-------------------------

  - First, you need to get a keydump.  If you're running a PKS server,
    you should be able to convince PKS to generate one for you.  If
    you're starting from scratch, you'll need to download one from the
    net.  You should contact the pgp keyserver list
    <pgp-keyserver-folk@flame.org>

  - in the SKS directory, put in a subdirectory called `dump` which
    contains the keydump files from which the database is to be built.

  - Run sks_build.sh.  That script actually runs three utilities.  You
    might want to edit sks_build.sh if you want to trade off speed for
    space usage.  At the current settings, you could run out of ram if
    you try this with less then 256 megs of RAM.

**DO NOT DELETE THE `dump` DIRECTORY**, even after the database is
built.  The original keys are not copied to the database, and so the
dump must be left in place.

Platform specific issues
------------------------

### FreeBSD ###

On FreeBSD it appears that libdb is named differently than on some
other platforms.  For that reason, you need to set the LIBDB
environment value to `-ldb46` instead of `-ldb-4.6` for other
platfomrs.
