# Name
man -- paren reference manuals.

# Synopsis

    man [SECTION] PAGE

# Description
Display the specified PAGE manual.

It generally consists of the following sections:

- Name
- Synopsis
- Description
- Options
- Examples
- Environment
- See Also

Manuals are generated from system module files or mm files.

Sections within a manual page
       The list below shows conventional or suggested sections.  Most
       manual pages should include at least the highlighted sections.
       Arrange a new manual page so that sections are placed in the
       order shown in the list.

              NAME
              SYNOPSIS
              CONFIGURATION    [Normally only in Section 4]
              DESCRIPTION
              OPTIONS          [Normally only in Sections 1, 8]
              EXIT STATUS      [Normally only in Sections 1, 8]
              RETURN VALUE     [Normally only in Sections 2, 3]
              ERRORS           [Typically only in Sections 2, 3]
              ENVIRONMENT
              FILES
              VERSIONS         [Normally only in Sections 2, 3]
              ATTRIBUTES       [Normally only in Sections 2, 3]
              CONFORMING TO
              NOTES
              BUGS

              EXAMPLES
              AUTHORS          [Discouraged]
              REPORTING BUGS   [Not used in man-pages]
              COPYRIGHT        [Not used in man-pages]
              SEE ALSO

       Where a traditional heading would apply, please use it; this kind
       of consistency can make the information easier to understand.  If
       you must, you can create your own headings if they make things
       easier to understand (this can be especially useful for pages in
       Sections 4 and 5).  However, before doing this, consider whether
       you could use the traditional headings, with some subsections
       (.SS) within those sections.

       The following list elaborates on the contents of each of the
       above sections.

       NAME   The name of this manual page.

              See man(7) for important details of the line(s) that
              should follow the .SH NAME command.  All words in this
              line (including the word immediately following the "\-")
              should be in lowercase, except where English or technical
              terminological convention dictates otherwise.

       SYNOPSIS
              A brief summary of the command or function's interface.

              For commands, this shows the syntax of the command and its
              arguments (including options); boldface is used for as-is
              text and italics are used to indicate replaceable
              arguments.  Brackets ([]) surround optional arguments,
              vertical bars (|) separate choices, and ellipses (...) can
              be repeated.  For functions, it shows any required data
              declarations or #include directives, followed by the
              function declaration.

              Where a feature test macro must be defined in order to
              obtain the declaration of a function (or a variable) from
              a header file, then the SYNOPSIS should indicate this, as
              described in feature_test_macros(7).

       CONFIGURATION
              Configuration details for a device.

              This section normally appears only in Section 4 pages.

       DESCRIPTION
              An explanation of what the program, function, or format
              does.

              Discuss how it interacts with files and standard input,
              and what it produces on standard output or standard error.
              Omit internals and implementation details unless they're
              critical for understanding the interface.  Describe the
              usual case; for information on command-line options of a
              program use the OPTIONS section.

              When describing new behavior or new flags for a system
              call or library function, be careful to note the kernel or
              C library version that introduced the change.  The
              preferred method of noting this information for flags is
              as part of a .TP list, in the following form (here, for a
              new system call flag):

                       XYZ_FLAG (since Linux 3.7)
                              Description of flag...

              Including version information is especially useful to
              users who are constrained to using older kernel or C
              library versions (which is typical in embedded systems,
              for example).

       OPTIONS
              A description of the command-line options accepted by a
              program and how they change its behavior.

              This section should appear only for Section 1 and 8 manual
              pages.

       EXIT STATUS
              A list of the possible exit status values of a program and
              the conditions that cause these values to be returned.

              This section should appear only for Section 1 and 8 manual
              pages.

       RETURN VALUE
              For Section 2 and 3 pages, this section gives a list of
              the values the library routine will return to the caller
              and the conditions that cause these values to be returned.

       ERRORS For Section 2 and 3 manual pages, this is a list of the
              values that may be placed in errno in the event of an
              error, along with information about the cause of the
              errors.

              Where several different conditions produce the same error,
              the preferred approach is to create separate list entries
              (with duplicate error names) for each of the conditions.
              This makes the separate conditions clear, may make the
              list easier to read, and allows metainformation (e.g.,
              kernel version number where the condition first became
              applicable) to be more easily marked for each condition.

              The error list should be in alphabetical order.

       ENVIRONMENT
              A list of all environment variables that affect the
              program or function and how they affect it.

       FILES  A list of the files the program or function uses, such as
              configuration files, startup files, and files the program
              directly operates on.

              Give the full pathname of these files, and use the
              installation process to modify the directory part to match
              user preferences.  For many programs, the default
              installation location is in /usr/local, so your base
              manual page should use /usr/local as the base.

       ATTRIBUTES
              A summary of various attributes of the function(s)
              documented on this page.  See attributes(7) for further
              details.

       VERSIONS
              A brief summary of the Linux kernel or glibc versions
              where a system call or library function appeared, or
              changed significantly in its operation.

              As a general rule, every new interface should include a
              VERSIONS section in its manual page.  Unfortunately, many
              existing manual pages don't include this information
              (since there was no policy to do so when they were
              written).  Patches to remedy this are welcome, but, from
              the perspective of programmers writing new code, this
              information probably matters only in the case of kernel
              interfaces that have been added in Linux 2.4 or later
              (i.e., changes since kernel 2.2), and library functions
              that have been added to glibc since version 2.1 (i.e.,
              changes since glibc 2.0).

              The syscalls(2) manual page also provides information
              about kernel versions in which various system calls first
              appeared.

       CONFORMING TO
              A description of any standards or conventions that relate
              to the function or command described by the manual page.

              The preferred terms to use for the various standards are
              listed as headings in standards(7).

              For a page in Section 2 or 3, this section should note the
              POSIX.1 version(s) that the call conforms to, and also
              whether the call is specified in C99.  (Don't worry too
              much about other standards like SUS, SUSv2, and XPG, or
              the SVr4 and 4.xBSD implementation standards, unless the
              call was specified in those standards, but isn't in the
              current version of POSIX.1.)

              If the call is not governed by any standards but commonly
              exists on other systems, note them.  If the call is Linux-
              specific, note this.

              If this section consists of just a list of standards
              (which it commonly does), terminate the list with a period
              ('.').

       NOTES  Miscellaneous notes.

              For Section 2 and 3 man pages you may find it useful to
              include subsections (SS) named Linux Notes and Glibc
              Notes.

              In Section 2, use the heading C library/kernel differences
              to mark off notes that describe the differences (if any)
              between the C library wrapper function for a system call
              and the raw system call interface provided by the kernel.

       BUGS   A list of limitations, known defects or inconveniences,
              and other questionable activities.

       EXAMPLES
              One or more examples demonstrating how this function,
              file, or command is used.

              For details on writing example programs, see Example
              programs below.

       AUTHORS
              A list of authors of the documentation or program.

              Use of an AUTHORS section is strongly discouraged.
              Generally, it is better not to clutter every page with a
              list of (over time potentially numerous) authors; if you
              write or significantly amend a page, add a copyright
              notice as a comment in the source file.  If you are the
              author of a device driver and want to include an address
              for reporting bugs, place this under the BUGS section.

       REPORTING BUGS
              The man-pages project doesn't use a REPORTING BUGS section
              in manual pages.  Information on reporting bugs is instead
              supplied in the script-generated COLOPHON section.
              However, various projects do use a REPORTING BUGS section.
              It is recommended to place it near the foot of the page.

       COPYRIGHT
              The man-pages project doesn't use a COPYRIGHT section in
              manual pages.  Copyright information is instead maintained
              in the page source.  In pages where this section is
              present, it is recommended to place it near the foot of
              the page, just above SEE ALSO.

       SEE ALSO
              A comma-separated list of related man pages, possibly
              followed by other related pages or documents.

              The list should be ordered by section number and then
              alphabetically by name.  Do not terminate this list with a
              period.

              Where the SEE ALSO list contains many long manual page
              names, to improve the visual result of the output, it may
              be useful to employ the .ad l (don't right justify) and
              .nh (don't hyphenate) directives.  Hyphenation of
              individual page names can be prevented by preceding words
              with the string "\%".

              Given the distributed, autonomous nature of FOSS projects
              and their documentation, it is sometimes necessary—and in
              many cases desirable—that the SEE ALSO section includes
              references to manual pages provided by other projects.

# Options
    -d -- Decode standard input.

# See Also
