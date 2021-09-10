# NAME
man-pages - conventions for writing man pages.

# DESCRIPTION
This page describes the conventions that should be employed when writing man pages for the Paren man-pages project.

It mimics the Linux man-pages project to make it more familiar to Linux users.

In the Paren man-pages project, the manual is written in markdown.

## Sections of the manual pages
The manual Sections are traditionally defined as follows:

1. User commands
1. System calls
1. Library calls
1. Special files
1. File formats and configuration files
1. Games
1. Overview, conventions, and miscellaneous

### User commands
Commands that can be executed by the user from within a shell.

### System calls
Functions which wrap operations performed by the kernel.

### Library calls
All library functions excluding the system call wrappers.

### Special files
Unused.

### File formats and configuration files
Describes various human-readable file formats and configuration files.

### Games
Games and funny little programs available on the system.

### Overview, conventions, and miscellaneous
Overviews or descriptions of various topics, conventions, and protocols, character set standards, the standard filesystem layout, and miscellaneous other things.

## Sections within a manual page
The list below shows conventional or suggested sections.

Arrange a new manual page so that sections are placed in the order shown in the list.

1. NAME
1. SYNOPSIS
1. CONFIGURATION
1. DESCRIPTION
1. OPTIONS
1. RETURN VALUE
1. ERRORS
1. ENVIRONMENT
1. FILES
1. NOTES
1. BUGS
1. EXAMPLES
1. SEE ALSO

Where a traditional heading would apply, please use it; this kind of consistency can make the information easier to understand.

If you must, you can create your own headings if they make things easier to understand (this can be especially useful for pages in Sections 4 and 5).

The following list elaborates on the contents of each of the above sections.

### NAME
The name of this manual page.

The only mandatory heading is NAME, which should be the first section and be followed on the next line by a one-line description of the program.

All word in this line (including the word immediately following the `-`) should be in lowercase, except where English or technical terminological convention dictates otherwise.

### SYNOPSIS
A brief summary of the command or function's interface.

For commands, this shows the syntax of the command and its arguments (including options); uppercase is used for as-is text.

Brackets `[]` surround optional arguments, vertical bars `|` separate choices, and ellipses `...` can be repeated.

### CONFIGURATION
Configuration details for a device.

This section normally appears only in Section 4 pages.

### DESCRIPTION
An explanation of what the program, function, or format does.

Discuss how it interacts with files and standard input, and what it produces on standard output or standard error.

Omit internals and implementation details unless they're critical for understanding the interface.

Describe the usual case; for information on command-line options of a program use the OPTIONS section.

### OPTIONS
A description of the command-line options accepted by a program and how they change its behavior.

This section should appear only for Section 1 and 8 manual pages.

### RETURN VALUE
For Section 2 and 3 pages, this section gives a list of the values the library routine will return to the caller and the conditions that cause these values to be returned.

### ERRORS
For Section 2 and 3 manual pages, this is a list of the values that may be placed in errno in the event of an error, along with information about the cause of the errors.

### ENVIRONMENT
A list of all environment variables that affect the program or function and how they affect it.

### FILES
A list of the files the program or function uses, such as configuration files, startup files, and files the program directly operates on.

### NOTES
Miscellaneous notes.

### BUGS
A list of limitations, known defects or inconveniences, and other questionable activities.

### EXAMPLES
One or more examples demonstrating how this function, file, or command is used.

For details on writing example programs, see Example programs below.

### SEE ALSO
A list of related man pages, possibly followed by other related pages or documents.

# SEE ALSO
- md2html(1)
