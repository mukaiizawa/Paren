# NAME
getenv, putenv - get and change/add an environment variable.

# SYNOPSIS

    (getenv NAME)
    (putenv NAME VALUE)

# DESCRIPTION
These functions get and set values from environment variables.

If `NAME` does not exist in the environment, `NAME-VALUE` is added to the environment.

If `NAME` exists in the environment, the value of `NAME` is changed to `value`.

# RETURN VALUE
The function `getenv` returns the value in the environment, or the `nil` if there is no match.

The function `putenv` returns the `nil`.

# EXAMPLES

    ) (getenv "JAVA_HOME")
    "C:\\Program Files\\Amazon Corretto\\jdk11.0.12_7"
    ) (putenv "JAVA_HOME" "C:\\Program Files\\Amazon Corretto\\jdk1.8.0_292")
    nil
    ) (getenv "JAVA_HOME")
    "C:\\Program Files\\Amazon Corretto\\jdk1.8.0_292"

# SEE ALSO
- `system(3)`
